{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
module SSA where
--------------------------------------------------------------------------------
import Data.Aeson qualified as Aeson
import Data.Aeson ((.=))
import Language.QBE        qualified as QBE
import Data.HashMap.Strict qualified as H
import Data.List.NonEmpty qualified as List1
import           Effectful
import           ANF
import           Lam.Syntax (Name, PrimOp (..), Program (..))
import Lam.Syntax qualified as Lam
import           SydPrelude
import Data.Text.Short qualified as TS
import Data.Text.IO qualified as T
import           Data.Foldable
import           Data.Monoid
import           Data.Maybe
import           Control.Lens hiding ((.=))
import           System.IO
import           Prettyprinter.Render.Text
import           Language.QBE (pattern (:=))
import           Effectful.Writer.Static.Local
import qualified Data.Text as T
import           Data.Text.Prettyprint.Doc
import qualified Data.ByteString.Lazy as BS
--------------------------------------------------------------------------------
-- Lowering to SSA/QBE

type Spills = H.HashMap Name Name

findSpills :: forall es. (Unique :> es)
           => List1 Join -> Eff es Spills
findSpills = foldlM go mempty
  where
    go :: H.HashMap Name Name -> Join -> Eff es Spills
    go spills = \case
      Join j (Just p) _ -> do
        p' <- mkFresh p
        pure $ H.insert j p' spills
      _ -> pure spills

name2id :: forall t. Name -> QBE.Ident t
name2id = QBE.Ident . TS.fromText

lowerValue :: Value -> QBE.Val
lowerValue (IntVal n) = QBE.ValConst (QBE.CInt s n')
  where
    s  = signum n == (-1)
    n' = fromIntegral . abs $ n
lowerValue (Var x)    = QBE.ValTemporary (name2id x)
lowerValue (Global x) = QBE.ValGlobal (name2id x)

blockInsts :: Lens' QBE.Block (List QBE.Inst)
blockInsts sbt (QBE.Block nm phis insts j) =
  (\insts' -> QBE.Block nm phis insts' j) <$> sbt insts

unitype = QBE.Long
unitypeAlignment = QBE.Eight
unitypeSize = 8

lowerBlock :: forall es. (Unique :> es) => Spills -> Join -> Eff es QBE.Block
lowerBlock spills (Join j0 p m) = go m <&> blockInsts <>:~ maybeLoad
  where
    j0' = name2id j0

    maybeLoad = case p of
      Just p' -> [QBE.Load (name2id p' := unitype) unitype (QBE.ValTemporary (name2id slot))]
        where slot = unsafeLookup j0 spills
      Nothing -> []

    go :: Term -> Eff es QBE.Block

    go (Val v) = pure $ QBE.Block j0' [] [] (QBE.Ret . Just . lowerValue $ v)

    go (LetPrim x p m) = do
      let ass = name2id x := unitype
      let mkBinOp op = QBE.BinaryOp ass op `on` lowerValue
      letPrimInsts <-
        case p of
          PrimAdd a b ->
            pure [ mkBinOp QBE.Add a b ]
          PrimMul a b ->
            pure [ mkBinOp QBE.Mul a b ]
          PrimSub a b ->
            pure [ mkBinOp QBE.Sub a b ]
          PrimPrintInt a ->
            pure [ QBE.Call
                     (Just (name2id x, QBE.AbiBaseTy unitype))
                     (QBE.ValGlobal "printf")
                     Nothing
                     [QBE.Arg
                        (QBE.AbiBaseTy unitype)
                        (QBE.ValGlobal primPrintIntFmtLabel)]
                     [QBE.Arg
                        (QBE.AbiBaseTy unitype)
                        (lowerValue a)]
                 ]
      go m <&> blockInsts <>:~ letPrimInsts

    go (LetTuple x vs m) = do
      let nbytes = fromIntegral (length vs) * unitypeSize
      let alloc =
            QBE.Call
              (Just (name2id x, QBE.AbiBaseTy unitype))
              (QBE.ValGlobal "malloc")
              Nothing
              [QBE.Arg
                (QBE.AbiBaseTy QBE.Long)
                (QBE.ValConst (QBE.CInt False nbytes))]
              []
      let populate :: _ -> _ -> Eff es _
          populate ix e = do
            ptr <- mkFresh "ptr"
            pure [ QBE.BinaryOp
                     (name2id ptr := unitype)
                     QBE.Add
                     (QBE.ValTemporary $ name2id x)
                     (QBE.ValConst $ QBE.CInt False
                        (fromIntegral (ix :: Int) * unitypeSize))
                 , QBE.Store (QBE.BaseTy unitype)
                     e
                     (QBE.ValTemporary $ name2id ptr)
                 ]
      insts <- (alloc:) . fold <$> (\i -> populate i . lowerValue) `itraverse` vs
               :: Eff es (List QBE.Inst)
      go m <&> blockInsts <>:~ insts

    go (LetApp r f xs m) = do
      let vs = List1.toList $
                QBE.Arg (QBE.AbiBaseTy unitype) . lowerValue <$> xs
      let insts = [ QBE.Call
                    (Just (name2id r, QBE.AbiBaseTy unitype))
                    (QBE.ValTemporary (name2id f))
                    Nothing
                    vs
                    []
                  ]
      go m <&> blockInsts <>:~ insts

    go (LetProj r i v m) = do
      ptr <- mkFresh "ptr"
      let insts =
            [ QBE.BinaryOp
                (name2id ptr := unitype)
                QBE.Add
                (QBE.ValConst $ QBE.CInt False (fromIntegral i * unitypeSize))
                (QBE.ValTemporary (name2id v))
            , QBE.Load
                (name2id r := unitype)
                unitype
                (QBE.ValTemporary (name2id ptr))
            ]
      go m <&> blockInsts <>:~ insts

    go (Jump j (Just p)) = pure $ QBE.Block j0' [] insts (QBE.Jmp (name2id j))
      where
        insts = case spills ^. at j of
          Just slot -> [ QBE.Store (QBE.BaseTy QBE.Long)
                            (lowerValue p)
                            (QBE.ValTemporary (name2id slot))
                       ]
          _ -> error $ "join points must have spill slots!"

    go (IfThenElse c (Jump t Nothing) (Jump f Nothing)) =
        pure $ QBE.Block j0' [] [] (QBE.Jnz c' (name2id t) (name2id f))
      where c' = lowerValue c

primPrintIntFmtLabel :: QBE.Ident 'QBE.Global
primPrintIntFmtLabel = "int_fmt"

lowerLam :: (Unique :> es) => Lam -> Eff es (List1 QBE.Block)
lowerLam (Lam f xs j js) = do
  spills <- findSpills (j :| js)
  let low = lowerBlock spills
  (:|) <$> (allocSpills spills <$> low j) <*> (low `traverse` js)

allocSpills :: Spills -> QBE.Block -> QBE.Block
allocSpills spills = blockInsts <>:~ foldMap mkAlloc spills
  where mkAlloc nm = [QBE.Alloc (name2id nm := unitype) unitypeAlignment unitypeSize]

lower :: (Unique :> es) => List1 Lam -> Eff es (List1 QBE.FuncDef)
lower = traverse go where
  go l = QBE.FuncDef linkage
                     abiTy
                     (name2id name)
                     env
                     params
                     QBE.NoVariadic
                     <$> lowerLam l
    where
      name = l ^. lamName
      linkage = case name of
            "main" -> [ QBE.Export ]
            _ -> []
      abiTy = Just . QBE.AbiBaseTy $ unitype
      env = Nothing
      params = List1.toList $
        QBE.Param (QBE.AbiBaseTy unitype) . name2id <$> l ^. lamParams

pipeline :: Lam.Term -> _
pipeline = QBE.Program [] [datas]
         . List1.toList
         . runPureEff
         . runUnique
         . ( lower
           <=< hoist
           <=< convert
           <=< anfTerm
           <=< rename )
  where
    datas = QBE.DataDef [] primPrintIntFmtLabel Nothing
      [ QBE.FieldExtTy QBE.Byte $
          List1.singleton (QBE.String "%d\n")
      , QBE.FieldExtTy QBE.Byte $
          List1.singleton (QBE.Const $ QBE.CInt False 0)
      ]

pipeline' :: Lam.Term -> _
pipeline'
  = first (QBE.Program [] [datas] . List1.toList)
  . runPureEff
  . runWriter @(List Aeson.Value)
  . runUnique
  . ( adornDump "lower" lower
     <=< adornDump "hoist" hoist
     <=< adornDump "conver" convert
     <=< adornDump "to anf" anfTerm
     <=< adornDump "rename" rename)
  where
    datas = QBE.DataDef [] primPrintIntFmtLabel Nothing
      [ QBE.FieldExtTy QBE.Byte $
          List1.singleton (QBE.String "%d\n")
      , QBE.FieldExtTy QBE.Byte $
          List1.singleton (QBE.Const $ QBE.CInt False 0)
      ]

makePass :: Text -> List Text -> List Text -> Aeson.Value
makePass name before after = Aeson.object
  [ "name" .= name
  , "machine" .= False
  , "before" .= ((\x -> Aeson.object ["text" .= x]) <$> before)
  , "after" .= ((\x -> Aeson.object ["text" .= x]) <$> after)
  , "irChanged" .= True
  ]

render = renderStrict . layoutPretty defaultLayoutOptions

adornDump :: (Pretty a, Pretty b, Writer (List Aeson.Value) :> es)
          => Text -> (a -> Eff es b) -> a -> Eff es b
adornDump name k a = do
  b <- k a
  let p = makePass name (f a) (f b)
      f x = T.lines . render . pretty $ x -- mono restr
  tell [p] $> b

writePipeline :: FilePath -> Lam.Term -> IO ()
writePipeline fp e = withFile fp WriteMode \h ->
  hPutDoc h . pretty . pipeline $ e

writePipeline' :: FilePath -> Lam.Term -> IO ()
writePipeline' fp e = withFile fp WriteMode \h -> do
  let (qbe,vs) = pipeline' e
  hPutDoc h . pretty $ qbe
  BS.putStr . Aeson.encode $ Aeson.object
    [ "compilation" .= vs ]

--------------------------------------------------------------------------------
-- Run pipeline up to X

upToHoist :: Lam.Term -> List1 Lam
upToHoist = runPureEff . runUnique . (hoist <=< convert <=< anfTerm <=< rename)

upToConvert :: Lam.Term -> Term
upToConvert = runPureEff . runUnique . (convert <=< anfTerm <=< rename)
