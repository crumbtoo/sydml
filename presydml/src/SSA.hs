{-# LANGUAGE ViewPatterns, PatternSynonyms #-}
{-# LANGUAGE LambdaCase, BlockArguments #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
module SSA where
--------------------------------------------------------------------------------
import Language.QBE        qualified as QBE
import Data.HashMap.Strict qualified as H
import Data.List.NonEmpty qualified as List1
import Effectful
import ANF
import Lam.Syntax (Name, PrimOp (..), Program (..))
import Lam.Syntax qualified as Lam
import SydPrelude
import Data.Text.Short qualified as TS
import Data.Text.IO qualified as T
import Data.Foldable
import Data.Monoid
import Data.Maybe
import Control.Lens
import System.IO
import Prettyprinter.Render.Text
import Language.QBE (pattern (:=))
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
      letPrimInsts <-
        case p of
          PrimAdd a b ->
            pure [ QBE.BinaryOp ass QBE.Add (lowerValue a) (lowerValue b) ]
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
  go l = QBE.FuncDef [linkage]
                     abiTy
                     (name2id name)
                     env
                     params
                     QBE.NoVariadic
                     <$> lowerLam l
    where
      name = l ^. lamName
      linkage = case name of
            "main" -> QBE.Export
            _ -> _
      abiTy = Just . QBE.AbiBaseTy $ unitype
      env = Nothing
      params = List1.toList $
        QBE.Param (QBE.AbiBaseTy unitype) . name2id <$> l ^. lamParams

getMain :: List1 Lam -> Lam
getMain = fromJust . getFirst . foldMap \case
  p@(Lam "main" _ _ _) -> First (Just p)
  _                    -> First Nothing

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

writePipeline :: FilePath -> Lam.Term -> IO ()
writePipeline fp e = withFile fp WriteMode \h ->
  hPutDoc h . pretty . pipeline $ e

--------------------------------------------------------------------------------
-- Run pipeline up to X

upToHoist :: Lam.Term -> List1 Lam
upToHoist = runPureEff . runUnique . (hoist <=< convert <=< anfTerm <=< rename)
