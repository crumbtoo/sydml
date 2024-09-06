{-# LANGUAGE ViewPatterns #-}
module Presydc.SSA where
--------------------------------------------------------------------------------
import Language.QBE        qualified as QBE
import Data.HashMap.Strict qualified as H
import Data.List.NonEmpty qualified as List1
import Effectful
import Presydc.ANF
import Presydc.Lam.Syntax (Name, PrimOp (..), Program (..))
import Presydc.Lam.Syntax qualified as Lam
import SydPrelude
import Data.Text.Short qualified as TS
import Data.Text.IO qualified as T
import Data.Foldable
import Data.Monoid
import Data.Maybe
import Control.Lens
import System.IO
import Data.Text.Prettyprint.Doc.Render.Text
import Language.QBE (Val(ValTemporary))
--------------------------------------------------------------------------------
-- Lowering to SSA/QBE

type Spills = H.HashMap Name Name

preprocess :: forall es. (Unique :> es)
           => List1 Join -> Eff es Spills
preprocess = foldlM go mempty
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

lowerBlock :: forall es. (Unique :> es) => Spills -> Join -> Eff es QBE.Block
lowerBlock spills (Join (name2id -> j0) p m) = allocSpills <$> go m
  where
    allocSpills :: QBE.Block -> QBE.Block
    allocSpills = blockInsts <>:~ foldMap (\nm -> []) spills

    go :: Term -> Eff es QBE.Block

    go (Val v) = pure $ QBE.Block j0 [] [] (QBE.Ret . Just . lowerValue $ v)

    go (Jump j (Just p)) = pure $ QBE.Block j0 [] insts (QBE.Jmp (name2id j))
      where
        insts = case spills ^. at j of
          Just slot -> [ QBE.Store (QBE.BaseTy QBE.Word)
                            (lowerValue p)
                            (QBE.ValTemporary (name2id slot))
                       ]
          _ -> error $ "join points must have spill slots!"

    go (IfThenElse c (Jump t Nothing) (Jump f Nothing)) =
        pure $ QBE.Block j0 [] [] (QBE.Jnz c' (name2id t) (name2id f))
      where c' = lowerValue c

lowerLam :: (Unique :> es) => Lam -> Eff es (List1 QBE.Block)
lowerLam (Lam f xs j js) = do
  spills <- preprocess (j :| js)
  let low = lowerBlock spills
  (:|) <$> low j <*> (low `traverse` js)

qbeTyWord = QBE.AbiBaseTy $ QBE.Word

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
      abiTy = Just qbeTyWord
      env = Nothing
      params = List1.toList $
        QBE.Param qbeTyWord . name2id <$> l ^. lamParams

getMain :: List1 Lam -> Lam
getMain = fromJust . getFirst . foldMap \case
  p@(Lam "main" _ _ _) -> First (Just p)
  _                    -> First Nothing

pipeline :: Lam.Term -> _
pipeline = QBE.Program [] []
         . List1.toList
         . runPureEff
         . runUnique
         . ( lower
           <=< hoist
           <=< convert
           <=< anfTerm
           <=< rename )

writePipeline :: FilePath -> Lam.Term -> IO ()
writePipeline fp e = withFile fp WriteMode \h ->
  hPutDoc h . pretty . pipeline $ e

--------------------------------------------------------------------------------
-- Run pipeline up to X

upToHoist :: Lam.Term -> List1 Lam
upToHoist = runPureEff . runUnique . (hoist <=< convert <=< anfTerm <=< rename)
