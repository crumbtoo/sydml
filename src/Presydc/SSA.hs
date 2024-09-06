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

nameToIdent :: forall t. Name -> QBE.Ident t
nameToIdent = QBE.Ident . TS.fromText

lowerValue :: Value -> QBE.Val
lowerValue (IntVal n) = QBE.ValConst (QBE.CInt s n')
  where
    s  = signum n == (-1)
    n' = fromIntegral . abs $ n
lowerValue (Var x)    = QBE.ValTemporary (nameToIdent x)
lowerValue (Global x) = QBE.ValGlobal (nameToIdent x)

lowerBlock :: forall es. (Unique :> es) => Spills -> Join -> Eff es QBE.Block
lowerBlock spills (Join j p m) = go m
  where
    j' = nameToIdent j

    go :: Term -> Eff es QBE.Block
    go (Val v) = pure $ QBE.Block j' [] [] (QBE.Ret . Just . lowerValue $ v)

lowerLam (Lam f xs j js) = do
  spills <- preprocess (j :| js)
  let low = lowerBlock spills
  (:|) <$> low j <*> (low `traverse` js)

qbeTyWord = QBE.AbiBaseTy $ QBE.Word

lower :: (Unique :> es) => List1 Lam -> Eff es (List1 QBE.FuncDef)
lower = traverse go where
  go l = QBE.FuncDef [linkage]
                     abiTy
                     (nameToIdent name)
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
        QBE.Param qbeTyWord . nameToIdent <$> l ^. lamParams

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
