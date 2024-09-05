module Presydc.SSA where
--------------------------------------------------------------------------------
import Language.QBE
import Data.HashMap.Strict qualified as H
import Data.List.NonEmpty qualified as List1
import Effectful
import Presydc.ANF
import Presydc.Lam.Syntax (Name, PrimOp (..), Program (..))
import Presydc.Lam.Syntax qualified as Lam
import SydPrelude
import Data.Foldable
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

lowerBlock :: Spills -> Join -> (Join, Block)
lowerBlock = _
