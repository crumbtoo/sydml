module Language.Core.ToANF
  (
  )
  where
--------------------------------------------------------------------------------
import           Control.Comonad.Cofree
import           Effect.Unique
import Language.ANF.Syntax qualified as ANF
import Language.Core.Syntax qualified as Core
import           SydPrelude
import           Sydc.Name (Ident, fresh)
import           Language.Common
import qualified Data.List.NonEmpty as List1
import           Control.Lens hiding ((:<))
import           Data.Located (SrcSpan)
--------------------------------------------------------------------------------

lowerModule
  :: (Unique :> es)
  => Core.Module p -> Eff es (ANF.Module p)
lowerModule = _

-- lowerTermF
--   :: forall p es.
--      ( Unique :> es
--      , Core.Untyped p )
--   => SrcSpan -> Core.TermF p ANF.LocatedTerm -> Eff es ANF.LocatedTerm
-- lowerTermF = _
--   where

type ANFContinuation es = Value -> Eff es ANF.LocatedTerm

lowerTermFWithCont
  :: Unique :> es
  => SrcSpan
  -> Core.TermF p (ANFContinuation es -> Eff es ANF.LocatedTerm)
  -> ANFContinuation es
  -> Eff es ANF.LocatedTerm
-- lowerTermFWithCont ss (Core.CaseF e [t,f]) k =
--   e \e' -> do
--     (j,p) <- each fresh ("j","p")
--     let jn = pure . (ss :<) . ANF.Jump j . (:[])
--     r <- ANF.LetJoin j [p] <$> k (Var (LocalId p)) <*> _
--     pure $ ss :< r
lowerTermFWithCont _ _ _ = _
