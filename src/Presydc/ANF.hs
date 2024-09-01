{-# LANGUAGE TemplateHaskell #-}
module Presydc.ANF
  ( Value(..)
  , Term(..)
  , Unique, fresh, mkFresh, runUnique
  , renameProgram, rename
  , anfTermF, anfTerm, anfProgram
  )
  where
--------------------------------------------------------------------------------
import Data.HashSet qualified as HS
import Data.Monoid
import Control.Lens
import SydPrelude
import Presydc.Lam.Syntax qualified as Lam
import Presydc.Lam.Syntax (Program(..), Name, PrimOp)
import Data.Functor.Foldable.Monadic
import Data.Text qualified as T
import Data.HashMap.Strict qualified as H
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Data.Hashable (Hashable)
import Debug.Trace
--------------------------------------------------------------------------------

examplePsProgram :: Program Lam.Term
examplePsProgram = Program
  [ ("add",
      _)
  , ("main",
     _)
  ]

--------------------------------------------------------------------------------
-- ANF AST

data Value = IntVal Int
           | Var Name
           | Global Name
           deriving (Show, Generic)

data Term = App Value Value
          | Let Text Term Term
          | Lam Name Term
          | Val Value
          | Prim (PrimOp Value)
          | IfThenElse Value Term Term
          | Tuple (List Value)
          | Proj Int Value
          deriving (Show, Generic)

makeBaseFunctor ''Term

values :: Traversal' Term Value
values k (App f x)          = App <$> k f <*> k x
values k (Let x e m)        = Let x <$> values k e <*> values k m
values k (Lam x m)          = Lam x <$> values k m
values k (Val v)            = Val <$> k v
values k (Prim p)           = Prim <$> traverse k p
values k (IfThenElse c t f) = IfThenElse <$> k c <*> values k t <*> values k f
values k (Tuple xs)         = Tuple <$> traverse k xs
values k (Proj n v)         = Proj n <$> k v

--------------------------------------------------------------------------------
-- Rename

data Unique :: Effect where
  Fresh :: Unique m Text

type instance DispatchOf Unique = Dynamic

fresh :: (Unique :> es) => Eff es Text
fresh = send Fresh

mkFresh :: (Unique :> es) => Name -> Eff es Name
mkFresh x = (\n -> x <> "_" <> n) <$> fresh

runUnique :: Eff (Unique ': es) a -> Eff es a
runUnique = reinterpret (evalState (0 :: Int)) $ const $ \case
  Fresh -> do
   n <- get @Int
   modify @Int (+1)
   pure (("x__"<>) . T.pack . show $ n)

renameProgram :: (Unique :> es) => Program Lam.Term -> Eff es (Program Lam.Term)
renameProgram = _

type RnEnv = H.HashMap Name Name

unsafeLookup :: (Show k, Eq k, Hashable k) => k -> H.HashMap k v -> v
unsafeLookup k = fromMaybe (error $ "unsafeLookup: " <> show k) . H.lookup k

rename :: (Unique :> es) => RnEnv -> Lam.Term -> Eff es Lam.Term
rename g (Lam.Var x) = pure . Lam.Var $ unsafeLookup x g
rename g (Lam.Lam x m) = do
  x' <- mkFresh x
  let g' = H.insert x x' g
  Lam.Lam x' <$> rename g' m
rename g (Lam.Let x e m) = do
  x' <- mkFresh x
  let g' = H.insert x x' g
  Lam.Let x' e <$> rename g' m
rename g e = plate (rename g) e

--------------------------------------------------------------------------------
-- LowerANF

anfProgram :: Program Lam.Term -> Program Term
anfProgram = runPureEff . runUnique . traverse anfTerm

anfTerm :: (Unique :> es) => Lam.Term -> Eff es Term
anfTerm = cataM anfTermF

-- TODO: document
float :: (Unique :> es) => Term -> Eff es (Term -> Term, Value)
float (Val v) = pure (id, v)
float x = do { nm <- fresh; pure (Let nm x, Var nm) }

anfTermF :: forall es. (Unique :> es) => Lam.TermF Term -> Eff es Term

anfTermF (Lam.VarF x) = pure . Val . Var $ x
anfTermF (Lam.IntValF n) = pure . Val . IntVal $ n

anfTermF (Lam.AppF f x) = do
  (f̂, f') <- float f
  (x̂, x') <- float x
  pure (f̂ . x̂ $ App f' x')

anfTermF (Lam.IfThenElseF c t f) = do
  (ĉ, c') <- float c
  pure (ĉ $ IfThenElse c' t f)

anfTermF (Lam.PrimF p) =
    fmap app . getCompose . traverse float' $ p
  where
    app (endo,pv) = appEndo endo (Prim pv)
    -- beautiful... }:3
    float' :: Term -> Compose (Eff es) ((,) (Endo Term)) Value
    float' = Compose . fmap (first Endo) . float

anfTermF (Lam.LamF x m) = pure $ Lam x m

anfTermF (Lam.LetF x e m) = pure $ Let x e m

--------------------------------------------------------------------------------
-- Closure-conversion

freeVars :: Term -> HS.HashSet Name
freeVars (Lam x m)   = HS.delete x (freeVars m)
freeVars (Let x e m) = HS.delete x (freeVars m <> freeVars e)
freeVars xs          = foldMapOf (values . #Var) HS.singleton xs

convert :: (Unique :> es) => Term -> Eff es Term
convert (Lam x m) = pure $ _
