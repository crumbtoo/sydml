{-# LANGUAGE TemplateHaskell #-}
module Presydc.ANF
  -- ( Value(..)
  -- , Term(..)
  -- , Unique, fresh, mkFresh, runUnique
  -- , renameProgram, rename
  -- , anfTermF, anfTerm, anfProgram
  -- )
  where
--------------------------------------------------------------------------------
import Data.Data
import Data.HashSet qualified as HS
import Data.Monoid
import Control.Lens
import SydPrelude
import Presydc.Lam.Syntax qualified as Lam
import Presydc.Lam.Syntax (Program(..), Name, PrimOp(..))
import Data.Functor.Foldable.Monadic
import Data.Text qualified as T
import Data.HashMap.Strict qualified as H
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import Data.Hashable (Hashable)
import Debug.Trace
import Data.List.NonEmpty qualified as List1
import Control.Monad.Cont
--------------------------------------------------------------------------------

examplePsProgram :: Program Lam.Term
examplePsProgram = Program
  [ ("add",
      _)
  , ("main",
     _)
  ]

-- exampleAnfAdd :: Term
-- exampleAnfAdd =
--   Let "add" (Lam "x" $ Lam "y" $ Prim $ PrimAdd (Var "x") (Var "y")) $
--   Let "add3" (App (Var "add") (IntVal 3)) $
--   App (Var "add3") (IntVal 2)

--------------------------------------------------------------------------------
-- ANF AST

data Value = IntVal Int
           | Var Name
           | Global Name
           deriving (Show, Generic, Data)

data Term = LetApp Name Name (List1 Value) Term
          | LetLam Name (List1 Name) Term Term
          | LetPrim Name (PrimOp Value) Term
          | LetTuple Name (List Value) Term
          | LetProj Name Natural Name Term
          | IfThenElse Value Term Term
          | Join Name (Maybe Name) Term Term
          | Jump Name (Maybe Value)
          | Val Value
          deriving (Show, Generic, Data)

instance Plated Term

makeBaseFunctor ''Term

-- values :: Traversal' Term Value
-- values k (App f x)          = App <$> k f <*> k x
-- -- values k (Let x e m)        = Let x <$> values k e <*> values k m
-- values k (Lam x m)          = Lam x <$> values k m
-- values k (Val v)            = Val <$> k v
-- values k (Prim p)           = Prim <$> traverse k p
-- values k (IfThenElse c t f) = IfThenElse <$> k c <*> values k t <*> values k f
-- values k (Tuple xs)         = Tuple <$> traverse k xs
-- values k (Proj n v)         = Proj n <$> k v

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

newtype Kendo m a = Kendo { appKendo :: a -> m a }

appKendo (Kendo r) = r

instance Monad m => Semigroup (Kendo m a) where
  (<>) = Kendo .: (<=<) `on` appKendo

instance Monad m => Monoid (Kendo m a) where
  mempty = Kendo pure

anfTerm :: forall es. (Unique :> es) => Lam.Term -> Eff es Term
anfTerm = flip go (pure . Val)
  where
    go :: Lam.Term -> (Value -> Eff es Term) -> Eff es Term
    go (Lam.IntVal n) k = k $ IntVal n
    go (Lam.Var x) k  = k $ Var x

    go (Lam.App f x) k =
      go f \case
        Var f' ->
          go x \x' -> do
            r <- mkFresh "r"
            LetApp r f' (List1.singleton x') <$> k (Var r)
        _ -> error "must apply a named value"

    go (Lam.IfThenElse c t f) k =
      go c \c' -> do
        (r,j,p) <- each mkFresh ("r","j","p")
        let jn = pure . Jump j . Just
        Join j (Just p) <$> k (Var p) <*> (IfThenElse c' <$> go t jn <*> go f jn)

    go (Lam.Prim p) k =
      -- lol?
      runContT (traverse (ContT . go) p) \p' -> do
        r <- mkFresh "r"
        LetPrim r p' <$> k (Var r)

    go (Lam.Lam x m) k =
      go m \m' -> do
        r <- mkFresh "r"
        LetLam r (List1.singleton x) (Val m') <$> k (Var r)

-- --------------------------------------------------------------------------------
-- -- Closure-conversion

-- freeVars :: Term -> HS.HashSet Name
-- freeVars (Lam x m)   = HS.delete x (freeVars m)
-- freeVars (Let x e m) = HS.delete x (freeVars m <> freeVars e)
-- freeVars xs          = foldMapOf (values . #Var) HS.singleton xs

-- convert :: (Unique :> es) => Term -> Eff es Term
-- convert f@(Lam x m) = do
--   let fvs = HS.toList . freeVars $ f
--   f' <- mkFresh "lam"
--   envAndX <- mkFresh "envAndX"
--   env <- mkFresh "env"
--   let projEnv (i,y) = Let y (Proj i (Var env))
--   m' <- convert m
--   let m'' = Let env (Proj 0 (Var envAndX)) $
--             Let x (Proj 1 (Var envAndX)) $
--             foldr projEnv m' ([1..] `zip` fvs)
--   let code = Lam envAndX m''
--   pure $ Let f' code (Tuple (Var <$> f' : fvs))

-- convert (App f x) = do
--   code <- mkFresh "code"
--   arg <- mkFresh "args"
--   pure $
--     Let code (Proj 0 f) $
--     Let arg (Tuple [f, x]) $
--     App (Var code) (Var arg)

-- convert e = traverseOf plate convert e
-- -- convertF :: (Unique :> es) => TermF Term -> Eff es Term
-- -- convertF f@(LamF x m) =

-- --------------------------------------------------------------------------------
-- -- ANF interpreter

-- type Ctx = H.HashMap Name Term

-- evalAnf :: Ctx -> Term -> Term

-- evalAnf g (Proj i v) =
--   case evalAnfVal g v of
--     Tuple vs -> Val $ vs !! i
--     e -> error $ "Proj'd: " <> show e

-- evalAnf g (Val v) = evalAnfVal g v

-- evalAnf g (App f y) =
--   case evalAnfVal g f of
--     Lam x m -> evalAnf g' m
--       where g' = g & H.insert x (evalAnfVal g y)
--     e -> error $ "applied: " <> show e

-- evalAnf g (Let x e m) = evalAnf g' m
--   where
--     g' = g & H.insert x (evalAnf g e)

-- evalAnf _ e@(Tuple vs) = e
-- evalAnf _ e@(Lam _ _) = e

-- evalAnf g (Prim p) =
--   case evalAnfVal g <$> p of
--     PrimAdd (Val (IntVal x)) (Val (IntVal y)) -> Val $ IntVal (x + y)

-- evalAnf _ e = error $ show e

-- evalAnfVal :: Ctx -> Value -> Term
-- evalAnfVal g (Var x) = unsafeLookup x g
-- evalAnfVal _ v       = Val v
