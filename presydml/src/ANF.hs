{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE BlockArguments, LambdaCase #-}
{-# LANGUAGE OverloadedLabels, OverloadedStrings #-}
{-# LANGUAGE DataKinds, TypeFamilies, DeriveTraversable #-}
{-# OPTIONS_GHC -Wno-typed-holes #-}
module ANF
  -- ( Value(..)
  -- , Term(..)
  -- , Unique, fresh, mkFresh, runUnique
  -- , renameProgram, rename
  -- , anfTermF, anfTerm, anfProgram
  -- )
  where
--------------------------------------------------------------------------------
import Data.Sequence qualified as Seq
import           Data.Data
import Data.HashSet qualified as HS
import           Data.Monoid
import           Control.Lens
import           SydPrelude
import Lam.Syntax qualified as Lam
import           Lam.Syntax (Program(..), Name, PrimOp(..))
import Data.Text qualified as T
import Data.HashMap.Strict qualified as H
import           Effectful
import           Effectful.Dispatch.Dynamic
import           Effectful.State.Static.Local
import           Data.Hashable (Hashable)
import           Debug.Trace
import Data.List.NonEmpty qualified as List1
import           Control.Monad.Cont
import           Data.Foldable
import           Data.Functor.Reverse
import           Prettyprinter
import           Data.Sequence (Seq, (><))
--------------------------------------------------------------------------------

examplePsProgram :: Program Lam.Term
examplePsProgram = Program
  [ ("add",
      _)
  , ("main",
     _)
  ]

exampleLamTerm :: Lam.Term
exampleLamTerm = Lam.Lam "x" $ Lam.Lam "y" $ Lam.Prim $ PrimAdd (Lam.Var "x") (Lam.Var "y")
-- exampleAnfAdd :: Term
-- exampleAnfAdd =
--   Let "add" (Lam "x" $ Lam "y" $ Prim $ PrimAdd (Var "x") (Var "y")) $
--   Let "add3" (App (Var "add") (IntVal 3)) $
--   App (Var "add3") (IntVal 2)

--------------------------------------------------------------------------------
-- ANF AST

-- | A terminal value.
data Value = IntVal Int
           | Var Name
           | Global Name
           deriving (Show, Generic, Data)

-- | A term in A-Normalised Form.
data Term
  -- | @let r = f x y z in m@
  = LetApp Name Name (List1 Value) Term
  -- | @let f = \x y z -> n in m
  | LetLam Name (List1 Name) Term Term
  -- | @let r = add# x y in m@
  | LetPrim Name (PrimOp Value) Term
  -- | @let r = (x, y, z) in m@
  | LetTuple Name (List Value) Term
  -- | @let r = projₙ x in m@
  | LetProj Name Natural Name Term
  -- | @if c then t else f@
  | IfThenElse Value Term Term
  -- | A /join point/ @j@ is like a local let-bound continuation, satisfying the
  -- following properties:
  --    * All calls to @j@ are tail calls;
  --    * All applications of @j@ are fully saturated;
  --    * @j@ is not captured in a closure.
  -- These stricter requirements ensure join points never need any allocations.
  --
  -- @let-join j p = n in m@
  | LetJoin Name (Maybe Name) Term Term
  -- | A tail-call to a join point.
  --
  -- @jump j p@
  | Jump Name (Maybe Value)
  -- | @v@
  | Val Value
  deriving (Show, Generic, Data)

instance Plated Term

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
-- Pretty-print

instance Pretty Value where
  pretty = \case
    IntVal n -> viaShow n
    Var n -> pretty n
    Global n -> parens $ "global" <+> pretty n

prettyLet :: Name -> Name -> Term -> Doc ann -> Doc ann
prettyLet kw x m e = align . parens $
  vsep [ pretty kw <+> brackets (pretty x <+> e)
       , indent 2 (pretty m)
       ]

asFunction :: (Pretty a, Pretty b, Foldable t) => a -> t b -> Doc ann
asFunction f xs = Lam.asFunction $ pretty f : xs ^.. folded . to pretty

toSubscript :: Char -> Char
toSubscript = \case
  '1' -> '₁'
  '2' -> '₂'
  '3' -> '₃'
  '4' -> '₄'
  '5' -> '₅'
  '6' -> '₆'
  '7' -> '₇'
  '8' -> '₈'
  '9' -> '₉'
  '0' -> '₀'
  x   -> x

instance Pretty Term where
  pretty (LetApp r f xs m) = prettyLet "let" r m (asFunction f xs)
  pretty (LetLam r xs n m) =
      prettyLet "let" r m lam
    where
      lam = align . parens $ vsep [ "lambda" <+> pars
                                  , indent 2 n' ]
      pars = Lam.asList (pretty <$> List1.toList xs)
      n' = pretty n
  pretty (LetPrim r p m) = prettyLet "let" r m (pretty p)
  pretty (LetTuple r xs m) = prettyLet "let" r m call
    where call = asFunction ("list" :: Text) xs
  pretty (LetProj r n x m) = prettyLet "let" r m call
    where call = asFunction ("proj" <> T.pack (toSubscript <$> show n)) [x]
  pretty (IfThenElse c t f) =
    parens . align . vsep $
      [ "if" <+> pretty c
      , indent 3 $ pretty t
      , indent 3 $ pretty f
      ]
  pretty (LetJoin r mx n m) = prettyLet "let" r m call
    where
      call = Lam.asFunction $ ["join"] <> foldMap ((:[]) . pretty) mx <> [pretty n]
  pretty (Jump l mx) =
    Lam.asFunction $ ["jump"] <> [pretty l] <> foldMap ((:[]) . pretty) mx
  pretty (Val v) = pretty v

prettyApp :: Name -> List Value -> Doc ann
prettyApp f xs = group . parens . hsep $ pretty f : (pretty <$> xs)

--------------------------------------------------------------------------------
-- Rename

data Unique :: Effect where
  Fresh :: Unique m Text

type instance DispatchOf Unique = Dynamic

fresh :: (Unique :> es) => Eff es Text
fresh = send Fresh

mkFresh :: (Unique :> es) => Name -> Eff es Name
mkFresh x = (\n -> x <> "__" <> n) <$> fresh

runUnique :: Eff (Unique ': es) a -> Eff es a
runUnique = reinterpret (evalState (0 :: Int)) $ const $ \case
  Fresh -> do
   n <- get @Int
   modify @Int (+1)
   pure . T.pack . show $ n

renameProgram :: (Unique :> es) => Program Lam.Term -> Eff es (Program Lam.Term)
renameProgram = _

type RnEnv = H.HashMap Name Name

unsafeLookup :: (Show k, Eq k, Hashable k) => k -> H.HashMap k v -> v
unsafeLookup k = fromMaybe (error $ "unsafeLookup: " <> show k) . H.lookup k

rename :: (Unique :> es) => Lam.Term -> Eff es Lam.Term
rename = renameWithEnv mempty

renameWithEnv :: (Unique :> es) => RnEnv -> Lam.Term -> Eff es Lam.Term
renameWithEnv g (Lam.Var x) = pure . Lam.Var $ unsafeLookup x g
renameWithEnv g (Lam.Lam x m) = do
  x' <- mkFresh x
  let g' = H.insert x x' g
  Lam.Lam x' <$> renameWithEnv g' m
renameWithEnv g (Lam.Let x e m) = do
  x' <- mkFresh x
  let g' = H.insert x x' g
  Lam.Let x' e <$> renameWithEnv g' m
renameWithEnv g e = plate (renameWithEnv g) e

--------------------------------------------------------------------------------
-- LowerANF

anfTerm :: forall es. (Unique :> es) => Lam.Term -> Eff es Term
anfTerm = flip go (pure . Val)
  where
    go :: Lam.Term -> (Value -> Eff es Term) -> Eff es Term
    go (Lam.IntVal n) k = k $ IntVal n
    go (Lam.Var x) k = k $ Var x

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
        LetJoin j (Just p) <$> k (Var p) <*> (IfThenElse c' <$> go t jn <*> go f jn)

    go (Lam.Prim p) k =
      -- lol?
      runContT (traverse (ContT . go) p) \p' -> do
        r <- mkFresh "r"
        LetPrim r p' <$> k (Var r)

    go (Lam.Lam x m) k = do
      r <- mkFresh "r"
      m' <- go m (pure . Val)
      LetLam r (List1.singleton x) m' <$> k (Var r)

--------------------------------------------------------------------------------
-- Closure-conversion

toHashSetOf :: Hashable a => Getting (HS.HashSet a) s a -> s -> HS.HashSet a
toHashSetOf l s = getConst (l (Const . HS.singleton) s)

freeVars :: Term -> HS.HashSet Name
freeVars (LetApp x f ys m) = toHashSetOf (each . #Var) ys
                          <> HS.singleton f
                          <> (freeVars m & HS.delete x)
freeVars (LetLam x ys n m) = (freeVars n `HS.difference` toHashSetOf each ys)
                          <> (freeVars m & HS.delete x)
freeVars (LetPrim x vs m)  = toHashSetOf (each . #Var) vs
                          <> (freeVars m & HS.delete x)
freeVars (LetTuple x vs m)  = toHashSetOf (each . #Var) vs
                          <> (freeVars m & HS.delete x)
freeVars (LetProj x i y m) = HS.singleton y
                          <> (freeVars m & HS.delete x)
freeVars (LetJoin x p r m) = freeVars m & HS.delete x
freeVars (Val (Var x))     = HS.singleton x
freeVars xs                = foldMapOf plate freeVars xs

convert :: (Unique :> es) => Term -> Eff es Term
convert (LetLam f xs n m) = do
    (env,fRaw) <- each mkFresh ("env",f <> "_unclosed")
    n' <- fromEnv env fvs <$> convert n
    m' <- convert m
    let closure = LetTuple f (Global fRaw : (Var <$> fvs)) m'
    pure $ LetLam fRaw (List1.cons env xs) n' closure
  where
    fvs = HS.toList $ freeVars n `HS.difference` HS.fromList (List1.toList xs)
    fromEnv :: Name -> List Name -> Term -> Term
    fromEnv env fvs e0 = fst $ foldr g (e0,1) fvs
      where
        g nm (e,n) = (LetProj nm n env e, n+1)

convert (LetApp r f xs m) = do
  code <- mkFresh "code"
  m' <- convert m
  pure $
    LetProj code 0 f $
    LetApp r code (Var f `List1.cons` xs) $
    m'

convert e = traverseOf plate convert e

--------------------------------------------------------------------------------
-- Hoist

data Join = Join Name (Maybe Name) Term
  deriving (Show)

data Lam = Lam Name (List1 Name) Join (List Join)
  deriving (Show)

lamName :: Lens' Lam Name
lamName sbt (Lam f xs j js) = (\f' -> Lam f' xs j js) <$> sbt f

lamParams :: Lens' Lam (List1 Name)
lamParams sbt (Lam f xs j js) = (\xs' -> Lam f xs' j js) <$> sbt xs

data Hoisted a = Hoisted !(Seq Lam) !(Seq Join) a
  deriving (Functor)

instance Applicative Hoisted where
  pure = Hoisted mempty mempty
  Hoisted fs js f <*> Hoisted fs' js' a = Hoisted (fs <> fs') (js <> js') (f a)

hoist :: forall es. (Unique :> es) => Term -> Eff es (List1 Lam)
hoist = go >=> finalise
  where
    finalise (Hoisted fs js t) = do
      (dummy,entry) <- each mkFresh ("dummy","entry")
      let jn = Join entry Nothing t
          main = Lam "main" (List1.singleton dummy) jn (toList js)
      pure $ main :| toList fs

    go :: Term -> Eff es (Hoisted Term)
    go (LetLam f xs n m) = do
      Hoisted fs js n' <- go n
      Hoisted fs' js' m' <- go m
      entry <- mkFresh "entry"
      let fn = Lam f xs (Join entry Nothing n') (toList js)
      pure $ Hoisted (fs >< fs' >< Seq.singleton fn) js' m'

    go (LetJoin j p n m) = do
      Hoisted fs js n' <- go n
      Hoisted fs' js' m' <- go m
      let jn = Join j p n'
      pure $ Hoisted (fs >< fs') (Seq.singleton jn >< js >< js') m'

    go (IfThenElse c t f) = do
      Hoisted fs js t' <- go t
      Hoisted fs' js' f' <- go f
      (th,el) <- each mkFresh ("then","else")
      let bt = Join th Nothing t'
          bf = Join el Nothing f'
      pure $ Hoisted (fs >< fs') (Seq.fromList [bt,bf] >< js >< js') $
        IfThenElse c (Jump th Nothing) (Jump el Nothing)

    go e = getCompose $ traverseOf plate (Compose . go) e

instance Pretty Join where
  pretty (Join j p m) = align . parens . vsep $
    [ "define-join" <+> pretty j <+> maybe "()" (parens . pretty) p
    , indent 2 $ pretty m
    ]

instance Pretty Lam where
  pretty (Lam f xs j js) = align . parens . vsep $
      [ "define" <+> pars
      , indent 2 $ pretty j
      , indent 2 js'
      ]
    where
      pars = Lam.asList $ pretty f : (pretty <$> List1.toList xs)
      js' = Lam.asList $ "joins" : (pretty <$> js)

instance Pretty a => Pretty (Hoisted a) where
  pretty = _

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
