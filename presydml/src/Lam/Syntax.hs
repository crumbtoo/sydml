{-# LANGUAGE TemplateHaskell, OverloadedStrings #-}
module Lam.Syntax where
--------------------------------------------------------------------------------
import Control.Lens
import SydPrelude
-- import Data.Located (Position(line))
import Data.Data (Data)
import Data.Data.Lens (uniplate)
import Data.String (IsString (fromString))
import Data.Text qualified as T
import Prettyprinter
import Data.Functor.Apply qualified
--------------------------------------------------------------------------------

data Term = App Term Term
          | Let Name Term Term
          --  LetRec (List (b, Expr b e)) (Expr b e)
          | IfThenElse Term Term Term
          | Prim (PrimOp Term)
          | IntVal Int
          | Lam Name Term
          | Var Name
          deriving (Show, Data)

instance IsString Term where
  fromString = Var . T.pack

instance Plated Term where
  plate = uniplate

data PrimOp a = PrimAdd a a
              | PrimMul a a
              | PrimSub a a
              | PrimPrint a
              | PrimPrintInt a
              deriving (Show, Functor, Foldable, Traversable, Data)

instance Each (PrimOp a) (PrimOp b) a b

instance Pretty a => Pretty (PrimOp a) where
  pretty p = asFunction $ name : (pretty <$> p ^.. each)
    where
      name = case p of
        PrimAdd _ _ -> "int+"
        PrimMul _ _ -> "int*"
        PrimSub _ _ -> "int-"
        PrimPrint _ -> "print"
        PrimPrintInt _ -> "int-print"

asFunction :: List (Doc ann) -> Doc ann
asFunction [] = "()"
asFunction (x : xs) =
  group . align . parens $
    x <> softline
      <> flatAlt (indent 2 . align . vsep $ xs) (hsep xs)

asList :: List (Doc ann) -> Doc ann
asList = parens . group . align . vsep

type Name = Text

newtype Program a = Program (List (Name, a))
  deriving (Functor, Foldable, Traversable)

instance Each (Program a) (Program b) (Name, a) (Name, b) where
  each k (Program ds) = Program <$> each k ds

-- makeBaseFunctor ''Term

{- |
The Z combinator — a strict fixed-point combinator.

@
Z g v = g (Z g) v
Z = λf. (λx. f (λv. x x v)) (λx. f (λv. x x v))
@
-}

zCombinator :: Term
zCombinator = Lam "f" $ d `App` d
  where
    d = Lam "x" $ "f" `App` (Lam "v" $ "x" `App` "x" `App` "v")

pIf :: Bool -> Doc ann -> Doc ann
pIf = bool id parens

applicants :: Traversal1' Term Term
applicants k (App f x) = App <$> applicants k f Data.Functor.Apply.<.> k x
applicants k x         = k x

instance Pretty Term where
  pretty = go 0
    where
      go d app@(App _ _) = pIf (d>11) $
        case toNonEmptyOf applicants app of
          f:|xs -> vsep $ go 11 f : (indent 2 . go 12 <$> xs)
      go d (IfThenElse c t f) = pIf (d>0) $ vsep
        [ "if" <+> go 0 c
        , indent 2 $ "then" <+> go 0 t
        , indent 2 $ "else" <+> go 0 f
        ]
      go d (Lam x m) = pIf (d>0) . vsep $
        [ "λ" <> pretty x <+> "->"
        , indent 2 $ go 0 m
        ]
      go d (Var x) = pretty x
      go d (Prim p) = pIf (d>11) $ pretty p
      go d (IntVal x) = pretty x
