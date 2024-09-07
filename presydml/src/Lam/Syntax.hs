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
              | PrimPrint a
              | PrimPrintInt a
              deriving (Show, Functor, Foldable, Traversable, Data)

instance Each (PrimOp a) (PrimOp b) a b

instance Pretty a => Pretty (PrimOp a) where
  pretty p = asFunction $ name : (pretty <$> p ^.. each)
    where
      name = case p of
        PrimAdd _ _ -> "add#"
        PrimPrint _ -> "print#"

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

zCombinator :: Term
zCombinator = Lam "f" $ d `App` d
  where
    d = Lam "x" $ "f" `App` (Lam "v" $ "x" `App` "x" `App` "v")
