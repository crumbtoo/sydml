{-# LANGUAGE TemplateHaskell #-}
module Presydc.Lam.Syntax where
--------------------------------------------------------------------------------
import Control.Lens
import SydPrelude
import Data.Located (Position(line))
import Data.Data (Data)
import Data.Data.Lens (uniplate)
import Language.LSP.Protocol.Lens (HasLanguage(language))
import Data.String (IsString (fromString))
import Data.Text qualified as T
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
              deriving (Show, Functor, Foldable, Traversable, Data)

type Name = Text

newtype Program a = Program (List (Name, a))
  deriving (Functor, Foldable, Traversable)

instance Each (Program a) (Program b) (Name, a) (Name, b) where
  each k (Program ds) = Program <$> each k ds

makeBaseFunctor ''Term

zCombinator :: Term
zCombinator = Lam "f" $ d `App` d
  where
    d = Lam "x" $ "f" `App` (Lam "v" $ "x" `App` "x" `App` "v")
