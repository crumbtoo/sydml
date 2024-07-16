module Language.Scheme.Syntax
  ( Atom(..)
  , Sexp
  , SExpr(..)
  , Symbol
  )
  where
--------------------------------------------------------------------------------
import Data.SCargot.Repr.Basic
import Data.Functor.Foldable
import SydPrelude
--------------------------------------------------------------------------------

type Symbol = Text

data Atom = ABool Bool
          | ASymbol Symbol
          | AInt Integer
          | AString Text
          deriving (Show)

type Sexp = SExpr Atom

