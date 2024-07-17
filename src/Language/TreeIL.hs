{-# LANGUAGE OverloadedLists #-}
module Language.TreeIL
  ( TreeIL(..)
  )
  where
--------------------------------------------------------------------------------
import GHC.Generics
import Control.Lens
import Control.Lens.Unsound
import Data.Sexp
import Data.SCargot.Repr.Basic
import Data.HashMap.Strict        qualified as H
import Data.Functor.Foldable
import Sydc
import SydPrelude
--------------------------------------------------------------------------------

parseTreeIL :: Sexp -> Either SydError TreeIL

parseTreeIL (A (ASymbol x)) = pure $ LexRefE x

parseTreeIL ("lambda" ::: bs ::: e ::: SNil) = do
  LamE <$> parseFormals bs <*> parseTreeIL e

parseTreeIL (f ::: xs) = CallE <$> parseTreeIL f <*> xs'
  where
    xs' = xs ^.. _cars & traverse parseTreeIL

parseTreeIL e = Left $ minimalError ParseError s
  where
    s = "malformed input:\n" <> printSexp e

--------------------------------------------------------------------------------

data Formals a = Formals
  { args :: List a
  , rest :: Maybe a
  }
  deriving (Show, Generic)

expect :: APrism s Sexp a b -> s -> Either SydError a
expect p = first expectedSymbol . matching p
  where
    expectedSymbol s = minimalError ParseError $
      "expected symbol in lambda formals. got: " <> printSexp s

parseFormals :: Sexp -> Either SydError (Formals Symbol)

parseFormals p@(SCons _ _) = go p
  where
    go :: _ -> Either SydError (Formals Symbol)
    go (SCons car cdr) = do
      car' <- expect _SSymbol car
      go cdr <&> (#args) %~ (car':)
    go a@(SAtom _) = Formals [] . Just <$> expect _SSymbol a
    go SNil = pure $ Formals [] Nothing

parseFormals SNil = Right $ Formals [] Nothing
parseFormals _    = Left $ minimalError ParseError "formals should be a list"

data TreeIL = VoidE
            | ConstE Atom
            | PrimRefE Symbol
            | LexRefE Symbol
            | LexSetE Symbol TreeIL
            | TopRefE Symbol
            | TopSetE Symbol TreeIL
            | TopDefE Symbol TreeIL
            | IfE TreeIL TreeIL TreeIL
            | SeqE TreeIL TreeIL
            | CallE TreeIL (List TreeIL)
            | LamE (Formals Symbol) TreeIL
            deriving (Show)

