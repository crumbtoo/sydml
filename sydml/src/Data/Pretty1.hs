{-# LANGUAGE QuantifiedConstraints #-}
module Data.Pretty1
  ( Pretty1(..)
  , PrettySig
  , PrettyListSig
  )
  where
--------------------------------------------------------------------------------
import Prettyprinter
import SydPrelude
import Control.Comonad.Cofree
--------------------------------------------------------------------------------

type PrettySig ann a = a -> Doc ann

type PrettyListSig ann a = List a -> Doc ann

class (forall a. Pretty a => Pretty (f a)) => Pretty1 (f :: Type -> Type) where
  liftPretty :: PrettySig ann a -> PrettyListSig ann a -> f a -> Doc ann
  liftPrettyList
    :: PrettySig ann a -> PrettyListSig ann a
    -> List (f a) -> Doc ann
  liftPrettyList pr pl = align . list . fmap (liftPretty pr pl)

pretty1 :: (Pretty1 f, Pretty a) => f a -> Doc ann
pretty1 = liftPretty pretty prettyList

instance Pretty1 f => Pretty1 (Cofree f) where
  liftPretty = todo "Pretty1 (Cofree f)"

instance (Pretty1 f, Pretty a) => Pretty (Cofree f a) where
  pretty = todo "Pretty (Cofree f a)"

instance (Functor f, Pretty1 f) => Pretty (Fix f) where
  pretty = foldFix (liftPretty id (align . list))
