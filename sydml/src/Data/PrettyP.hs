{-# LANGUAGE DefaultSignatures, UndecidableInstances #-}
module Data.PrettyP
  ( PrettyP(..)
  , prettyp0
  , module Prettyprinter
  , pIf
  )
  where
--------------------------------------------------------------------------------
import Prettyprinter
import Data.Fix
import Control.Comonad.Cofree
import SydPrelude
--------------------------------------------------------------------------------

instance PrettyP Text
instance PrettyP Int

prettyp0 :: PrettyP a => a -> Doc ann
prettyp0 = prettyp 0

class PrettyP a where
  prettyp :: Int -> a -> Doc ann

  default prettyp :: Pretty a => Int -> a -> Doc ann
  prettyp = const pretty

pIf :: Bool -> Doc ann -> Doc ann
pIf = bool id parens

instance PrettyP (f (Fix f)) => PrettyP (Fix f) where
  prettyp d (Fix f) = prettyp d f

instance PrettyP (f (Cofree f a)) => PrettyP (Cofree f a) where
  prettyp d (_ :< as) = prettyp d as

