{-# LANGUAGE TemplateHaskell #-}
module Data.EDN.Syntax
  ( ValueF(..)
  , LocatedValue
  , Value
  )
  where
--------------------------------------------------------------------------------
import SydPrelude
import Data.HashMap.Strict (HashMap)
import Data.HashSet (HashSet)
import Control.Comonad.Cofree
import Data.Located
import Data.Deriving
import GHC.Generics
import Data.Functor.Classes
import Data.Pretty1
import Prettyprinter
import Control.Lens hiding ((:<))
--------------------------------------------------------------------------------

data ValueF a where
  -- NilF :: ValueF a
  -- BooleanF :: !Bool -> ValueF a
  -- StringF :: !Text -> ValueF a
  -- CharacterF :: !Char -> ValueF a
  SymbolF :: !Text -> !Text -> ValueF a
  KeywordF :: !Text -> ValueF a
  IntegerF :: !Int -> ValueF a
  -- FloatingF :: !Double -> ValueF a
  ListF :: !(List a) -> ValueF a
  VecF :: !(List a) -> ValueF a
  -- MapF :: !(HashMap a a) -> ValueF a
  -- SetF :: !(HashSet a) -> ValueF a
  deriving ( Eq, Ord, Show, Data, Generic, Generic1
           , Functor, Foldable, Traversable )

type LocatedValue = Cofree ValueF SrcSpan

type Value = Fix ValueF

deriveShow1 ''ValueF
deriveEq1 ''ValueF
deriveOrd1 ''ValueF

instance Pretty a => Pretty (ValueF a) where
  pretty = liftPretty pretty prettyList

instance Pretty1 ValueF where
  liftPretty pr prl (SymbolF "" s) = pretty s
  liftPretty pr prl (SymbolF prefix s) = pretty prefix <> "/" <> pretty s
  liftPretty pr prl (KeywordF k) = ":" <> pretty k
  liftPretty pr prl (IntegerF n) = pretty n
  -- TODO: make it pretty! indent & align.
  liftPretty pr prl (ListF xs) = "(" <> hsep (pr <$> xs) <> ")"
  liftPretty pr prl (VecF xs) = "[" <> hsep (pr <$> xs) <> "]"

-- prettyLocatedValue :: LocatedValue -> Doc ann
-- prettyLocatedValue = vsep . fmap concatLineSrcSpan . go
--   where
--     concatLineSrcSpan (d,ss) = d <+> ";" <+> pretty ss
--     go (ss :< SymbolF "" s)     = pretty s
--     go (ss :< SymbolF prefix s) = pretty prefix <> "/" <> pretty s
--     go (ss :< KeywordF k)       = ":" <> pretty k
--     go (ss :< IntegerF n)       = pretty n
--     go (ss :< ListF xs)         = "(" <> vsep xs <> ")"
--     go (ss :< VecF xs)          = "[" <> hsep (pr <$> xs) <> "]"
