{-# LANGUAGE FunctionalDependencies #-}
module Data.Located
  -- * Types
  ( Located(..)
  , SrcSpan(..)
  , Position(..)
  , Loc
  -- * Optics
  , HasLocation(..)
  , Unlocate(..)
  -- * Combinators
  , upward
  , constL1
  -- -- * Megaparsec helpers
  -- , sourcePosToPosition
  -- , locate
  -- , getPosition
  )
  where
--------------------------------------------------------------------------------
import Control.Lens                     hiding ((:<))
import Control.Comonad.Cofree
import Data.Functor.Foldable
import Data.Functor.Apply
import Data.Functor.Bind
import Numeric.Natural
-- import Text.Megaparsec
import SydPrelude
--------------------------------------------------------------------------------

-- | A position of a single character in a text input. Both fields are
-- 1-indexed.
data Position = Position
  { line   :: !Natural
  , column :: !Natural
  }
  deriving (Eq, Ord, Show)

-- | A description of a span of text.
-- INVARIANT: @start < end@.
data SrcSpan = SrcSpan
  { file  :: FilePath
  , start :: !Position
  , end   :: !Position
  }
  deriving (Eq, Ord, Show, Generic)

-- | Take the "hull" of the two spans; i.e., @a <> b@ is the smallest 'SrcSpan'
-- containing both @a@ and @b@. This is seldom used with spans from different
-- files, so we feel justified in casually defining the resulting 'SrcSpan' to
-- belong to the same file as the left operand.
instance Semigroup SrcSpan where
  -- commutative modulo filepath!
  SrcSpan fp sa ea <> SrcSpan _ sb eb = SrcSpan fp s e
    where
      s = min sa sb
      e = max ea eb

data Located a = At !SrcSpan a
  deriving (Show, Functor, Eq, Ord, Generic)

instance Apply Located where
  liftF2 f (At ss a) (At ss' b) = At (ss <> ss') (f a b)

instance Bind Located where
  join (At ss (At ss' a)) = At (ss <> ss') a

type Loc t = Cofree (Base t) SrcSpan

--------------------------------------------------------------------------------
-- Class

class HasLocation s where
  location :: Lens' s SrcSpan

-- this class is a bit silly. :\
class HasLocation s => Unlocate a s | s -> a where
  unlocated :: Lens' s a

instance HasLocation (Located a) where
  location afb (At ss a) = (\ss' -> At ss' a) <$> afb ss

instance Unlocate a (Located a) where
  unlocated afb (At ss a) = At ss <$> afb a

instance HasLocation SrcSpan where
  location = id

instance Unlocate SrcSpan SrcSpan where
  unlocated = id

instance HasLocation a => HasLocation (Cofree f a) where
  location afb (a :< as) = (:< as) <$> location afb a

instance HasLocation a => Unlocate (f (Cofree f a)) (Cofree f a) where
  unlocated afb (a :< as) = (a :<) <$> afb as

instance Pretty a => Pretty (Located a) where
  pretty = pretty . view unlocated

--------------------------------------------------------------------------------
-- Combinators

-- i don't feel like working out a single `upward` generalised over Haslocation
-- and Unlocate right now.
upward :: Semigroup a
       => (Cofree f a -> Cofree f a -> f (Cofree f a))
       -> Cofree f a -> Cofree f a -> Cofree f a
upward f a@(x :< _) b@(y :< _) = x <> y :< f a b

-- | Resize `b` to span both `a` and `b`.
constL1 :: (HasLocation a, HasLocation b) => a -> b -> b
constL1 x y = y & location <>~ (x ^. location)

--------------------------------------------------------------------------------

-- sourcePosToPosition :: SourcePos -> Position
-- sourcePosToPosition pos =
--     Position (pos ^. #sourceLine . nat) (pos ^. #sourceColumn . nat)
--   where nat = to $ fromIntegral . unPos

-- getPosition :: (TraversableStream s, MonadParsec e s m) => m Position
-- getPosition = sourcePosToPosition <$> getSourcePos

-- locate :: (TraversableStream s, MonadParsec e s m) => m a -> m (Located a)
-- locate p = do
--     pos <- getSourcePos
--     a <- p
--     pos' <- getSourcePos
--     pure $ At (SrcSpan (pos ^. #sourceName) (f pos) (f pos')) a
--   where
--     f = sourcePosToPosition
