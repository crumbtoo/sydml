module SydPrelude
  ( T.Text
  , MonadIO(liftIO)
  , List
  , Data.Foldable.fold
  , ($>)
  , (&)
  , bimap, first, second
  , fromRight
  , printf
  -- * Recursion schemes
  , Compose(..)
  , Fix(Fix)
  , foldFix, foldFixM
  , cata, cataM
  , ListF(..)
  -- * Optics
  , Lens, Lens'
  , Traversal, Traversal'
  , Prism, Prism'
  )
  where
--------------------------------------------------------------------------------
import Control.Lens
import Data.Generics.Labels ()
import Control.Monad.IO.Class
import Data.Bifunctor
import Data.Functor
import Data.Functor.Foldable
import Data.Functor.Foldable.Monadic
import Data.Function
import Data.Foldable
import Data.Text qualified as T
import Data.Fix hiding (cata, cataM)
import Data.Functor.Compose
import Data.Either
import Text.Printf
import GHC.List
--------------------------------------------------------------------------------

