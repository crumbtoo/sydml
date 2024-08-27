module SydPrelude
  -- * Types
  -- ** Misc.
  ( T.Text
  , Void, void
  , Type
  -- ** Lists
  , List, List1, NE.NonEmpty((:|))
  -- * Classes
  , MonadIO(liftIO)
  , Generic
  , Data.Foldable.fold
  , Pretty(pretty)
  , Filterable(..)
  , Witherable(..)
  , Foldable1(foldMap1)
  -- * Functions
  , ($>), (&), on, (.:), bimap, first, second
  , fromMaybe
  , fromRight
  , printf
  , (<=<), (>=>), join
  , traverse_
  , for
  , for_
  , bool
  -- * Recursion schemes
  , Compose(..), Fix(Fix), foldFix, foldFixM
  , cata, cataM, para, paraM
  , ListF(..)
  , makeBaseFunctor
  , Recursive(project), Corecursive(embed)
  -- * Optics
  , Lens, Lens'
  , Traversal, Traversal'
  , Prism, Prism'
  -- * Debug
  , trace, traceShow
  , tracePretty
  )
  where
--------------------------------------------------------------------------------
import Control.Lens hiding (para)
import Data.Generics.Labels ()
import Control.Monad.IO.Class
import Control.Monad
import Data.Maybe
import Data.Void
import Data.Bifunctor
import Data.Functor
import Data.Functor.Foldable
import Data.Foldable1
import Data.Functor.Foldable.TH
import Data.Functor.Foldable.Monadic
import Data.Function
import Data.Foldable
import Data.Traversable
import Data.Text qualified as T
import Data.Fix hiding (cata, cataM)
import Data.Functor.Compose
import Data.Either
import Data.Kind
import Data.List.NonEmpty qualified as NE
import Data.Text.Lens
import Data.Bool
import Witherable
import Text.Printf
import GHC.List
import GHC.Generics
import Prettyprinter
import Text.Pretty.Simple
import Debug.Trace
--------------------------------------------------------------------------------

type List1 = NE.NonEmpty

infixr 9 .:
  
-- | Blackbird operator.
(.:) :: (c -> d) -> (a -> b -> c) -> a -> b -> d
(f .: g) x y = f (g x y)

tracePretty :: Show a => a -> b -> b
tracePretty a = trace (view unpacked $ pShow a)
