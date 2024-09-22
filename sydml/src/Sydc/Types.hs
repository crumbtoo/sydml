{-# LANGUAGE NoFieldSelectors #-}
--------------------------------------------------------------------------------
module Sydc.Types
  where
--------------------------------------------------------------------------------
import Data.Hashable
import Data.HashSet qualified as H
import GHC.Generics
import SydPrelude
import Data.IORef
--------------------------------------------------------------------------------

data SydOptions = SydOptions
  { debugFlags :: DebugFlags
  , buildDir :: FilePath
  , sourceDirectories :: H.HashSet FilePath
  }
  deriving (Generic)

data Command = BatchCmd SydBatchOptions
  deriving (Generic)

data SydBatchOptions = SydBatchOptions
  { sydOptions :: SydOptions
  , files :: List FilePath
  }
  deriving (Generic)

--------------------------------------------------------------------------------

newtype DebugFlags = DebugFlags (H.HashSet Text)
  deriving stock (Show, Generic)
  deriving newtype (Semigroup, Monoid)
