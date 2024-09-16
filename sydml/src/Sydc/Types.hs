{-# LANGUAGE NoFieldSelectors #-}
--------------------------------------------------------------------------------
module Sydc.Types
  where
--------------------------------------------------------------------------------
import Data.Hashable
import Data.HashSet qualified as H
import GHC.Generics
import SydPrelude
--------------------------------------------------------------------------------

data SydOptions = SydOptions
  { debugFlags :: DebugFlags
  , buildDir :: FilePath
  , sourceDirectories :: H.HashSet FilePath
  }
  deriving (Show, Generic)

data Command = BatchCmd SydBatchOptions
  deriving (Show, Generic)

data SydBatchOptions = SydBatchOptions
  { sydOptions :: SydOptions
  , files :: List FilePath
  }
  deriving (Show, Generic)

--------------------------------------------------------------------------------

newtype DebugFlags = DebugFlags (H.HashSet Text)
  deriving stock (Show, Generic)
  deriving newtype (Semigroup, Monoid)
