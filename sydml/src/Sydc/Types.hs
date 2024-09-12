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
  , command :: Command
  , sourceDirectories :: H.HashSet FilePath
  }

data Command = BatchCmd SydBatchOptions

data SydBatchOptions = SydBatchOptions
  { files :: List FilePath
  }
  deriving (Generic)

--------------------------------------------------------------------------------

newtype DebugFlags = DebugFlags (H.HashSet Text)
