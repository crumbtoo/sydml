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

newtype Ident = Ident Text
  deriving (Show, Generic)

newtype Namespace = Namespace (List Ident)
  deriving (Show, Generic)

data Name = Qualified Namespace Ident
  deriving (Show, Generic)

-- temp definition.
data SourceCtx = SourceCtx
  deriving (Show, Generic)

data SydOptions = SydOptions
  {
  }

data SydCompileOptions = SydCompileOptions
  { debugFlags :: DebugFlags
  }

--------------------------------------------------------------------------------

newtype DebugFlags = DebugFlags (H.HashSet Text)
