{-# LANGUAGE NoFieldSelectors #-}
--------------------------------------------------------------------------------
module Sydc.Types
  where
--------------------------------------------------------------------------------
import GHC.Generics
import SydPrelude
--------------------------------------------------------------------------------

newtype Ident = Ident Text
  deriving (Show, Generic)

newtype Namespace = Namespace (List Ident)
  deriving (Show, Generic)

data Name = Qualified Namespace Ident
  deriving (Show, Generic)

data Span = Span
  { }
  deriving (Show, Generic)

-- temp definition.
data SourceCtx = SourceCtx
  deriving (Show, Generic)

