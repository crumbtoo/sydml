module Sydc.Name where
--------------------------------------------------------------------------------
import SydPrelude
import Data.Foldable (foldrM)
import System.FilePath
import qualified Data.List.NonEmpty as List1
import qualified Data.Text as T
import Data.Hashable (Hashable)
--------------------------------------------------------------------------------

newtype Namespace = Namespace (List1 Ident)
  deriving (Show, Eq, Generic)

instance Hashable Namespace

data Qualified a = Qualified Namespace a
  deriving (Show, Eq, Generic)

newtype Ident = Ident Text
  deriving (Show, Eq, Generic)

instance Hashable Ident

newtype Module = Module Namespace
  deriving (Show, Eq, Generic)

instance Hashable Module

filePathModule :: FilePath -> Maybe Module
filePathModule
  = fmap (Module . Namespace . fmap (Ident . T.pack))
  . List1.nonEmpty . splitDirectories
