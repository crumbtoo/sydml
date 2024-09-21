module Sydc.Name where
--------------------------------------------------------------------------------
import SydPrelude
import Data.Foldable (foldrM)
import System.FilePath
import qualified Data.List.NonEmpty as List1
import qualified Data.Text as T
import Data.Hashable (Hashable)
import Data.Located (SrcSpan)
import Effect.Unique qualified
import Effect.Unique (getUnique)
--------------------------------------------------------------------------------

data Qualified a = Qualified Namespace a
  deriving (Show, Eq, Generic, Data)

newtype Ident = Ident Text
  deriving (Show, Eq, Generic, Data)

type Global = Qualified Ident

instance Hashable Ident

newtype Namespace = Namespace (List1 Ident)
  deriving (Show, Eq, Generic, Data)

instance Hashable Namespace

type Module = Namespace

filePathModule :: FilePath -> Maybe Module
filePathModule
  = fmap (Namespace . fmap (Ident . T.pack))
  . List1.nonEmpty . splitDirectories

data Unique = Unique
  { original :: !Text
  , unique :: !Natural
  , definedAt :: !(Maybe SrcSpan)
  }
  deriving (Data, Show, Generic, Eq)

{-# DEPRECATED #-}
type UniqueName = Unique

fresh :: (Effect.Unique.Unique :> es) => Text -> Eff es UniqueName
fresh x = getUnique <&> \n ->
  Unique
    { original = x
    , unique = n
    , definedAt = Nothing
    }
