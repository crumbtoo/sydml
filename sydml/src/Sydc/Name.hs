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
import Control.Lens
import Data.Foldable1
import Prelude hiding (foldr1)
import Data.String (IsString)
--------------------------------------------------------------------------------

data Qualified a = Qualified Namespace a
  deriving (Show, Eq, Generic, Data)

newtype Ident = Ident Text
  deriving (Show, Eq, Ord, Generic, Data)
  deriving newtype (IsString)

type Global = Qualified Ident

instance Hashable Ident

newtype Namespace = Namespace (List1 Ident)
  deriving (Show, Eq, Generic, Data, Ord)

instance Hashable Namespace

namespaceComponents :: Traversal1' Namespace Ident
namespaceComponents k (Namespace is) = Namespace <$> traversed1 k is

type Module = Namespace

filePathModule :: FilePath -> Maybe Module
filePathModule
  = fmap (Namespace . fmap (Ident . T.pack))
  . List1.nonEmpty . splitDirectories

moduleFilePath :: Module -> FilePath
moduleFilePath m
    = toNonEmptyOf namespaceComponents m
    & fmap (\(Ident i) -> T.unpack i)
    & foldr1 (</>)
    & (`addExtension` ".sydml")

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
