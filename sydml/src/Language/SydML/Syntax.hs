module Language.SydML.Syntax
  -- * Syntax tree
  ( ModuleInfo(..)
  -- ** Modules
  , Qualification(..)
  , Import(..)
  , Decl(..)
  , Module(..)
  -- * Extension points
  , PassGlobal
  , PassVar
  )
  where
--------------------------------------------------------------------------------
import SydPrelude
import Sydc.Name qualified as Name
import Language.Common qualified as Shared
--------------------------------------------------------------------------------

type family PassGlobal p :: Type
type family PassVar p :: Type

data Decl p
  deriving (Show, Eq, Generic, Data)

data Qualification = Qualified | Unqualified
  deriving (Show, Eq, Generic, Data)

data Import = Import
  { module_   :: Name.Module
  , qualified :: Qualification
  , as        :: Name.Module
  }
  deriving (Show, Eq, Generic, Data)

data ModuleInfo = ModuleInfo
  { name    :: Name.Module
  , imports :: List Import
  }
  deriving (Show, Eq, Generic, Data)

data Module p = Module
  { info :: ModuleInfo
  , content :: List (Decl p)
  }
  deriving (Generic)
