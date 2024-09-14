module Language.SydML.Syntax
  ( ModuleInfo(..)
  , Qualification(..)
  , Import(..)
  , Decl(..)
  , Module(..)
  )
  where
--------------------------------------------------------------------------------
import SydPrelude
import Sydc.Name qualified as Name
import Language.Shared qualified as Shared
--------------------------------------------------------------------------------

data Qualification = Qualified | Unqualified
  deriving (Show, Eq, Generic)

data Import = Import
  { module_   :: Name.Module
  , qualified :: Qualification
  , as        :: Name.Module
  }
  deriving (Show, Eq, Generic)

data Decl
  deriving (Show, Eq, Generic)

data ModuleInfo = ModuleInfo
  { name    :: Name.Module
  , imports :: List Import
  }
  deriving (Show, Eq, Generic)

data Module = Module
  { info :: ModuleInfo
  , content :: List Decl
  }
  deriving (Show, Eq, Generic)
