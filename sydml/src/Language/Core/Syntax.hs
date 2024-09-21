module Language.Core.Syntax
  ( Module(..)
  )
  where
--------------------------------------------------------------------------------
import SydPrelude
import Sydc.Name qualified as Name
--------------------------------------------------------------------------------

data Supercombinator = MkSC Name.Ident ()
  deriving (Show, Eq, Generic)

data ModuleInfo = ModuleInfo
  { name    :: Name.Module
  -- all names are resolved; all imports unqualified and unaliased.
  , imports :: List Name.Module
  }
  deriving (Show, Eq, Generic)

data Module = Module
  { info :: ModuleInfo
  , supercombinators :: List Supercombinator
  }
  deriving (Show, Eq, Generic)
