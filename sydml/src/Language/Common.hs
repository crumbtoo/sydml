module Language.Common
  ( Id(..)
  , Value(..)
  , ModuleInfo(..)
  , DataDef(..)
  )
where
--------------------------------------------------------------------------------
import SydPrelude
import Sydc.Name (Ident)
import qualified Sydc.Name as Name
--------------------------------------------------------------------------------

-- if only we had dependent types...
--   type data IdKind
--     = GlobalId
--     | LocalId
--     | JoinId
--     | ConId
--   data Id :: IdKind -> Type

-- | An identifier of some sort.
data Id
  = LocalId
    { unique :: Name.UniqueName }
  | GlobalId
    { global :: Name.Global }
  deriving (Show, Eq, Generic, Data)

-- | A terminal value.
data Value = IntVal Int
           | Var Id
           deriving (Show, Eq, Generic, Data)

data ModuleInfo p = ModuleInfo
  { name    :: Name.Module
  -- all names are resolved; all imports unqualified and unaliased.
  , imports :: List Name.Module
  }
  deriving (Show, Eq, Generic, Data)

data DataDef p
  deriving (Show, Eq, Generic, Data)
