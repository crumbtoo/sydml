{-# LANGUAGE UndecidableInstances #-}
module Language.Common
  ( Id(..)
  , Value(..)
  , ModuleInfo(..)
  , DataDef(..)
  -- * Extension points
  , PassImports
  )
where
--------------------------------------------------------------------------------
import SydPrelude
import Sydc.Name (Ident)
import qualified Sydc.Name as Name
import Data.EDN.Class
import Data.EDN.ParseFromEDN
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
  , imports :: PassImports p
  }
  deriving (Generic)

deriving instance Show (PassImports p) => Show (ModuleInfo p)
deriving instance (Data (PassImports p), Data p) => Data (ModuleInfo p)
deriving instance Eq (PassImports p) => Eq (ModuleInfo p)

type family PassImports p :: Type

data DataDef p
  deriving (Show, Eq, Generic, Data)

instance (PassImports p ~ ()) => FromEDN (ModuleInfo p) where
  fromEDN = list . const $ do
    symbol "module"
    name <- fromEDN
    pure $ ModuleInfo name ()
