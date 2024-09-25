{-# LANGUAGE UndecidableInstances #-}
module Language.SydML.Syntax
  -- * Syntax tree
  ( ModuleInfo(..)
  -- ** Modules
  , Qualification(..)
  , Import(..)
  , Decl(..)
  , Module(..)
  , defaultModule
  -- * Extension points
  , PassGlobal
  , PassVar
  )
  where
--------------------------------------------------------------------------------
import SydPrelude
import Sydc.Name qualified as Name
import Language.Common as Common
import Data.EDN
import Data.Kind
import Data.EDN.AST.Types (Parser)
import Control.Lens
import Data.EDN.Class.Parser (parserError)
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

data Module p = Module
  { info :: ModuleInfo p
  , content :: List (Decl p)
  }
  deriving (Generic)

type ConstrainPassWith :: (Type -> Constraint) -> Type -> Constraint
type ConstrainPassWith c p =
  ( c (PassImports p)
  )

deriving instance ConstrainPassWith Show p => Show (Module p)

-- | Module subtitutive used to recover from unparsable modules.
defaultModule ::Name.Module -> PassImports p -> Module p
defaultModule nm is = Module
  { info = ModuleInfo
    { name = nm
    , imports = is
    }
  , content = []
  }

--------------------------------------------------------------------------------

instance FromEDN (PassImports p) => FromEDN (ModuleInfo p) where
  parseEDNv = _
instance FromEDN (Module p) where
  parseEDNv = _

instance FromEDN (Decl p) where
  parseEDNv = _
