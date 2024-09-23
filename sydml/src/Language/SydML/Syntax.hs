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
import Data.EDN.Class
import Data.Kind
import Control.Lens
import Data.EDN.ParseFromEDN
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
  , as        :: Maybe Name.Module
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

-- | Module substitute used to recover from unparsable modules.
defaultModule :: Name.Module -> PassImports p -> Module p
defaultModule nm is = Module
  { info = ModuleInfo
    { name = nm
    , imports = is
    }
  , content = []
  }

--------------------------------------------------------------------------------

instance FromEDN Import where
  fromEDN = label "import" . list . const $ do
    module_ <- fromEDN
    qualified <- keywordFlag Unqualified Qualified "qualified"
    as <- keywordArgument "as" fromEDN
    pure $ Import module_ qualified as

instance (FromEDN (ModuleInfo p), FromEDN (Decl p))
      => FromEDN (Module p)
      where
  fromEDN = do
    info <- fromEDN
    content <- many fromEDN
    pure $ Module
      { info = info
      , content = content
      }

instance FromEDN (Decl p) where
  -- TODO:
  fromEDN = empty
