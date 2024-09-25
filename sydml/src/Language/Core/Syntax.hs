{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}
module Language.Core.Syntax
  -- * Syntax tree
  ( Module(..)
  , TermF(..)
  , TypeF(..)

  -- * Extension points
  , PassTerm
  , PassType
  , PassTyAbsFAnnotation
  -- ** Convenience
  , ConstrainPassWith
  , Untyped
  )
  where
--------------------------------------------------------------------------------
import Data.Deriving
import SydPrelude
import Sydc.Name qualified as Name
import Sydc.Name (Ident)
import Data.Located (SrcSpan)
import Control.Comonad.Cofree
import Data.Functor.Classes
import Language.Common
import Data.Kind (Constraint)
--------------------------------------------------------------------------------

type family PassTerm p :: Type
type family PassType p :: Type
type family PassTyAbsFAnnotation p :: Type

type ConstrainPassWith (c :: Type -> Constraint) p =
  ( c (PassTerm p)
  , c (PassType p)
  , c (PassTyAbsFAnnotation p)
  , c (PassImports p)
  )

type Untyped p = (PassType p ~ Void, PassTyAbsFAnnotation p ~ Void)

data TermF p e
  = AbsF Name.UniqueName e
  | TyAbsF !(PassTyAbsFAnnotation p) Name.UniqueName e
  | AppF e e
  | TyAppF e !(PassType p)
  | CaseF e (List (CaseAlt p e))
  | ValF Value
  deriving (Functor, Foldable, Traversable)

data CaseAlt p e = CaseAlt (Pattern p) e
  deriving (Functor, Foldable, Traversable)

data Pattern p
  = ConP Name.Global (List (Pattern p))
  | VarP Name.UniqueName
  | WildcardP
  deriving (Show, Eq, Generic, Data)

data TypeF p t
  = ForallF Name.UniqueName t
  | TyConF Name.Global
  | TyFun
  | TyVarF Name.UniqueName
  | AppTyF t t
  deriving (Show, Eq, Generic, Data)

data ScDef p = ScDef Ident (PassType p) (PassTerm p)
  deriving (Generic)

data Module p = Module
  { info     :: ModuleInfo p
  , scDefs   :: List (ScDef p)
  , dataDefs :: List (DataDef p)
  }
  deriving (Generic)

type LocatedTerm p = Cofree (TermF p) SrcSpan
type LocatedType p = Cofree (TypeF p) SrcSpan

--------------------------------------------------------------------------------
-- Instance hell

pure []

deriving instance (ConstrainPassWith Show p, Show e) => Show (CaseAlt p e)
deriving instance (ConstrainPassWith Eq p, Eq e) => Eq (CaseAlt p e)

deriving instance (ConstrainPassWith Data p, Data p) => Data (ScDef p)
deriving instance (ConstrainPassWith Show p) => Show (ScDef p)
deriving instance (ConstrainPassWith Eq p) => Eq (ScDef p)

instance (ConstrainPassWith Show p) => Show1 (CaseAlt p) where
  liftShowsPrec = $(makeLiftShowsPrec ''CaseAlt)

instance (ConstrainPassWith Show p) => Show1 (TermF p) where
  liftShowsPrec = $(makeLiftShowsPrec ''TermF)

deriving instance (ConstrainPassWith Show p, Show e) => Show (TermF p e)

instance (ConstrainPassWith Show p) => Show1 (TypeF p) where
  liftShowsPrec = $(makeLiftShowsPrec ''TypeF)

deriving instance (ConstrainPassWith Show p) => Show (Module p)
deriving instance (ConstrainPassWith Eq p) => Eq (Module p)

--------------------------------------------------------------------------------
-- ToCore pass

data ToCore

type instance PassTerm ToCore = LocatedTerm ToCore
type instance PassType ToCore = LocatedType ToCore
