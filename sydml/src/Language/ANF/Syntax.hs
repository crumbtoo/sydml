{-# LANGUAGE UndecidableInstances #-}
module Language.ANF.Syntax
  -- * Syntax tree
  ( Module(..)
  , TermF(..)
  -- ** Convenience
  , LocatedTerm

  -- * ToANF pass
  , ToANF
  )
  where
--------------------------------------------------------------------------------
import SydPrelude
import Control.Lens
import Data.Data
import Sydc.Name (Ident, UniqueName)
import qualified Sydc.Name as Name
import Language.Common
import Data.Located (SrcSpan)
import Control.Comonad.Cofree
import Data.Kind (Constraint)
import Data.Deriving
--------------------------------------------------------------------------------
-- ANF AST

-- | A term in A-Normalised Form.
data TermF e
  -- | @let r = f x y z in m@
  = LetApp UniqueName Id (List1 Value) e
  -- | @let f = \x y z -> n in m
  | LetLam UniqueName (List1 UniqueName) e e
  -- | @let r = add# x y in m@
  -- TODO: replace with an idris-style %foreign / %foreign_impl pragma.
  | LetPrim UniqueName (PrimOp Value) e
  -- | @let r = projₙ x in m@
  | LetProj UniqueName Natural UniqueName e
  -- | @case v of {C x y -> z}@
  | Case Value (List (CaseAlt e))
  -- | A /join point/ @j@ is like a local let-bound continuation, satisfying the
  -- following properties:
  --    * All calls to @j@ are tail calls;
  --    * All applications of @j@ are fully saturated;
  --    * @j@ is not captured in a closure.
  -- These stricter requirements ensure join points never need any allocations.
  --
  -- @let-join j p = n in m@
  | LetJoin UniqueName (List UniqueName) e e
  -- | A tail-call to a join point.
  --
  -- @jump j p@
  | Jump UniqueName (List Value)
  -- | @v@
  | Val Value
  deriving (Functor, Foldable, Traversable, Show, Generic, Data)

type LocatedTerm = Cofree TermF SrcSpan

data CaseAlt e = CaseAlt UniqueName e
  deriving (Functor, Foldable, Traversable, Show, Generic, Data)

data PrimOp a = PrimIntMul a a
              | PrimIntSub a a
              | PrimIntPrint a
              deriving (Generic, Show, Data)

data ScDef p = ScDef Ident !(PassTerm p) !(PassScDefAnnotation p)
  deriving (Generic)

data Module p = Module
  { info :: ModuleInfo p
  , scDefs :: List (ScDef p)
  , dataDefs :: List (DataDef p)
  }
  deriving (Generic)

type family PassTerm p :: Type
type family PassScDefAnnotation p :: Type

type ConstrainPassWith :: (Type -> Constraint) -> Type -> Constraint
type ConstrainPassWith (c :: Type -> Constraint) p =
  ( c (PassTerm p)
  , c (PassScDefAnnotation p)
  )

deriving instance (ConstrainPassWith Data p, Data p) => Data (ScDef p)
deriving instance (ConstrainPassWith Show p) => Show (ScDef p)
deriving instance (ConstrainPassWith Eq p) => Eq (ScDef p)

deriving instance (ConstrainPassWith Show p) => Show (Module p)
deriving instance (ConstrainPassWith Eq p) => Eq (Module p)
deriving instance (ConstrainPassWith Data p, Data p) => Data (Module p)

--------------------------------------------------------------------------------
-- The ToANF pass

-- | Identifier for the pass which lowers the Core language to ANF.
--   TODO: Document the pass parameter, and link to it here.
data ToANF

type instance PassTerm            ToANF = LocatedTerm
type instance PassScDefAnnotation ToANF = Void

--------------------------------------------------------------------------------
-- The ClosureConvert pass

-- | Identifier for the pass performing closure-conversion on the ANF IL.
--   TODO: Document the pass parameter, and link to it here.
data ClosureConvert

-- TODO: move to Language.ANF.ClosureConvert

type instance PassTerm            LambdaLift = LocatedTerm
type instance PassScDefAnnotation LambdaLift = Void

--------------------------------------------------------------------------------
-- The LambdaLift pass

-- | Identifier for the pass which lifts nested lambdas to the top-level, and
--   join points to the top-level lambda to which they belong.
--   TODO: Document the pass parameter, and link to it here.
data LambdaLift

-- TODO: move to Language.ANF.LambdaLift

type instance PassTerm LambdaLift = LocatedTerm
-- type instance PassScDefAnnotation LambdaLift = List Join
