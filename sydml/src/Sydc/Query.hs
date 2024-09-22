{-# LANGUAGE TemplateHaskell #-}
module Sydc.Query
  ( Query(..)
  , RockEffects
  )
  where
--------------------------------------------------------------------------------
import Language.SydML.Syntax qualified as Surface
import Language.SydML.Parse qualified as Surface
import Sydc.Name qualified as Name
import Data.Hashable
import Data.Some
import Data.GADT.Compare.TH (deriveGEq, deriveGCompare)
import Data.GADT.Show.TH (deriveGShow)
-- import Rock                           qualified
import Effect.Rock
import Data.Text.Lazy                   qualified as Lazy
import SydPrelude
import qualified Language.QBE as QBE
import Language.ANF qualified as ANF
import Effectful
import Data.Data
import Effect.Unique (Unique, runUnique)
import Data.Hashable.Generic
import Sydc.Error
import Data.Monoid
import Effectful.Writer.Static.Shared (Writer, runWriter)
import Sydc (SydOptions)
import Effectful.Reader.Static
-- import Generics.Kind.Derive.Hashable
-- import Generics.Kind.TH (deriveGenericK)
--------------------------------------------------------------------------------

-- -- | Convenience synonym to cons all 'RockEffects' onto a list of effects.
-- type ConsRockEffects :: List Effect -> List Effect
-- type ConsRockEffects es =
--   Reader SydOptions
--   ': Writer (Dual (List SydError))
--   ': Unique
--   ': Rock Query
--   ': IOE
--   ': es

type RockEffects =
  [ Reader SydOptions
  , Writer (Dual (List SydError))
  , Unique
  , Rock Query
  , IOE
  ]

runRockEffects opts = runReader opts . runWriter . runUnique

data Query es a where
  FileText :: FilePath -> Query RockEffects Text
  ParsedFile :: FilePath -> Query RockEffects (Surface.Module Surface.Parse)
  ModuleFile :: Name.Module -> Query RockEffects (Maybe FilePath)
  QBEOfModule :: Name.Module -> Query RockEffects QBE.Program
  ANFOfModule :: Name.Module -> Query RockEffects (ANF.Module ANF.ToANF)

deriving instance Eq (Query es a)
deriving instance Typeable (Query es a)
deriving instance Show (Query es a)

deriveGEq ''Query
deriveGCompare ''Query
deriveGShow ''Query

instance Hashable (Query es a) where
  hashWithSalt salt = \case
      FileText fp    -> h 0 fp
      ParsedFile fp  -> h 1 fp
      ModuleFile nm  -> h 2 nm
      QBEOfModule nm -> h 3 nm
      ANFOfModule nm -> h 4 nm
    where
      h :: Hashable b => Int -> b -> Int
      h tag payload =
        hash tag `hashWithSalt` payload `hashWithSalt` salt

-- instance Hashable (Some Query) where
--   hashWithSalt salt (Some query) = hashWithSalt salt query
