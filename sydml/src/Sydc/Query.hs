{-# LANGUAGE TemplateHaskell #-}
module Sydc.Query
  ( Query(..)
  )
  where
--------------------------------------------------------------------------------
import Language.SydML.Syntax qualified as Surface
import Language.SydML.Parse qualified as Surface
import Sydc.Name qualified as Name
import Data.Hashable
import Data.Some
import Data.GADT.Compare.TH (deriveGEq)
import Data.GADT.Show.TH (deriveGShow)
import Rock                           qualified
import Data.Text.Lazy                   qualified as Lazy
import SydPrelude
import qualified Language.QBE as QBE
import Language.ANF qualified as ANF
--------------------------------------------------------------------------------

data Query a where
  FileText :: FilePath -> Query Text
  ParsedFile :: FilePath -> Query (Surface.Module Surface.Parse)
  ModuleFile :: Name.Module -> Query (Maybe FilePath)
  QBEOfModule :: Name.Module -> Query QBE.Program
  ANFOfModule :: Name.Module -> Query (ANF.Module ANF.ToANF)

deriveGEq ''Query
deriveGShow ''Query

deriving instance Eq (Query a)
deriving instance Show (Query a)

instance Hashable (Query a) where
  hashWithSalt salt = \case
      FileText fp   -> h 0 fp
      ParsedFile fp -> h 1 fp
      ModuleFile nm -> h 2 nm
    where
      h :: Hashable b => Int -> b -> Int
      h tag payload =
        hash tag `hashWithSalt` payload `hashWithSalt` salt

instance Hashable (Some Query) where
  hashWithSalt salt (Some query) = hashWithSalt salt query

