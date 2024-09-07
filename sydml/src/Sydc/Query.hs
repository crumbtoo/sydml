{-# LANGUAGE TemplateHaskell #-}
module Sydc.Query
  ( Query(..)
  )
  where
--------------------------------------------------------------------------------
import Data.Hashable
import Data.Some
import Data.GADT.Compare.TH           (deriveGEq)
import Rock                           qualified
import Data.Text.Lazy                   qualified as Lazy
import SydPrelude
--------------------------------------------------------------------------------

data Query a where
  FileText :: FilePath -> Query Lazy.Text
  -- SystemF_ParsedText :: Lazy.Text -> Query SystemF.ModuleL
  -- SystemF_ParsedFile :: FilePath -> Query SystemF.ModuleL

deriveGEq ''Query

deriving instance Eq (Query a)
deriving instance Show (Query a)

instance Hashable (Query a) where
  hashWithSalt salt = \case
      FileText fp           -> h 0 fp
      -- SystemF_ParsedText s  -> h 1 s
      -- SystemF_ParsedFile fp -> h 2 fp
    where
      h :: Hashable b => Int -> b -> Int
      h tag payload =
        hash tag `hashWithSalt` payload `hashWithSalt` salt

instance Hashable (Some Query) where
  hashWithSalt salt (Some query) = hashWithSalt salt query

