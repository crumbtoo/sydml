module Sydc.Error where
--------------------------------------------------------------------------------
import Control.Lens
import Data.Located
import GHC.Generics
import Sydc.Types
import Sydc.Name qualified as Name
import SydPrelude
--------------------------------------------------------------------------------

data ErrorText = ErrText Text
               deriving (Show, Generic)

data ErrorKind = ParseError
               | TypeError
               | UnboundIdentifierError
               | InternalError
               deriving (Show, Generic)

data SydError = SydError
  { modulePath :: Maybe Name.Module
  , kind       :: ErrorKind
  , srcSpan    :: Maybe SrcSpan
  , text       :: ErrorText
  }
  deriving (Show, Generic)

minimalError :: ErrorKind -> ErrorText -> SydError
minimalError k t = SydError
  { modulePath = Nothing
  , kind = k
  , srcSpan = Nothing
  , text = t
  }

adornWithSpan :: SrcSpan -> SydError -> SydError
adornWithSpan ss = #srcSpan %~ maybe (Just ss) Just
