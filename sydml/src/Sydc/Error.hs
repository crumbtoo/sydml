module Sydc.Error where
--------------------------------------------------------------------------------
import Control.Lens
import Data.Located
import GHC.Generics
import Sydc.Types
import SydPrelude
--------------------------------------------------------------------------------

-- temp definition.
type ErrorText = Text

data ErrorKind = ParseError
               | TypeError
               | UnboundIdentifierError
               | InternalError
               deriving (Show, Generic)

data SydError = SydError
  { modulePath :: Maybe Namespace
  , kind       :: ErrorKind
  , srcSpan    :: Maybe SrcSpan
  , text       :: ErrorText
  , sourceCtx  :: Maybe SourceCtx
  }
  deriving (Show, Generic)

minimalError :: ErrorKind -> ErrorText -> SydError
minimalError k t = SydError
  { modulePath = Nothing
  , kind = k
  , srcSpan = Nothing
  , text = t
  , sourceCtx = Nothing
  }

adornWithSpan :: SrcSpan -> SydError -> SydError
adornWithSpan ss = #srcSpan %~ maybe (Just ss) Just
