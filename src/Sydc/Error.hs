module Sydc.Error where
--------------------------------------------------------------------------------
import GHC.Generics
import Sydc.Types
import SydPrelude
--------------------------------------------------------------------------------

-- temp definition.
type ErrorText = Text

data ErrorKind = ParseError
               | TypeError
               | InternalError
               deriving (Show, Generic)

data SydError = SydError
  { modulePath :: Maybe Namespace
  , kind       :: ErrorKind
  , span       :: Maybe Span
  , text       :: ErrorText
  , sourceCtx  :: Maybe SourceCtx
  }
  deriving (Show, Generic)

minimalError :: ErrorKind -> ErrorText -> SydError
minimalError k t = SydError
  { modulePath = Nothing
  , kind = k
  , span = Nothing
  , text = t
  , sourceCtx = Nothing
  }

