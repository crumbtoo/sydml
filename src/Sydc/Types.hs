{-# LANGUAGE NoFieldSelectors #-}
--------------------------------------------------------------------------------
module Sydc.Types
  where
--------------------------------------------------------------------------------
import SydPrelude
--------------------------------------------------------------------------------

newtype Ident = Ident Text

newtype Namespace = Namespace (List Ident)

data Name = Qualified Namespace Ident

data ErrorKind = ParseError
               | TypeError
               | InternalError

data Span = Span
  { }

data SydError = SydError
  { modulePath :: Maybe Namespace
  , kind       :: ErrorKind
  , span       :: Maybe Span
  , text       :: ErrorText
  , sourceCtx  :: Maybe SourceCtx
  }

