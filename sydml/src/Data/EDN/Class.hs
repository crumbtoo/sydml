module Data.EDN.Class
  ( FromEDN(..)
  , ToEDN(..)
  , decodeOne
  , decodeMany
  )
  where
--------------------------------------------------------------------------------
import Data.EDN.ParseFromEDN
import Data.EDN.Read
import SydPrelude
import Data.EDN.Syntax
import Text.Megaparsec
import Text.Megaparsec.Internal
import Sydc.Error
import Control.Lens
import qualified Data.Text as T
import Data.Located (bogusSrcSpan)
import Sydc.Name
import qualified Data.List.NonEmpty as List1
--------------------------------------------------------------------------------

class FromEDN a where
  fromEDN :: ParseFromEDN a

class ToEDN a where
  toEDN :: a -> Value

-- TODO:
errorBundleToSydErrors
  :: (Show (Token s))
  => ParseErrorBundle s Void
  -> List1 SydError
errorBundleToSydErrors = fmap f . bundleErrors
  where
    f (TrivialError _ us es)
        = minimalError ParseError (ErrText t)
        & #srcSpan ?~ bogusSrcSpan
      where
        t = T.unlines [ "unexpected: " <> T.pack (show us)
                      , "expected: " <> T.pack (show es)
                      ]

decodeOne :: FromEDN a => FilePath -> Text -> Either (List1 SydError) a
decodeOne fp s =
  first errorBundleToSydErrors (readOne fp s)
  >>= first errorBundleToSydErrors . parseFromEDN fromEDN fp . (:[])

decodeMany :: FromEDN a => FilePath -> Text -> Either (List1 SydError) a
decodeMany fp s =
  first errorBundleToSydErrors (readMany fp s)
  >>= first errorBundleToSydErrors . parseFromEDN fromEDN fp

--------------------------------------------------------------------------------

instance FromEDN Namespace where
  -- TODO: allow more than one namepsace.
  fromEDN = anySymbol >>= \case
    (ss, "", s) -> pure . Namespace . List1.singleton . Ident $ s

instance FromEDN a => FromEDN (List a) where
  fromEDN = list (const $ many fromEDN)
