module SydmlTests.EmptyInput
  ( tests
  )
  where
--------------------------------------------------------------------------------
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden
import qualified Data.ByteString.Lazy as BS
import SydPrelude
import Data.EDN.Class
import Data.Text qualified as T
import qualified Data.Text.Lazy.Encoding as T
import qualified Data.Text.Lazy as T.Lazy
import Data.Proxy
import qualified Language.SydML.Parse as Surface
import qualified Language.SydML.Syntax as Surface
import Text.Pretty.Simple
import System.IO
--------------------------------------------------------------------------------

decodeFile
  :: forall (a :: Type). (Show a, FromEDN a)
  => Proxy a -> FilePath -> IO BS.ByteString
decodeFile _ fp =
  BS.readFile fp
  <&> T.encodeUtf8 . pShowNoColor . decodeOne @a fp . T.Lazy.toStrict . T.decodeUtf8

goldenVsTempFile
  :: TestName
  -- | The golden file.
  -> FilePath
  -- | Write to the output file.
  -> (FilePath -> Handle -> IO ())
  -> TestTree
goldenVsTempFile = _

tests :: TestTree
tests = testGroup "Empty input"
  [ goldenVsString
    "Parse"
    "sydml/golden/Empty module/golden-files/Parse"
    (decodeFile (Proxy @(Surface.Module Surface.Parse)) "sydml/golden/Empty module/Empty.sydml")
  ]
