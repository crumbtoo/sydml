{- |
-}
module SydmlTests.Utils
  ( runSydcAndCollectDebugFlag
  )
  where
--------------------------------------------------------------------------------
import qualified Data.ByteString.Lazy as BS
import SydPrelude
import System.IO
import System.Directory
import System.Environment (withArgs)
import System.FilePath
import Data.Text qualified as T
import Sydc.CLI.Main qualified as Sydc
import Control.Monad
--------------------------------------------------------------------------------

-- | Run sydc with some arguments, but add an additional debug flag, whose
--   output will be redirected to a temporary file, read, and returned.
runSydcAndCollectDebugFlag
  -- | The debug flag of interest.
  :: Text
  -> List Text
  -> IO BS.ByteString
runSydcAndCollectDebugFlag flagName args = do
  tmpDir <- getTemporaryDirectory
  let fp = tmpDir </> T.unpack ("sydc-" <> flagName)
  -- sydc will append to the log file file, if it exists — that's bad!
  fpExists <- doesFileExist fp
  when fpExists (removeFile fp)
  let args' = ("-d" <> T.unpack flagName <> "=" <> fp) : (T.unpack <$> args)
  withArgs args' Sydc.main
  BS.readFile fp
