module Sydc.Command.Batch
  ( batchCompile
  )
  where
--------------------------------------------------------------------------------
import Sydc.Types
import Control.Monad.Cont
import SydPrelude
import qualified Sydc.Driver as Driver
import qualified Rock
import Control.Lens
import qualified Data.HashSet as HS
import Data.Hashable (Hashable)
import System.Directory
import System.FilePath (takeDirectory)
import Effectful
--------------------------------------------------------------------------------

batchCompile :: SydBatchOptions -> IO ()
batchCompile batchOpts = do
  srcDirs <- batchOpts
           & foldMapOf (#files . each)
               (fmap HS.singleton . sourceDirectoryOfFile)
  let opts = batchOpts.sydOptions
           & #sourceDirectories <>~ srcDirs
  (_,es) <- runEff $ Driver.runSydTask opts (Driver.compileFiles batchOpts.files)
  for_ es \e ->
    putStrLn $ "error: " <> show e
  pure ()

sourceDirectoryOfFile :: FilePath -> IO FilePath
sourceDirectoryOfFile = fmap takeDirectory . makeAbsolute

rev :: List a -> Cont (List a) (List a)
rev []     = pure []
rev (x:xs) = callCC \k -> (++[x]) <$> rev xs

-- withCompiledExecutable :: SydOptions -> (FilePath -> IO ()) -> IO ()
-- withCompiledExecutable opts k = _
