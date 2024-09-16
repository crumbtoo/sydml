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
--------------------------------------------------------------------------------

batchCompile :: SydBatchOptions -> IO ()
batchCompile batchOpts = do
    srcDirs <- batchOpts
             & foldMapOf (#files . each) (fmap HS.singleton . makeAbsolute)
    let opts = batchOpts.sydOptions
             & #sourceDirectories <>~ srcDirs
    Driver.runSydTask opts (Driver.compile opts)

rev :: List a -> Cont (List a) (List a)
rev []     = pure []
rev (x:xs) = callCC \k -> (++[x]) <$> rev xs

-- withCompiledExecutable :: SydOptions -> (FilePath -> IO ()) -> IO ()
-- withCompiledExecutable opts k = _
