{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UnicodeSyntax #-}
module Sydc.CLI.Command.Batch where
--------------------------------------------------------------------------------
import Options.Applicative
import Sydc
import Control.Lens hiding (argument)
import System.IO
import SydPrelude
import Sydc.Command.Batch (batchCompile)
--------------------------------------------------------------------------------

batchCommand :: Mod CommandFields (SydOptions -> IO ())
batchCommand =
    command "batch"
      (info cmd (progDesc "batch-compile a number of files"))

cmd :: Parser (SydOptions -> IO ())
cmd = do
  files <- some (argument str (metavar "FILES..."))
  -- -XApplicativeDo moment.
  pure $ \opts ->
    let batchOpts = SydBatchOptions
          { sydOptions = opts
          , inputFiles = files
          }
    in batchCompile batchOpts
