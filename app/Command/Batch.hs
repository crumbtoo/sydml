{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE UnicodeSyntax #-}
module Command.Batch where
--------------------------------------------------------------------------------
import Options.Applicative
import Sydc
import Control.Lens hiding (argument)
import System.IO
import SydPrelude
--------------------------------------------------------------------------------

batchCommand :: Mod CommandFields (IO ())
batchCommand =
    command "batch"
      (info cmd (progDesc "batch-compile a number of files"))

cmd :: Parser (IO ())
cmd = do
  files <- some (argument str (metavar "FILES..."))
  -- -XApplicativeDo moment.
  pure $
    let opts = SydBatchOptions
          { files = files
          }
    in doBatch opts

doBatch :: SydBatchOptions -> IO ()
doBatch opts =
  forOf_ (#files . each) opts \fp ->
    hPutStrLn stderr $ "pretending to compile: " <> fp
