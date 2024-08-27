{-# LANGUAGE ApplicativeDo #-}
module Main where
--------------------------------------------------------------------------------
import Options.Applicative
import SydPrelude
import Sydc
import Command.Batch qualified as Batch
--------------------------------------------------------------------------------

data Command = CmdBatch SydBatchOptions

-- parser :: Parser SydOptions
-- parser = subparser . mconcat $
--   [ batchCommand
--   ]

opts :: Parser (IO ())
opts = subparser Batch.batchCommand

main :: IO ()
main = join $ execParser (info opts idm)
