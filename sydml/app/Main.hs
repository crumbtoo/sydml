{-# LANGUAGE ApplicativeDo #-}
module Main where
--------------------------------------------------------------------------------
import Options.Applicative
import SydPrelude
import Sydc
import Command.Batch qualified as Batch
import System.Environment (getArgs)
--------------------------------------------------------------------------------

-- parser :: Parser SydOptions
-- parser = subparser . mconcat $
--   [ batchCommand
--   ]

parser :: ParserInfo (IO ())
parser = info (helper <*> opts) idm
  where
    opts = subparser Batch.batchCommand

main :: IO ()
-- main = join $ execParser (info parser idm)
main = getArgs >>= main'

main' :: List String -> IO ()
main' = join . handleParseResult . execParserPure (prefs mempty) parser
