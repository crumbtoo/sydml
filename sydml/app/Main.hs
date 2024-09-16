{-# LANGUAGE ApplicativeDo #-}
module Main where
--------------------------------------------------------------------------------
import Data.HashSet qualified as HS
import Options.Applicative
import SydPrelude
import Sydc
import Command.Batch qualified as Batch
import System.Environment (getArgs)
import System.Posix.Internals (newFilePath)
--------------------------------------------------------------------------------

-- parser :: Parser SydOptions
-- parser = subparser . mconcat $
--   [ batchCommand
--   ]

parser :: ParserInfo (IO ())
parser = info (helper <*> opts) idm
  where
    opts = commonOpts <**> subparser Batch.batchCommand

commonOpts :: Parser SydOptions
commonOpts = do
  debugFlags <- pure mempty
  buildDir <- pure "./build"
  sourceDirectories <- pure mempty
  pure $ SydOptions
    { debugFlags = debugFlags
    , buildDir = buildDir
    , sourceDirectories = sourceDirectories
    }

main :: IO ()
main = getArgs >>= main'

main' :: List String -> IO ()
main' = join . handleParseResult . execParserPure (prefs mempty) parser
