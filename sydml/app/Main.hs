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
  debugFlags <- parseDebugFlags
  buildDir <- parseBuildDir
  sourceDirectories <- parseSourceDirs
  pure $ SydOptions
    { debugFlags = debugFlags
    , buildDir = buildDir
    , sourceDirectories = sourceDirectories
    }

parseSourceDirs :: Parser (HS.HashSet FilePath)
parseSourceDirs =
  foldMap HS.singleton
  <$> many (option str
             (  long "source-dir"
             <> metavar "DIRECTORY"
             <> help "Compile SydML files in DIRECTORY" ))

parseBuildDir :: Parser FilePath
parseBuildDir =
  option str
    (  long "build-dir"
    <> metavar "DIRECTORY"
    <> value "sydc-build"
    <> help "Litter DIRECTORY with build artifacts" )

parseDebugFlags :: Parser DebugFlags
parseDebugFlags =
  foldMap (DebugFlags . HS.singleton)
  <$> many (option str
             (  short 'd'
             <> metavar "FLAG-NAME"
             <> help "Enable debug flag FLAG-NAME" ))

main :: IO ()
main = getArgs >>= main'

main' :: List String -> IO ()
main' = join . handleParseResult . execParserPure (prefs mempty) parser
