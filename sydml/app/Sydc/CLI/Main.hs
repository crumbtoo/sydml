{-# LANGUAGE ApplicativeDo #-}
module Sydc.CLI.Main where
--------------------------------------------------------------------------------
import Data.HashSet qualified as HS
import Options.Applicative
import SydPrelude
import Sydc
import Sydc.CLI.Command.Batch qualified as Batch
import System.Environment (getArgs)
import System.Posix.Internals (newFilePath)
import Data.List.Extra
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
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
    foldMap (DebugFlags . uncurry H.singleton)
    <$> many (option keyWithOptionalFlag
              (  short 'd'
              <> metavar "FLAG-NAME[=OUTFILE]"
              <> help "Enable debug flag FLAG-NAME, \
                      \and output to OUTFILE or stderr" ))
  where
    keyWithOptionalFlag =
      eitherReader \s ->
        case splitOn "=" s of
          [flag, outPath] -> Right (T.pack flag, Just outPath)
          [flag]          -> Right (T.pack flag, Nothing)
          _               -> Left "expected debug flag form of flag=outputPath"

main :: IO ()
main = getArgs >>= main'

main' :: List String -> IO ()
main' = join . handleParseResult . execParserPure (prefs mempty) parser
