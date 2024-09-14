{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoFieldSelectors, ApplicativeDo, OverloadedRecordDot #-}
module Main where
--------------------------------------------------------------------------------
import Data.Aeson
import Data.HashMap.Strict qualified as H
import Options.Applicative
import SydPrelude
import System.Environment
import qualified Data.Text.IO as T
import Lam.Parse
import Lam.Syntax qualified as Lam
import SSA
import System.Process
import Control.Monad
import System.IO
import qualified Data.Aeson as Aeson
import qualified Data.Text as T
import Data.Text.Prettyprint.Doc (layoutPretty, defaultLayoutOptions)
import Data.Text.Prettyprint.Doc.Render.Text
import Data.Functor
--------------------------------------------------------------------------------

data Opts = Opts
  { asm :: Bool
  , output :: FilePath
  , input :: FilePath
  , dumpPasses :: Bool
  }
  deriving (Show)

parser :: ParserInfo Opts
parser = info (helper <*> opts) idm
  where
    opts :: Parser Opts
    opts = do
      asm <- switch ( short 'S' )
      _ <- switch (short 'g')
      output <- strOption ( short 'o'
                          <> value "t.s"
                          )
      input <- argument str (metavar "FILE")
      dumpPasses <- switch (long "dump-passes")
      pure $ Opts
        { asm = asm
        , output = output
        , input = input
        , dumpPasses = dumpPasses
        }

compile :: Opts -> Lam.Term -> IO ()
compile opts e = do
  let asmFile = if opts.asm then opts.output else opts.output ++ ".s"
      qbeFile = opts.output ++ ".qbe"
  if opts.dumpPasses
    then writePipeline' qbeFile e
    else writePipeline qbeFile e
  _ <- createProcess . shell $ printf "qbe %s -o %s" qbeFile asmFile
  unless opts.asm $ do
    _ <- createProcess . shell $ printf "gcc %s -o %s" asmFile opts.output
    pure ()

main :: IO ()
main = do
  opts <- execParser parser
  -- print opts
  -- putStrLn "{\"coolPassGroup\": [{\"name\": \"cool pass 1\", \"machine\": false, \"after\": [], \"before\": [], \"irChanged\": true}]}"
  me <- parseLam <$> T.readFile opts.input
  case me of
    Right e -> compile opts e
    Left err -> T.hPutStrLn stderr err
