{-# LANGUAGE NoFieldSelectors, ApplicativeDo, OverloadedRecordDot #-}
module Main where
--------------------------------------------------------------------------------
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
--------------------------------------------------------------------------------

data Opts = Opts
  { asm :: Bool
  , output :: FilePath
  , input :: FilePath
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
      pure $ Opts
        { asm = asm
        , output = output
        , input = input
        }

compile :: Opts -> Lam.Term -> IO ()
compile opts e = do
  let asmFile = if opts.asm then opts.output else opts.output ++ ".s"
      qbeFile = opts.output ++ ".qbe"
  writePipeline qbeFile e
  _ <- createProcess . shell $ printf "qbe %s -o %s" qbeFile asmFile
  unless opts.asm $ do
    _ <- createProcess . shell $ printf "gcc %s -o %s" asmFile opts.output
    pure ()

main :: IO ()
main = do
  opts <- execParser parser
  print opts
  me <- parseLam <$> T.readFile opts.input
  case me of
    Right e -> compile opts e
    Left err -> T.hPutStrLn stderr err
