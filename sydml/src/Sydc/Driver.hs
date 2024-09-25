{-# LANGUAGE ImpredicativeTypes #-}
module Sydc.Driver
  ( rules
  , runSydTask
  , compile
  , compileFiles
  )
  where
--------------------------------------------------------------------------------
import Language.SydML.Syntax qualified as Surface
import Data.Text                          qualified as T
import Data.Text.IO                       qualified as T
import Data.Text.Lazy                     qualified as Lazy
import Data.Text.Lazy.IO                  qualified as Lazy
import           Sydc.Query
-- import           Rock
import Effect.Rock
import           SydPrelude
import           Sydc (SydOptions)
import           Language.SydML.Parse (parseSydML)
import           Data.IORef
import           Control.Concurrent
import           Data.HashMap.Strict (HashMap)
import           Control.Monad
import qualified Data.Dependent.HashMap as DHashMap
import           Data.Functor.Const
import           Data.Dependent.HashMap (DHashMap)
import qualified Language.SydML.Parse as Surface
import Effectful
import Effectful.Reader.Static
import Data.Monoid
import Effectful.Writer.Static.Shared
import Effect.Unique
import Control.Lens
import Effect.Rock.Memo
import Sydc.Error
import qualified Sydc.Name as Name
import qualified Data.Sequence as Seq
import Data.Sequence (Seq)
import Text.Pretty.Simple
--------------------------------------------------------------------------------

rules :: Rules Query
rules = \case
    FileText fp -> fileText fp
    ParsedFile fp -> parsedFile fp
    ModuleFile nm -> todo "look for filepath of module on opts.sourecDirs"

input :: Functor m => m a -> m (a, List SydError)
input = fmap (,mempty)

--------------------------------------------------------------------------------
-- Task implementations

type TaskImpl = Eff RockEffects

addError :: Writer (Seq SydError) :> es => SydError -> Eff es ()
addError = tell . Seq.singleton

parsedFile :: FilePath -> TaskImpl (Surface.Module Surface.Parse)
parsedFile fp = do
  s <- fetch (FileText fp)
  case parseSydML fp s of
    Right m -> pure m
    Left e -> addError e $> Surface.defaultModule nm []
      where
        nm = fromMaybe (error "fp mod") $ Name.filePathModule fp

fileText :: FilePath -> TaskImpl Text
fileText = liftIO . T.readFile

--------------------------------------------------------------------------------

runSydTask :: (IOE :> es)
           => SydOptions
           -> Eff (ConsRockEffects es) a
           -> Eff es (a, Seq SydError)
-- runSydTask opts = runWriter . runUnique . runReader opts . runRock rules
runSydTask opts task = do
  startedVar <- liftIO $ newIORef mempty
  -- depsVar <- liftIO $ newIORef mempty
  let rules' :: Rules Query
      rules' = memoise startedVar rules
        -- TODO: add an implicit variant of `memoiseWithCycleDetection`.
        -- memoiseWithCycleDetection startedVar depsVar rules
  task & runRock rules'
       & runReader opts
       & runUnique
       & runWriter

compile :: IOE :> es => Eff (ConsRockEffects es) ()
compile = do
  opts <- ask @SydOptions
  liftIO $ print opts

compileFiles
  :: (Traversable f, IOE :> es)
  => f FilePath
  -> Eff (ConsRockEffects es) ()
compileFiles files = do
  opts <- ask @SydOptions
  liftIO . putStrLn $ "compiling with options: " <> show opts
  for_ files \f -> do
    m <- fetch $ ParsedFile f
    liftIO . T.putStrLn $ "---\nparsed module:\n" <> (pShow m ^. strict)

test :: ( Reader SydOptions :> es
        , IOE :> es
        , Writer (Seq SydError) :> es
        , Unique :> es
        )
     => Eff es Text
test = runRock rules $ fetch $ FileText "sydml.cabal"

test' :: (IOE :> es) => SydOptions -> Eff es (Text, Seq SydError)
test' opts = runWriter . runUnique . runReader opts $ test
