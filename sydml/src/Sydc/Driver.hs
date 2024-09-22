module Sydc.Driver
  ( rules
  , runSydTask
  , compile
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
import           Sydc.Monad
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
--------------------------------------------------------------------------------

rules :: Rules Query
rules = \case
    FileText fp -> fileText fp
    ParsedFile fp -> parsedFile fp
    -- ParsedFile fp -> do
    --   s <- fetch (FileText fp)
    --   liftIO (parseSydML fp s) >>= \case
    --     Right m -> input $ pure m
    --     _ -> _
    ModuleFile nm -> todo "look for filepath of module on opts.sourecDirs"

input :: Functor m => m a -> m (a, List SydError)
input = fmap (,mempty)

--------------------------------------------------------------------------------
-- Task implementations

type TaskImpl = Eff RockEffects

parsedFile :: FilePath -> TaskImpl (Surface.Module Surface.Parse)
parsedFile fp = do
  s <- fetch (FileText fp)
  liftIO (parseSydML fp s) >>= \case
    Right m -> pure m
    _ -> _

fileText :: FilePath -> TaskImpl Text
fileText = liftIO . T.readFile

--------------------------------------------------------------------------------

-- runSydTask :: SydOptions ->
runSydTask = _

-- runSydTask :: SydOptions -> Task Query a -> IO a
-- runSydTask opts task = do
--   startedVar <- newIORef mempty
--   errorsVar <- newIORef (mempty :: DHashMap Query (Const (List SydError)))
--   depsVar <- newIORef (mempty :: HashMap ThreadId ThreadId)
--   let writeErrors :: Query a -> List SydError -> Task Query ()
--       writeErrors q errs =
--         unless (null errs) $
--           liftIO . atomicModifyIORef' errorsVar $
--             (,()) . DHashMap.insert q (Const errs)
--   let rules' :: Rules Query
--       rules' =
--         memoiseWithCycleDetection startedVar depsVar $
--           writer writeErrors $
--             rules opts
--   Rock.runTask rules' task

compile = _
-- compile :: SydOptions -> Task Query ()
-- compile opts = do
--   liftIO $ print opts

test :: ( Reader SydOptions :> es
        , IOE :> es
        , Writer (Dual (List SydError)) :> es
        , Unique :> es
        )
     => Eff es Text
test = runRock rules $ fetch $ FileText "sydml.cabal"

test' :: (IOE :> es) => SydOptions -> Eff es (Text, Dual (List SydError))
test' opts = runWriter . runUnique . runReader opts $ test
