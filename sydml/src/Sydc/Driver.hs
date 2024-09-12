module Sydc.Driver
  ( rules
  , runSydTask
  )
  where
--------------------------------------------------------------------------------
import Data.Text                          qualified as T
import Data.Text.IO                       qualified as T
import Data.Text.Lazy                     qualified as Lazy
import Data.Text.Lazy.IO                  qualified as Lazy
import Sydc.Query
import Rock
-- import Language.SystemF                   qualified as SystemF
-- import Control.Monad.Trans.Writer.CPS     (runWriterT)
import Control.Monad.Writer.CPS     (runWriter)
import Sydc.Monad
import SydPrelude
import Sydc (SydOptions)
import Language.SydML.Parse (parseSydML)
import Data.IORef
import Control.Concurrent
import Data.HashMap.Strict (HashMap)
import Control.Monad
import qualified Data.Dependent.HashMap as DHashMap
import Data.Functor.Const
import Data.Dependent.HashMap (DHashMap)
--------------------------------------------------------------------------------

rules :: SydOptions -> GenRules (Writer (List SydError) Query) Query
rules opts (Writer query) = case query of
    FileText fp -> input . liftIO . T.readFile $ fp
    ParsedFile fp -> do
      s <- fetch (FileText fp)
      liftIO (parseSydML fp s) >>= \case
        Right m -> input $ pure m
        _ -> _
    ModuleFile nm -> todo "look for filepath of module on opts.sourecDirs"
    -- SystemF_ParsedText s -> _
    -- SystemF_ParsedFile fp -> do
    --     s <- fetch (FileText fp)
    --     let (es,maybeMod) = evalSyd $ SystemF.parseModule fp s
    --     pure $ maybe (defaultMod,es) (,es) maybeMod
    --   where
    --     defaultMod = SystemF.Module
    --       { SystemF.name = SystemF.namespaceFromFilepath fp
    --       , SystemF.imports = []
    --       , SystemF.items = []
    --       }
  where
    input :: Functor m => m a -> m (a, List SydError)
    input = fmap (,mempty)

-- rules (FileText fp) = liftIO (T.readFile fp)
-- rules (SystemF_ParsedText s) = pure $ SystemF.parse s

runSydTask :: SydOptions -> Task Query a -> IO a
runSydTask opts task = do
  startedVar <- newIORef mempty
  errorsVar <- newIORef (mempty :: DHashMap Query (Const (List SydError)))
  depsVar <- newIORef (mempty :: HashMap ThreadId ThreadId)
  let writeErrors :: Query a -> List SydError -> Task Query ()
      writeErrors q errs =
        unless (null errs) $
          liftIO . atomicModifyIORef' errorsVar $
            (,()) . DHashMap.insert q (Const errs)
  let rules' :: Rules Query
      rules' =
        memoiseWithCycleDetection startedVar depsVar $
          writer writeErrors $
            rules opts
  Rock.runTask rules' task
