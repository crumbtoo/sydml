module Sydc.Driver
  ( rules
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
--------------------------------------------------------------------------------

fromSyd :: Syd a -> Task Query (a, List SydError)
fromSyd = pure . runWriter . unSydT

rules :: GenRules (Writer (List SydError) Query) Query
rules (Writer query) = case query of
    FileText fp -> input $ liftIO (Lazy.readFile fp)
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

