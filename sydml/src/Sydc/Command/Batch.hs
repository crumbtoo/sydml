module Sydc.Command.Batch
  (
  )
  where
--------------------------------------------------------------------------------
import Sydc.Types
import Control.Monad.Cont
import SydPrelude
--------------------------------------------------------------------------------

compile :: SydOptions -> IO ()
compile opts = runContT (withCompiledExecutable opts) (const $ pure ())

-- compile = flip withCompiledExecutable $ const $ pure ()

withCompiledExecutable :: SydOptions -> ContT () IO FilePath
withCompiledExecutable = _

rev :: List a -> Cont (List a) (List a)
rev []     = pure []
rev (x:xs) = callCC \k -> (++[x]) <$> rev xs

-- withCompiledExecutable :: SydOptions -> (FilePath -> IO ()) -> IO ()
-- withCompiledExecutable = _
