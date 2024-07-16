module Sydc.Monad where
--------------------------------------------------------------------------------
import Sydc.Error
import Control.Monad.Except
--------------------------------------------------------------------------------

newtype SydT m a = SydT { runSydT :: ExceptT SydError m a }

