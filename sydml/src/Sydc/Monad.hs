module Sydc.Monad
  ( SydT(..), SydIO, Syd
  , unSydT
  , evalSyd
  , pattern Syd
  , addError
  -- , evalSyd
  , module Sydc.Error
  , throwError
  )
  where
--------------------------------------------------------------------------------
import Data.Tuple
import Data.Functor.Identity
import Control.Monad.Trans
import Sydc.Error
import Control.Monad.Except
import Control.Monad.Writer.CPS
import SydPrelude
--------------------------------------------------------------------------------

newtype SydT m a = SydT { unSydT :: WriterT (List SydError) m a }
  deriving (Functor, Applicative, Monad)
  deriving (MonadTrans, MonadIO)

addError :: Monad m => SydError -> SydT m ()
addError err = SydT (tell [err])

unSydT (SydT x) = x

type SydIO = SydT IO
type Syd = SydT Identity

pattern Syd :: Writer (List SydError) a -> Syd a
pattern Syd w = (SydT w)

evalSyd :: Syd a -> (List SydError, a)
evalSyd (Syd w) = swap $ runWriter w

-- evalSyd :: Syd a -> Either SydError a
-- evalSyd syd = runExcept (unSydT syd)

-- liftEither :: Applicative m => Either SydError a -> SydT m a
-- liftEither = SydT . ExceptT . pure

