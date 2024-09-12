module Sydc.Monad
  ( Syd
  , unSyd
  , evalSydE
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
import Sydc.Types
import Control.Monad.Reader
import Data.Sequence (Seq)
--------------------------------------------------------------------------------

newtype SydE es a = Syd {
    unSydE :: ReaderT SydOptions (WriterT (Seq SydError) (Eff es)) a
  }

addError :: forall es. SydError -> SydE es ()
addError err = Syd (tell . pure $ err)

unSydE (Syd x) = x

type Syd = SydE '[]

unSyd :: Syd a -> (a, List SydError)
unSyd (Syd x) = _

-- type SydIO = SydT IO
-- type Syd = SydT Identity

-- pattern Syd :: Writer (List SydError) a -> Syd a
-- pattern Syd w = (SydT w)

evalSydE :: SydE es a -> Eff es (a, _)
evalSydE (Syd w) = _

-- evalSyd :: Syd a -> Either SydError a
-- evalSyd syd = runExcept (unSydT syd)

-- liftEither :: Applicative m => Either SydError a -> SydT m a
-- liftEither = SydT . ExceptT . pure

