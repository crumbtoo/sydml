{-# LANGUAGE NoFieldSelectors #-}
--------------------------------------------------------------------------------
module Sydc.Types
  where
--------------------------------------------------------------------------------
import Data.Hashable
import Data.HashSet qualified as HS
import GHC.Generics
import SydPrelude
import Data.IORef
import qualified Data.HashMap.Strict as H
import Effectful
import Effectful.Reader.Static
import Control.Lens
import qualified Data.Text.IO as T
import System.IO
import qualified Data.Text as T
--------------------------------------------------------------------------------

data SydOptions = SydOptions
  { debugFlags :: DebugFlags
  , buildDir :: FilePath
  , sourceDirectories :: HS.HashSet FilePath
  }
  deriving (Show, Generic)

data Command = BatchCmd SydBatchOptions
  deriving (Generic)

data SydBatchOptions = SydBatchOptions
  { sydOptions :: SydOptions
  , files :: List FilePath
  }
  deriving (Generic)

--------------------------------------------------------------------------------

newtype DebugFlags = DebugFlags (H.HashMap Text (Maybe FilePath))
  deriving stock (Show, Generic)
  deriving newtype (Semigroup, Monoid)

type instance Index DebugFlags = Text
type instance IxValue DebugFlags = Maybe FilePath

instance Ixed DebugFlags where
  ix i k (DebugFlags m) =
    case H.lookup i m of
      Just v -> (\v' -> DebugFlags $ H.insert i v' m) <$> k v
      Nothing -> pure (DebugFlags m)

instance At DebugFlags where
  at k sbt (DebugFlags m) = sbt (H.lookup k m) <&> \case
    Just v -> DebugFlags $ H.insert k v m
    Nothing -> DebugFlags $ H.delete k m

linedText :: Applicative f => IndexedLensLike' Int f Text Text
linedText f
  = fmap (T.intercalate "\n") . conjoined traverse (indexing traverse) f
  . T.lines

printToDebugFlag :: (IOE :> es, Reader SydOptions :> es) => Text -> Text -> Eff es ()
printToDebugFlag flagName s =
  asks @SydOptions (view $ #debugFlags . at flagName) >>= \case
    Nothing        -> pure ()
    Just Nothing   -> liftIO $ T.hPutStrLn stderr $
                        s & linedText <>:~ ("-d" <> flagName <> "> ")
    Just (Just fp) -> liftIO $ T.appendFile fp s
