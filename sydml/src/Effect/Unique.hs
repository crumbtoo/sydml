module Effect.Unique
  ( Unique
  , fresh
  , runUnique
  , getUnique
  )
  where
--------------------------------------------------------------------------------
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import SydPrelude
import System.IO.Error (ioeGetFileName)
import qualified Sydc.Name as Name
import qualified Data.Text as T
--------------------------------------------------------------------------------

data Unique :: Effect where
  GetUnique :: Unique m Natural

type instance DispatchOf Unique = Dynamic

getUnique :: (Unique :> es) => Eff es Natural
getUnique = send GetUnique

fresh :: (Unique :> es) => Text -> Eff es Name.Ident
fresh x = (\n -> Name.Ident $ x <> "__" <> T.pack (show n)) <$> getUnique

runUnique :: Eff (Unique ': es) a -> Eff es a
runUnique = reinterpret (evalState (0 :: Natural)) $ const $ \case
  GetUnique -> do
   n <- get @Natural
   modify @Natural (+1)
   pure n
