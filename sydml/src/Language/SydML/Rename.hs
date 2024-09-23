module Language.SydML.Rename
  ( renameModule
  , Renamed
  )
  where
--------------------------------------------------------------------------------
import qualified Language.SydML.Syntax as Surface
import SydPrelude
import Effect.Unique
import Language.SydML.Parse (Parse)
import Sydc.Name (Global)
import qualified Sydc.Name as Name
--------------------------------------------------------------------------------

data Renamed

type instance Surface.PassGlobal Renamed = Global
type instance Surface.PassVar Renamed = Name.Unique

renameModule
  :: Unique :> es
  => Surface.Module Parse
  -> Eff es (Surface.Module Renamed)
renameModule module_ = pure $ Surface.Module
  { Surface.info = renameModuleInfo module_.info
  , Surface.content = []
  }

renameModuleInfo :: Surface.ModuleInfo Parse -> Surface.ModuleInfo Renamed
renameModuleInfo (Surface.ModuleInfo nm is) = _
