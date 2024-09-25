module Language.SydML.Rename
  ( renameModule
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

data Rename

type instance Surface.PassGlobal Rename = Global
type instance Surface.PassVar Rename = Name.Unique

renameModule
  :: Unique :> es
  => Surface.Module Parse
  -> Eff es (Surface.Module Rename)
renameModule module_ = pure $ Surface.Module
  { Surface.info = renameModuleInfo module_.info
  , Surface.content = []
  }

renameModuleInfo :: Surface.ModuleInfo Parse -> Surface.ModuleInfo Rename
renameModuleInfo (Surface.ModuleInfo nm is) = _
