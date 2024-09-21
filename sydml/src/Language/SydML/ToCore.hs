-- | AKA Elaborate.
module Language.SydML.ToCore
  ( lowerToCoreF
  , lowerToCore
  )
  where
--------------------------------------------------------------------------------
import SydPrelude
import qualified Language.SydML.Syntax as Surface
import qualified Language.Core.Syntax as Core
import Effect.Unique
--------------------------------------------------------------------------------

data ToCore

lowerToCoreF :: _
lowerToCoreF = _

lowerToCore :: (Unique :> es) => Surface.Module p -> Eff es (Core.Module ToCore)
lowerToCore = _
