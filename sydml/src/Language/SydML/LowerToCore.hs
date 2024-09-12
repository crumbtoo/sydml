-- | AKA Elaborate.
module Language.SydML.LowerToCore
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

lowerToCoreF :: _
lowerToCoreF = _

lowerToCore :: (Unique :> es) => Surface.Module -> Eff es Core.Module
lowerToCore = _
