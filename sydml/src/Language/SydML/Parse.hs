module Language.SydML.Parse
  ( parseSydML

  -- * Pass
  , Parse
  )
  where
--------------------------------------------------------------------------------
import Data.Located
import SydPrelude
import Prettyprinter
import Control.Lens
import Language.SydML.Syntax as Surface
import qualified Sydc.Name as Name
import qualified Data.List.NonEmpty as List1
import Data.EDN.AST.Parser qualified as EDNP
import Data.EDN qualified as EDN
import Sydc.Error
import Text.Megaparsec
import qualified Data.EDN.Class.Parser as EDN
import qualified Data.Text as T
import qualified Language.Common as Common
--------------------------------------------------------------------------------

data Parse

type instance Surface.PassGlobal Parse = Located Text
type instance Surface.PassVar    Parse = Located Text
type instance Common.PassImports Parse = List (Located Surface.Import)

parseSydML :: FilePath -> Text -> Either SydError (Module Parse)
parseSydML fp s =
  case parse (many EDNP.parseValue) fp s of
     Left e -> Left (parseErrorBundleToSydError e)
     Right [] -> Left $ adornWithSpan ss err
       where
         err = minimalError
             ParseError
             (ErrText "Expected (module ModuleName) form.")
         ss = SrcSpan fp (Position 0 0) (Position 0 0)
     -- TODO: don't stop at the first error.
     Right (x:xs) -> EDN.runParser p err ok
       where
         p = Module <$> info <*> content
         err es s = Left $ minimalError ParseError
           (ErrText $ T.unlines
            [ T.pack (show es)
            , T.pack s])
         ok = Right
         info = EDN.parseEDNv @(ModuleInfo Parse) x
         content = traverse (EDN.parseEDNv @(Decl Parse)) xs

parseErrorBundleToSydError :: ParseErrorBundle Text Void -> SydError
parseErrorBundleToSydError = _

--------------------------------------------------------------------------------

sourceFile :: _
sourceFile = _
