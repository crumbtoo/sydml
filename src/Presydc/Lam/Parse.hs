module Presydc.Lam.Parse
  ( gParseLam
  , value
  )
  where
--------------------------------------------------------------------------------
import Control.Monad.Reader
import Presydc.Lam.Syntax
import Text.Megaparsec
import Text.Megaparsec.Char.Lexer qualified as L
import SydPrelude
import Text.Megaparsec.Char
--------------------------------------------------------------------------------

type P = Parsec Void Text

sc :: P ()
sc = L.space space1 lineComment empty
  where
    lineComment = L.skipLineComment "--"

lexeme :: P a -> P a
lexeme = L.lexeme sc

symbol :: Text -> P Text
symbol = L.symbol sc

--------------------------------------------------------------------------------

gParseLam = _

value = _
