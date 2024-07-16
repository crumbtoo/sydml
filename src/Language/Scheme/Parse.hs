module Language.Scheme.Parse
  ( Language.Scheme.Parse.parse
  )
  where
--------------------------------------------------------------------------------
import Data.SCargot.Common
import Data.SCargot.Atom
import Data.SCargot.Comments
import Data.SCargot.Parse
import Data.SCargot.Repr.Basic
import Text.Parsec
import Text.Parsec.Text (Parser)
import Language.Scheme.Syntax
import Data.Text qualified as T
import Sydc
import SydPrelude
--------------------------------------------------------------------------------

parseR7RSString :: Parser Text
parseR7RSString = T.pack <$> (  char '"'
                             *> (concat <$> many strElem)
                             <* char '"')
  where
    strElem = normal
          <|> mnemonic
          <|> (pure <$> (char '\\' *> oneOf ['\\', '"']))
          <|> wsEscape
          <|> hexEscape
    normal = pure <$> noneOf ['\\', '"']
    mnemonic = char '\\' *> choice
      [ char 'a' $> "\a"
      , char 'b' $> "\b"
      , char 't' $> "\t"
      , char 'n' $> "\n"
      , char 'r' $> "\r" ]
    wsEscape = char '\\' *> many intralineWs *> lineEnd *> many intralineWs
             $> ""
    intralineWs = oneOf [' ', '\t']
    lineEnd = choice [string "\n", string "\r\n", string "\r"]
    hexEscape = _ -- TODO

parseR7RSBool :: Parser Bool
parseR7RSBool = (string "#t" *> optional (string "rue") $> True)
            <|> (string "#f" *> optional (string "alse") $> False)

atomParser :: SExprParser Atom Sexp
atomParser = atoms
           & addReader '\'' (quoter "quote")
           & addReader '`'  (quoter "quasiquote")
           & addReader ','  (quoter "unquote")
           & withLispComments
  where
    quoter s = fmap $ \x -> SCons (A $ ASymbol s) (SCons x SNil)
    atoms = mkAtomParser
      [ atom ASymbol parseR7RSIdent
      , atom AInt    signedDecNumber
      , atom AString parseR7RSString
      , atom ABool   parseR7RSBool
      ]

parse :: Text -> Either SydError (List Sexp)
parse = first (minimalError ParseError . T.pack) . decode atomParser

