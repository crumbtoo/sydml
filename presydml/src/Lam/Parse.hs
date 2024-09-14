module Lam.Parse
  ( parseLam
  )
  where
--------------------------------------------------------------------------------
import Data.Text qualified as T
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec
import Control.Applicative hiding (many, some)
import Lam.Syntax
import SydPrelude
import Text.Megaparsec.Char
import qualified Data.HashSet as HS
import Data.Foldable1
import Control.Monad.Combinators.NonEmpty qualified as List1
--------------------------------------------------------------------------------

parseLam :: Text -> Either Text Term
parseLam = first (T.pack . errorBundlePretty) . parse term "<file>"

type P = Parsec Void Text

sc :: P ()
sc = L.space space1 empty empty

lexeme :: P a -> P a
lexeme = L.lexeme sc

symbol :: Text -> P Text
symbol = L.symbol sc

parens :: P a -> P a
parens = between (symbol "(") (symbol ")")

--------------------------------------------------------------------------------

term1 :: P Term
term1 = choice
  [ Var <$> name
  , IntVal <$> int
  , parens term
  ]

term :: P Term
term = choice
  [ ifThenElse
  , lam
  , let_
  , prim
  , app
  ]

prim :: P Term
prim = Prim <$> choice
    [ symbol "int+" *> parseBinOp PrimAdd
    , symbol "int-" *> parseBinOp PrimSub
    , symbol "int*" *> parseBinOp PrimMul
    , symbol "int-print" *> (PrimPrintInt <$> term1)
    ]
  where
    parseBinOp op = op <$> term1 <*> term1

lam :: P Term
lam = Lam <$> (kw *> name)
          <*> (symbol "->" *> term)
  where
    kw :: P Name
    kw = choice $ symbol <$> ["lambda", "λ", "fun", "\\"]

ifThenElse :: P Term
ifThenElse = IfThenElse <$> (symbol "if" *> term)
                        <*> (symbol "then" *> term)
                        <*> (symbol "else" *> term)

let_ :: P Term
let_ = Let <$> (symbol "let" *> name)
           <*> (symbol "=" *> term)
           <*> (symbol "in" *> term)

app :: P Term
app = foldl1' App <$> List1.some term1

int :: P Int
int = lexeme $ L.signed (fail "no ws after sign") L.decimal

keywords :: HS.HashSet Text
keywords = HS.fromList
  [ "λ","if","then","else","let","in","="
  , "int+", "int-", "int*", "int-print"]

-- r7rs-like identifiers.
name :: P Name
name = try $ do
    nm <- lexeme $ T.pack <$> ((:) <$> initial <*> subsequent)
    if nm `HS.member` keywords || T.head nm == 'λ'
      then fail "name is not kw"
      else pure nm
  where
    initial = letterChar <|> specialInitial
    specialInitial = oneOf ("!$%&*/:<=>?^_~" :: List Char)
    subsequent = many $ initial <|> digitChar <|> specialSubsequent
    specialSubsequent = oneOf (".@+-" :: List Char)
