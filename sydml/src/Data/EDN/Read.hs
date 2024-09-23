module Data.EDN.Read
  ( readOne
  , readMany
  , sourcePosToPosition
  )
  where
--------------------------------------------------------------------------------
import Text.Megaparsec.Char.Lexer qualified as L
import Text.Megaparsec.Char
import Text.Megaparsec
import Text.Megaparsec.Internal
import Data.Located
import Control.Comonad.Cofree
import Data.EDN.Syntax
import SydPrelude
import Data.Functor.Foldable
import qualified Data.Text as T
import Data.Char
import Control.Lens hiding ((:<))
--------------------------------------------------------------------------------

type P = Parsec Void Text

sourcePosToPosition :: SourcePos -> Position
sourcePosToPosition pos = Position (f $ sourceLine pos) (f $ sourceColumn pos)
  where
    f = fromIntegral . unPos

readOne :: FilePath -> Text -> Either (ParseErrorBundle Text Void) LocatedValue
readOne = parse (ws *> value <* lexeme eof)

readMany
  :: FilePath
  -> Text
  -> Either (ParseErrorBundle Text Void) (List LocatedValue)
readMany = parse (ws *> manyTill value (lexeme eof))

sourcePosToSrcSpan :: SourcePos -> SourcePos -> SrcSpan
sourcePosToSrcSpan p1 p2 = SrcSpan
  { file = sourceName p1 -- arbitrary choice; could've been p2
  , start = sourcePosToPosition p1
  , end = sourcePosToPosition p2
  }

fixCofree :: Functor f => Iso (Cofree f a) (Cofree f ()) (Fix f) (Fix f)
fixCofree = iso up down
  where
    up (_ :< as) = Fix (up <$> as)
    down (Fix f) = () :< (down <$> f)

--------------------------------------------------------------------------------
-- EDN

ws :: P ()
ws = L.space space1 lc bc
  where
    lc = L.skipLineComment ";"
    bc = L.skipBlockCommentNested "#|" "|#"

lexeme :: P a -> P a
lexeme = L.lexeme ws

locatedLexeme :: P a -> P (SrcSpan, a)
locatedLexeme p = do
  pos1 <- getSourcePos
  r <- p
  pos2 <- getSourcePos
  ws
  pure (sourcePosToSrcSpan pos1 pos2, r)

word :: Text -> P Text
word = L.symbol ws

wordWithSourcePos :: Text -> P (SourcePos, Text)
wordWithSourcePos s =
  ws *> ((,) <$> getSourcePos <*> string s)

locatedPairToCofree :: (SrcSpan, f (Cofree f SrcSpan)) -> Cofree f SrcSpan
locatedPairToCofree = uncurry (:<)

value :: P LocatedValue
value = choice
  [ list
  , keyword
  , prefixedSymbol
  ]

list :: P LocatedValue
list = do
  (pos1,_) <- wordWithSourcePos "("
  rs <- manyTill value (word ")")
  pos2 <- getSourcePos
  pure $ sourcePosToSrcSpan pos1 pos2 :< ListF rs

keyword :: P LocatedValue
keyword = fmap locatedPairToCofree . locatedLexeme $
  KeywordF <$> (char ':' *> symbol)

prefixedSymbol :: P LocatedValue
prefixedSymbol = fmap locatedPairToCofree . locatedLexeme $ do
  s1 <- symbol
  optional (char '/' *> symbol) <&> \case
    Just s2 -> SymbolF s1 s2
    Nothing -> SymbolF "" s1

symbol :: P Text
symbol = T.cons <$> lead <*> lag
  where
    -- > "Symbols begin with a non-numeric character...
    lead = satisfy \c -> (isAlpha c && not (isDigit c))
                    || c `elem` misc
    -- > ... and can contain alphanumeric characters and . * + ! - _ ? $ % & = <
    -- > > ...
    lag = takeWhileP Nothing \c -> isAlphaNum c || c `elem` misc
    -- TODO: If -, + or . are the first character, the second character (if any)
    -- must be non-numeric."
    misc = ".*+!-_?$&=<>" :: List Char
