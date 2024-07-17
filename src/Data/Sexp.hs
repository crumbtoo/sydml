{-# LANGUAGE TemplateHaskell #-}
module Data.Sexp
  ( Atom(..)
  , Sexp
  , SExpr(..)
  , SExprF(..)
  , Symbol
  , parseSexp
  , parseSexps
  , printSexp
  -- * Optics
  , _cars
  -- , _SNil
  , _SCons
  , _SAtom
  , _SSymbol
  )
  where
--------------------------------------------------------------------------------
import Control.Lens               hiding (noneOf)
import Data.String                (IsString(..))
import Data.Functor.Foldable
import Data.Functor.Foldable.TH
import Data.Hashable
import Data.Text                  qualified as T
import GHC.Generics

import Data.SCargot.Common
import Data.SCargot.Atom
import Data.SCargot.Comments
import Data.SCargot.Parse
import Data.SCargot.Print
import Data.SCargot.Repr.Basic
import Text.Parsec
import Text.Parsec.Text (Parser)

import SydPrelude
--------------------------------------------------------------------------------

type Symbol = Text

data Atom = ABool Bool
          | ASymbol Symbol
          | AInt Integer
          | AString Text
          deriving (Show, Eq, Generic)

instance Hashable Atom

type Sexp = SExpr Atom

instance IsString Atom where
  fromString = ASymbol . T.pack

makeBaseFunctor ''SExpr

deriving instance Generic a => Generic (SExpr a)

instance Hashable a => Hashable (SExpr a) where
  hashWithSalt s =
    let (^-) a b = hashWithSalt a b
    in cata \case
      SConsF x y -> s ^- (0::Int) ^- x ^- y
      SAtomF a   -> s ^- (1::Int) ^- a
      SNilF      -> s ^- (2::Int)

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

parser :: SExprParser Atom Sexp
parser = atoms
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

parseSexp :: Text -> Either String Sexp
parseSexp = decodeOne parser

parseSexps :: Text -> Either String (List Sexp)
parseSexps = decode parser

printAtom :: Atom -> Text
printAtom (ASymbol s)   = s
printAtom (AInt n)      = T.pack $ show n
printAtom (AString s)   = T.pack $ show s
printAtom (ABool True)  = "#true"
printAtom (ABool False) = "#false"

printer :: SExprPrinter Atom Sexp
printer = basicPrint printAtom

printSexp :: Sexp -> Text
printSexp = encodeOne printer

--------------------------------------------------------------------------------
-- Optics

_cars :: Traversal' (SExpr a) (SExpr a)
_cars _ SNil            = pure SNil
_cars k (SCons car cdr) = SCons <$> k car <*> _cars k cdr
_cars _ a               = pure a

-- causes mysterious RTS failure.
-- TODO: report this.
-- makePrisms ''SExpr

_SCons :: Prism' (SExpr a) (SExpr a, SExpr a)
_SCons = prism' up down
  where
    up = uncurry SCons
    down = Data.SCargot.Repr.Basic.uncons

_SAtom :: Prism' (SExpr a) a
_SAtom = prism' up down
  where
    up = SAtom
    down (SAtom a) = Just a
    down _         = Nothing

_SSymbol :: Prism' Sexp Symbol
_SSymbol = (#SAtom) . (#ASymbol)

