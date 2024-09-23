{-# LANGUAGE ViewPatterns #-}
module Data.EDN.ParseFromEDN
  ( ParseFromEDN(..)
  , parseFromEDN
  , list
  , symbol
  , anySymbol
  , keyword
  , keywordArgument
  , keywordFlag
  , module Text.Megaparsec
  )
  where
--------------------------------------------------------------------------------
import Control.Lens hiding ((:<))
import Data.EDN.Syntax
import Data.Set (Set)
import SydPrelude
import Control.Applicative
import qualified Data.Set as S
import Text.Megaparsec
import Text.Megaparsec.Internal
import Control.Monad
import Control.Comonad.Cofree
import Data.Located
import qualified Data.List.NonEmpty as List1
import qualified Data.Text as T
--------------------------------------------------------------------------------

newtype ParseFromEDN a = ParseFromEDN (Parsec Void (List LocatedValue) a)
  deriving newtype ( Functor, Applicative, Alternative, Monad, MonadPlus
                   , MonadParsec Void (List LocatedValue) )

parseFromEDN
  :: ParseFromEDN a
  -> FilePath
  -> List LocatedValue
  -> Either (ParseErrorBundle (List LocatedValue) Void) a
parseFromEDN (ParseFromEDN p) = runParser p

runParseFromEDN
  :: ParseFromEDN a
  -> State (List LocatedValue) Void
  -> Reply Void (List LocatedValue) a
runParseFromEDN (ParseFromEDN p) = runIdentity . runParsecT p

runSubParser
  :: ParseFromEDN a
  -> State (List LocatedValue) Void
  -> List LocatedValue
  -> List LocatedValue
  -> Reply Void (List LocatedValue) a
runSubParser p st rest xs =
    case r of
      -- 1. drop the hints; they are not useful for a "subparser."
      -- 2. whether or not `list p` is consuming depends only on whether the
      --    head of the input is a ListF, and whether `p ss` *succeeds*;
      --    whether `p ss` consumes is irrelevant.
      OK _ a -> Reply st'' Consumed (OK mempty a)
      Error e -> Reply st' Consumed (Error e)
  where
    st' = st & #stateInput .~ xs
    Reply _ _ r = runParseFromEDN p st'
    st'' = st & #stateInput .~ rest
              & #stateOffset +~ 1

makeSubParser
  -- | Retrive the "subinput" for the subparser
  :: (LocatedValue -> Maybe (SrcSpan, List LocatedValue))
  -- | The expected item.
  -> ErrorItem _
  -- | The subparser.
  -> (SrcSpan -> ParseFromEDN a)
  -> ParseFromEDN a
makeSubParser getSubInput expected p = mkParsec \st ->
    case take1_ st.stateInput of
      Just (getSubInput -> Just (ss, xs), rest) ->
          runSubParser (p ss) st rest xs
      Just (x, rest) -> makeError st (Just . Tokens . List1.singleton $ x)
      Nothing -> makeError st (Just EndOfInput)
  where
    makeError st unexpected = Reply st NotConsumed (Error e)
      where
        e = TrivialError
              st.stateOffset
              unexpected
              (S.singleton expected)

_Cofree :: Iso (Cofree f a)        (Cofree g b)
               (a, f (Cofree f a)) (b, g (Cofree g b))
_Cofree = iso up down
  where
    up (a :< as) = (a,as)
    down = uncurry (:<)

list :: (SrcSpan -> ParseFromEDN a) -> ParseFromEDN a
list = makeSubParser f (Label $ 'l' :| "ist")
  where
    f (ss :< ListF xs) = Just (ss,xs)
    f _                = Nothing

singleLabel :: String -> Set (ErrorItem t)
singleLabel (c:cs) = S.singleton $ Label $ c :| cs

symbol :: Text -> ParseFromEDN Text
symbol s = token f (singleLabel $ "symbol " <> T.unpack s)
  where
    f (ss :< SymbolF "" s')
      | s == s' = Just s'
    f _ = Nothing

anySymbol :: ParseFromEDN (SrcSpan, Text, Text)
anySymbol = token f (singleLabel "symbol")
  where
    f (ss :< SymbolF prefix s) = Just (ss, prefix, s)
    f _ = Nothing

keyword :: Text -> ParseFromEDN Text
keyword s = token f (singleLabel $ "keyword " <> T.unpack s)
  where
    f (ss :< KeywordF s')
      | s == s' = Just s'
    f _ = Nothing

keywordArgument :: Text -> ParseFromEDN a -> ParseFromEDN (Maybe a)
keywordArgument s p = optional $ keyword s *> p

keywordFlag :: a -> a -> Text -> ParseFromEDN a
keywordFlag ifNotPresent ifPresent s =
  keyword s $> ifPresent
  <|> pure ifNotPresent
