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
import qualified Data.ByteString as BS
import TreeSitter.Node
import Foreign.C
import Data.Set (Set)
import Control.Lens
import qualified Data.Set as S
import TreeSitter.Language
import Foreign.C.ConstPtr
import TreeSitter.SydML (tree_sitter_sydml)
import Foreign
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Language.SydML.Syntax as Surface
import qualified Sydc.Name as Name
import Data.EDN
--------------------------------------------------------------------------------

data Parse

type instance Surface.PassGlobal Parse = Located Text
type instance Surface.PassVar Parse = Located Text

parseSydML :: FilePath -> Text -> IO (Either ReparseError (Surface.Module Parse))
parseSydML fp = const . pure . Right $ Surface.Module
  { Surface.info = Surface.ModuleInfo
    { Surface.name = fromMaybe (error "fp mod") $ Name.filePathModule fp
    , Surface.imports = []
    }
  , Surface.content = []
  }

parseAndReparse :: Reparse a -> Text -> IO (Either ReparseError a)
parseAndReparse p s = do
  n <- parseTS tree_sitter_sydml "<file>" s
  pure $ runReparse p (initialState [n])

parseTS :: ConstPtr Language -> FilePath -> Text -> IO Syntax
parseTS lang fp s = _
  -- withParser (unConstPtr lang) \parser ->
  --   let src = T.encodeUtf8 s
  --   in withParseTree parser src \tree ->
  --     withRootNode tree (syntaxOfNode fp src <=< peek)

--------------------------------------------------------------------------------

data Syntax = SyntaxNode !SrcSpan !Text !(List Syntax)
            | SyntaxAnonymous !SrcSpan !Text
            | SyntaxMissing !SrcSpan !Text
            deriving (Show, Generic, Eq, Ord)

instance HasLocation Syntax where
  location afb (SyntaxNode ss name cs) =
    (\ss' -> SyntaxNode ss' name cs) <$> afb ss
  location afb (SyntaxAnonymous ss s) =
    (\ss' -> SyntaxAnonymous ss' s) <$> afb ss
  location afb (SyntaxMissing ss name) =
    (\ss' -> SyntaxMissing ss' name) <$> afb ss

instance Pretty Syntax where
  pretty = foldr f mempty . prettyLines
    where
      f (d,ss) acc = vsep [d <+> ";" <+> prettySrcSpan ss, acc]

      prettyLines :: Syntax -> List (Doc ann, SrcSpan)
      prettyLines (SyntaxAnonymous ss s) = [(viaShow s, ss)]
      prettyLines (SyntaxNode ss t []) = [(parens (pretty t), ss)]
      prettyLines (SyntaxNode ss t cs) =
          ("(" <> pretty t, ss) : (cs' & each . _1 <>:~ "  ")
        where
          cs' = foldMap prettyLines cs & _last . _1 <>~ ")"
      prettyLines (SyntaxMissing ss t) = [("(MISSING" <+> viaShow t, ss)]

      prettySrcSpan :: SrcSpan -> Doc ann
      prettySrcSpan ss = hsep [pos $ ss ^. #start, "-", pos $ ss ^. #end]
        where
          pos (Position l c) = brackets $ pretty l <> ", " <> pretty c

--------------------------------------------------------------------------------

getChildren :: FilePath -> BS.ByteString -> Node -> IO (List Syntax)
getChildren fp src n
  | childCount == 0 = pure []
  | otherwise = do
    -- pure []
    childArray <- mallocArray childCount
    tsNode <- malloc
    poke tsNode (n ^. #nodeTSNode)
    ts_node_copy_child_nodes tsNode childArray
    xs <- for [0 .. childCount - 1] $
      syntaxOfNode fp src <=< peekElemOff childArray
    free childArray
    free tsNode
    pure xs
  where
    childCount = fromIntegral $ n ^. #nodeChildCount

cboolIsTrue :: CBool -> Bool
cboolIsTrue (CBool n) = n /= 0

nodeSrcSpan :: FilePath -> Node -> SrcSpan
nodeSrcSpan fp n =
    SrcSpan fp (tsPointToPosition start) (tsPointToPosition end)
  where
    TSNode _ start _ _ _ = n ^. #nodeTSNode
    end = n ^. #nodeEndPoint

tsPointToPosition :: TSPoint -> Position
tsPointToPosition (TSPoint row column) = Position (fi row) (fi column)
  where fi = fromIntegral

syntaxOfNode :: FilePath -> BS.ByteString -> Node -> IO Syntax
syntaxOfNode fp src n = do
  t <- T.pack <$> peekCString (n ^. #nodeType)
  let ss = nodeSrcSpan fp n
  if | cboolIsTrue (nodeIsMissing n)
       -> pure $ SyntaxMissing ss t
     | cboolIsTrue (nodeIsNamed n)
       -> SyntaxNode ss t <$> getChildren fp src n
     | otherwise
       -> pure $ SyntaxAnonymous ss t

-- stolen from hécate's /boreal/ project. :p
fetchSource :: BS.ByteString -> Node -> Text
fetchSource src n = T.decodeUtf8 bs
  where
    bs = BS.take (endByte - startByte) $ BS.drop startByte src
    startByte = fromIntegral (nodeStartByte n)
    endByte = fromIntegral (nodeEndByte n)

--------------------------------------------------------------------------------

data ErrorItem = Tokens (List1 Syntax) | Label !Text | EndOfInput
  deriving (Eq, Ord, Show)

newtype Hints = Hints (Set ErrorItem)
  deriving stock (Show)
  deriving newtype (Semigroup, Monoid)

data ReparseError = ReparseError (Maybe ErrorItem) (Set ErrorItem)
  deriving (Show)

data State = State
  { input :: List Syntax
  }
  deriving (Show, Generic)

initialState :: List Syntax -> State
initialState s = State
  { input = s
  }

instance Cons State State Syntax Syntax where
  _Cons = prism' up down
    where
      up (x,xs) = xs & #input %~ (x:)
      down :: State -> Maybe (Syntax, State)
      down st = case st ^. #input of
        []   -> Nothing
        x:xs -> Just (x, st & #input .~ xs)

-- | Failure continuation.
type Failure r = ReparseError -> r

-- | Success continuation.
type Success a r = a -> State -> r

data Reparse a = Reparse
  { runReparse
    :: forall r.
       State
    -- | Consumed at least one token, OK.
    -> Success a r
    -- | Consumed at least one token, Fail.
    -> Failure r
    -- | Did not consume, OK.
    -> Success a r
    -- | Did not consume, Fail.
    -> Failure r
    -> r
  }

unReparse :: Reparse a
           -> State
           -> Success a r
           -> Failure r
           -> Success a r
           -> Failure r
           -> r
unReparse (Reparse run) = run

instance Functor Reparse where
  -- `fmap f p` is consuming iff `p` is consuming.
  fmap f p = Reparse \s cok cerr eok eerr ->
    unReparse p s (cok . f) cerr (eok . f) eerr

instance Applicative Reparse where
  -- `pure a` is never consuming.
  pure a = Reparse \s _ _ eok _ -> eok a s

  -- `mf <*> ma` is consuming iff `mf` is consuming or `ma` is consuming.
  -- put symbolically,
  --     Consuming (mf <*> ma) <=> Consuming mf ∨ Consuming ma.
  mf <*> ma = Reparse \st cok cerr eok eerr ->
    let mk g f st' = unReparse ma st' (g . f) cerr (g . f) eerr
    in unReparse mf st (mk cok) cerr (mk eok) eerr

instance Monad Reparse where
  m >>= k = Reparse \st cok cerr eok eerr ->
    let
      -- if `m` is consuming, then `m >>= k` is consuming.
      cok' a st' = unReparse (k a) st' cok cerr cok eerr
      -- if `m` is /not/ consuming, then it is unknown whether `m >>= k`
      -- consumes or not.
      eok' a st' = unReparse (k a) st' cok cerr eok eerr
    in unReparse m st cok' cerr eok' eerr

runReparse :: Reparse a -> State -> Either ReparseError a
runReparse = \p st -> unReparse p st cok cerr eok eerr
  where
    cok a _ = Right a
    cerr    = Left
    eok a _ = Right a
    eerr    = Left

anySingle :: Reparse Syntax
anySingle = Reparse \st cok cerr eok eerr ->
  case st ^? _Cons of
    Nothing     -> eerr $ ReparseError Nothing mempty
    Just (x,xs) -> cok x xs

withNode :: Text -> Reparse a -> Reparse a
withNode ty p = Reparse \st cok cerr eok eerr ->
    case st ^? _Cons of
      Nothing                   -> eerr $ ReparseError (Just EndOfInput) es
      Just (SyntaxNode ss ty' cs, st')
        | ty == ty'             -> unReparse p st' cok cerr eok eerr
      Just (n, st')             -> eerr $ ReparseError us es
        where
          us = Just . Tokens $ n :| []
  where
    es = S.singleton (Label $ ty <> " node")

element :: Reparse a -> Reparse (Maybe ReparseError)
element = _

observing :: Reparse a -> Reparse (Either () a)
observing = _

--------------------------------------------------------------------------------
