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

parseSydML :: FilePath -> Text -> IO (Either DesexpError (Surface.Module Parse))
parseSydML fp = const . pure . Right $ Surface.Module
  { Surface.info = Surface.ModuleInfo
    { Surface.name = fromMaybe (error "fp mod") $ Name.filePathModule fp
    , Surface.imports = []
    }
  , Surface.content = []
  }

parseAndDesexp :: Desexp a -> Text -> IO (Either DesexpError a)
parseAndDesexp p s = do
  n <- parseTS tree_sitter_sydml "<file>" s
  pure $ runDesexp p (initialState [n])

-- parseModule :: FilePath -> Text -> IO ModuleL
-- parseModule fp s = do
--   n <- parseTS tree_sitter_systemf fp s
--   pure _

parseTS :: ConstPtr Language -> FilePath -> Text -> IO Syntax
parseTS lang fp s = _
  -- withParser (unConstPtr lang) \parser ->
  --   let src = T.encodeUtf8 s
  --   in withParseTree parser src \tree ->
  --     withRootNode tree (syntaxOfNode fp src <=< peek)

-- printChildren :: Ptr Node -> Int -> IO ()
-- printChildren children count =
--   for_ [0 .. count - 1] \n -> do
--     child <- peekElemOff children n
--     printNode child

-- printNode :: Node -> IO ()
-- printNode n@(Node {..}) = do
--   theType <- peekCString nodeType
--   let TSPoint {..} = nodeStartPoint n
--       start        = "[" ++ show pointRow ++ "," ++ show pointColumn ++ "]"
--   let TSPoint {..} = nodeEndPoint
--       end          = "[" ++ show pointRow ++ "," ++ show pointColumn ++ "]"
--   putStrLn $ theType ++ start ++ "-" ++ end

--------------------------------------------------------------------------------

data Syntax = SyntaxNode !SrcSpan !Text (List Syntax)
            -- REVIEW: do we need two text fields for atoms (anonymous nodes)?
            | SyntaxAtom !SrcSpan !Text !Text
            | SyntaxMissing !SrcSpan !Text
            deriving (Show, Generic, Eq, Ord)

syntaxNodeType :: Traversal' Syntax Text
syntaxNodeType k (SyntaxNode ss ty cs) = (\ty' -> SyntaxNode ss ty' cs) <$> k ty
syntaxNodeType k (SyntaxAtom ss ty t)  = pure $ SyntaxAtom ss ty t
syntaxNodeType k (SyntaxMissing ss t)  = pure $ SyntaxMissing ss t

instance HasLocation Syntax where
  location afb (SyntaxNode ss name cs) =
    (\ss' -> SyntaxNode ss' name cs) <$> afb ss
  location afb (SyntaxAtom ss name s) =
    (\ss' -> SyntaxAtom ss' name s) <$> afb ss
  location afb (SyntaxMissing ss name) =
    (\ss' -> SyntaxMissing ss' name) <$> afb ss

instance Pretty Syntax where
  pretty = foldr f mempty . prettySyntax
    where
      f (d,ss) acc = vsep [d <+> ";" <+> prettySrcSpan ss, acc]

prettySyntax :: Syntax -> List (Doc ann, SrcSpan)
prettySyntax (SyntaxNode ss t []) = [(parens (pretty t), ss)]
prettySyntax (SyntaxNode ss t cs) =
    ("(" <> pretty t, ss) : (cs' & each . _1 %~ ("  "<>))
  where
    cs' = foldMap prettySyntax cs & _last . _1 %~ (<>")")
prettySyntax (SyntaxAtom ss t _) = [(viaShow t, ss)]
prettySyntax (SyntaxMissing ss t) = [("(MISSING" <+> viaShow t, ss)]

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
  -- HACK: we hard-code the names of atomic nodes here. don't do this in the
  -- real project!
  -- TODO: the aforementioned hack does not account for qualified symbols.
  if | t `elem` atomTypes
       -> pure $ SyntaxAtom ss t (fetchSource src n)
     | cboolIsTrue (nodeIsNamed n)
       -> SyntaxNode ss t <$> getChildren fp src n
     | cboolIsTrue (nodeIsMissing n)
       -> pure $ SyntaxMissing ss t
     | otherwise
       -> pure $ SyntaxAtom ss t (fetchSource src n)
  where
    atomTypes = S.fromList
      ["symbol", "string", "number", "keyword", "variable"]

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

data DesexpError = DesexpError (Maybe ErrorItem) (Set ErrorItem)
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
type Failure r = DesexpError -> r

-- | Success continuation.
type Success a r = a -> State -> r

data Desexp a = Desexp
  { unDesexp
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

unDesexp :: Desexp a
         -> State
         -> Success a r
         -> Failure r
         -> Success a r
         -> Failure r
         -> r
unDesexp (Desexp run) = run

instance Functor Desexp where
  -- `fmap f p` is consuming iff `p` is consuming.
  fmap f p = Desexp \s cok cerr eok eerr ->
    unDesexp p s (cok . f) cerr (eok . f) eerr

instance Applicative Desexp where
  -- `pure a` is never consuming.
  pure a = Desexp \s _ _ eok _ -> eok a s

  -- `mf <*> ma` is consuming iff `mf` is consuming or `ma` is consuming.
  -- put symbolically,
  --     Consuming (mf <*> ma) <=> Consuming mf ∨ Consuming ma.
  mf <*> ma = Desexp \st cok cerr eok eerr ->
    let mk g f st' = unDesexp ma st' (g . f) cerr (g . f) eerr
    in unDesexp mf st (mk cok) cerr (mk eok) eerr

instance Monad Desexp where
  m >>= k = Desexp \st cok cerr eok eerr ->
    let
      -- if `m` is consuming, then `m >>= k` is consuming.
      cok' a st' = unDesexp (k a) st' cok cerr cok eerr
      -- if `m` is /not/ consuming, then it is unknown whether `m >>= k`
      -- consumes or not.
      eok' a st' = unDesexp (k a) st' cok cerr eok eerr
    in unDesexp m st cok' cerr eok' eerr

runDesexp :: Desexp a -> State -> Either DesexpError a
runDesexp = \p st -> unDesexp p st cok cerr eok eerr
  where
    cok a _ = Right a
    cerr    = Left
    eok a _ = Right a
    eerr    = Left

anySingle :: Desexp Syntax
anySingle = Desexp \st cok cerr eok eerr ->
  case st ^? _Cons of
    Nothing     -> eerr $ DesexpError Nothing mempty
    Just (x,xs) -> cok x xs

withNode :: Text -> Desexp a -> Desexp a
withNode ty p = Desexp \st cok cerr eok eerr ->
    case st ^? _Cons of
      Nothing                   -> eerr $ DesexpError (Just EndOfInput) es
      Just (SyntaxNode ss ty' cs, st')
        | ty == ty'             -> unDesexp p st' cok cerr eok eerr
      Just (n, st')             -> eerr $ DesexpError us es
        where
          us = Just . Tokens $ n :| []
  where
    es = S.singleton (Label $ ty <> " node")

element :: Desexp a -> Desexp (Maybe DesexpError)
element = _

observing :: Desexp a -> Desexp (Either () a)
observing = _

--------------------------------------------------------------------------------

-- module_ :: Desexp ModuleL
-- module_ = _
