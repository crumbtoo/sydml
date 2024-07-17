{-# LANGUAGE TemplateHaskell #-}
module Main where
--------------------------------------------------------------------------------
import Data.Hashable
import Data.Some
import Data.IORef
import Data.Text qualified as T
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Rock                     qualified
import Data.GADT.Compare.TH     (deriveGEq)
import Language.Scheme
import Sydc
import SydPrelude
--------------------------------------------------------------------------------

data Query a where
  A :: Query Integer
  B :: Query Integer
  C :: Query Integer
  D :: Query Integer

deriveGEq ''Query

deriving instance Eq (Query a)
deriving instance Show (Query a)

instance Hashable (Query a) where
  hashWithSalt salt = \case
      A -> hashWithSalt salt (0 :: Int)
      B -> hashWithSalt salt (1 :: Int)
      C -> hashWithSalt salt (2 :: Int)
      D -> hashWithSalt salt (3 :: Int)

instance Hashable (Some Query) where
  hashWithSalt salt (Some query) = hashWithSalt salt query

rules :: Rock.Rules Query
rules key = do
  liftIO $ putStrLn $ "Fetching " <> show key
  case key of
    A -> pure 10
    B -> do
      a <- Rock.fetch A
      pure $ a + 20
    C -> do
      a <- Rock.fetch A
      pure $ a + 30
    D ->
      (+) <$> Rock.fetch B <*> Rock.fetch C

--------------------------------------------------------------------------------

handlers :: Handlers (LspM ())
handlers =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_not -> do
        let params =
              ShowMessageRequestParams
                MessageType_Info
                "Turn on code lenses?"
                (Just [MessageActionItem "Turn on", MessageActionItem "Don't"])
        _ <- sendRequest SMethod_WindowShowMessageRequest params $ \case
          Right (InL (MessageActionItem "Turn on")) -> do
            let regOpts = CodeLensRegistrationOptions (InR Null) Nothing (Just False)

            _ <- registerCapability mempty SMethod_TextDocumentCodeLens regOpts $ \_req responder -> do
              let cmd = Command "Say hello" "lsp-hello-command" Nothing
                  rsp = [CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing]
              responder $ Right $ InL rsp
            pure ()
          Right _ ->
            sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Info "Not turning on code lenses")
          Left err ->
            sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Error $ "Something went wrong!\n" <> T.pack (show err))
        pure ()
    , requestHandler SMethod_TextDocumentHover $ \req responder -> do
        let TRequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
            Position _l _c' = pos
            rsp = Hover (InL ms) (Just range)
            ms = mkMarkdown "Hello world"
            range = Range pos pos
        responder (Right $ InL rsp)
    ]

main :: IO Int
main =
  runServer $
    ServerDefinition
      { parseConfig = const $ const $ Right ()
      , onConfigChange = const $ pure ()
      , defaultConfig = ()
      , configSection = "demo"
      , doInitialize = \env _req -> pure $ Right env
      , staticHandlers = \_caps -> handlers
      , interpretHandler = \env -> Iso (runLspT env) liftIO
      , options = defaultOptions
      }

-- main :: IO ()
-- main = do
--   do
--     liftIO $ putStrLn "Running"
--     result <- Rock.runTask rules (Rock.fetch D)
--     print result
--   do
--     liftIO $ putStrLn "Running with memoisation"
--     memoVar <- newIORef mempty
--     result <- Rock.runTask (Rock.memoise memoVar rules) (Rock.fetch D)
--     liftIO $ print result

--     liftIO $ putStrLn "once more:"
--     result' <- Rock.runTask (Rock.memoise memoVar rules) (Rock.fetch D)
--     liftIO $ print result'

