{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- |
-- Module      : Argus.LSP.Server
-- Description : Language Server Protocol integration for Argus
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides full LSP integration for Argus, enabling real-time
-- linting in IDEs like VS Code, Emacs, and Vim.
--
-- Features:
-- - Real-time diagnostics on file open, save, and change
-- - Code actions for quick fixes
-- - Hover information for diagnostics
-- - Workspace folder support
-- - Progress reporting for long analyses
-- - Debounced analysis for change events
-- - Execute command support for applying fixes
-- - Configuration change handling
module Argus.LSP.Server
  ( -- * Server
    runLspServer
  , LspConfig (..)
  , defaultLspConfig

    -- * LSP Types (re-exported for convenience)
  , LspDiagnostic (..)
  , LspPosition (..)
  , LspRange (..)
  , LspSeverity (..)
  , LspCodeAction (..)
  , LspTextEdit (..)
  , LspWorkspaceEdit (..)
  , LspHover (..)
  , LspMarkupContent (..)
  , LspCommand (..)
  , LspLocation (..)
  , LspDocumentSymbol (..)
  , LspSymbolKind (..)
  , LspCompletionItemKind (..)
  , LspCompletionItem (..)
  , LspCompletionList (..)

    -- * Conversion Functions
  , diagToLsp
  , symbolKindToCompletionKind
  , getCompletionPrefix
  , diagsToLsp
  , fixToCodeAction
  , severityToLsp
  , spanToRange
  ) where

import Control.Concurrent (forkIO, threadDelay, killThread, ThreadId)
import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Control.Monad (when, forM_)
import Data.Aeson (ToJSON (..), FromJSON (..), encode, decode, object, (.=), (.:), (.:?), (.!=))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types (parseMaybe, Parser)
import Data.ByteString.Lazy qualified as BL
import Data.IORef
import Data.Int (Int64)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.IO qualified as TIO
import Data.Time.Clock (getCurrentTime, UTCTime)
import GHC.Generics (Generic)
import System.IO (hFlush, stdout, stderr, stdin, hSetBuffering, BufferMode(..))
import System.Random (randomRIO)

import Argus.Types (Diagnostic(..), Severity(..), SrcSpan(..), Fix(..), FixEdit(..), DiagnosticKind(..), ArgusOptions(..), AnalysisMode(..), defaultOptions, Line(..), Column(..), srcSpanStartLineRaw, SymbolKind(Function, TypeConstructor, DataConstructor, TypeClass, TypeClassMethod, TypeFamily, PatternSynonym, Module))
import Argus.Config (Config, loadConfig)
import Argus.Core (analyzeSource, defaultContext)
import Argus.Rules.ConfigurableRules (defaultRulesConfig)
import Argus.Suppression (addSuppressionEntry)
import Argus.HIE.Query (findSymbolDefinition, findSymbolReferences, findAllSymbols, withHieQuery)
import Argus.HIE.Types (HieSymbol(hsName, hsKind, hsType), SymbolReference(..))

--------------------------------------------------------------------------------
-- LSP Types (Simplified)
--------------------------------------------------------------------------------

-- | LSP Position (0-indexed)
data LspPosition = LspPosition
  { lpLine      :: Int
  , lpCharacter :: Int
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LspPosition where
  toJSON LspPosition{..} = object
    [ "line"      .= lpLine
    , "character" .= lpCharacter
    ]

instance FromJSON LspPosition where
  parseJSON = Aeson.withObject "LspPosition" $ \o ->
    LspPosition <$> o .: "line" <*> o .: "character"

-- | LSP Range
data LspRange = LspRange
  { lrStart :: LspPosition
  , lrEnd   :: LspPosition
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LspRange where
  toJSON LspRange{..} = object
    [ "start" .= lrStart
    , "end"   .= lrEnd
    ]

instance FromJSON LspRange where
  parseJSON = Aeson.withObject "LspRange" $ \o ->
    LspRange <$> o .: "start" <*> o .: "end"

-- | LSP Severity
data LspSeverity
  = LspError       -- ^ 1
  | LspWarning     -- ^ 2
  | LspInformation -- ^ 3
  | LspHint        -- ^ 4
  deriving stock (Eq, Show, Ord, Enum, Bounded)

instance ToJSON LspSeverity where
  toJSON LspError       = toJSON (1 :: Int)
  toJSON LspWarning     = toJSON (2 :: Int)
  toJSON LspInformation = toJSON (3 :: Int)
  toJSON LspHint        = toJSON (4 :: Int)

instance FromJSON LspSeverity where
  parseJSON = Aeson.withScientific "LspSeverity" $ \n ->
    case round n :: Int of
      1 -> pure LspError
      2 -> pure LspWarning
      3 -> pure LspInformation
      4 -> pure LspHint
      _ -> fail "Invalid LSP severity"

-- | LSP Diagnostic
data LspDiagnostic = LspDiagnostic
  { ldRange    :: LspRange
  , ldSeverity :: LspSeverity
  , ldCode     :: Maybe Text
  , ldSource   :: Text
  , ldMessage  :: Text
  , ldTags     :: [Int]  -- ^ DiagnosticTag values
  , ldData     :: Maybe Aeson.Value  -- ^ For code action resolution
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LspDiagnostic where
  toJSON LspDiagnostic{..} = object $
    [ "range"    .= ldRange
    , "severity" .= ldSeverity
    , "source"   .= ldSource
    , "message"  .= ldMessage
    ] ++ catMaybes
    [ ("code" .=) <$> ldCode
    , if null ldTags then Nothing else Just ("tags" .= ldTags)
    , ("data" .=) <$> ldData
    ]

instance FromJSON LspDiagnostic where
  parseJSON = Aeson.withObject "LspDiagnostic" $ \o ->
    LspDiagnostic
      <$> o .: "range"
      <*> o .: "severity"
      <*> o .:? "code"
      <*> o .: "source"
      <*> o .: "message"
      <*> o .:? "tags" .!= []
      <*> o .:? "data"

-- | LSP Text Edit
data LspTextEdit = LspTextEdit
  { lteRange   :: LspRange
  , lteNewText :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LspTextEdit where
  toJSON LspTextEdit{..} = object
    [ "range"   .= lteRange
    , "newText" .= lteNewText
    ]

instance FromJSON LspTextEdit where
  parseJSON = Aeson.withObject "LspTextEdit" $ \o ->
    LspTextEdit <$> o .: "range" <*> o .: "newText"

-- | LSP Workspace Edit
data LspWorkspaceEdit = LspWorkspaceEdit
  { lweChanges :: Map Text [LspTextEdit]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LspWorkspaceEdit where
  toJSON LspWorkspaceEdit{..} = object
    [ "changes" .= lweChanges
    ]

instance FromJSON LspWorkspaceEdit where
  parseJSON = Aeson.withObject "LspWorkspaceEdit" $ \o ->
    LspWorkspaceEdit <$> o .: "changes"

-- | LSP Code Action
data LspCodeAction = LspCodeAction
  { lcaTitle       :: Text
  , lcaKind        :: Text
  , lcaDiagnostics :: [LspDiagnostic]
  , lcaIsPreferred :: Bool
  , lcaEdit        :: Maybe LspWorkspaceEdit
  , lcaCommand     :: Maybe LspCommand
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LspCodeAction where
  toJSON LspCodeAction{..} = object $
    [ "title"       .= lcaTitle
    , "kind"        .= lcaKind
    , "isPreferred" .= lcaIsPreferred
    ] ++ catMaybes
    [ if null lcaDiagnostics then Nothing else Just ("diagnostics" .= lcaDiagnostics)
    , ("edit" .=) <$> lcaEdit
    , ("command" .=) <$> lcaCommand
    ]

-- | LSP Command
data LspCommand = LspCommand
  { lcdTitle     :: Text
  , lcdCommand   :: Text
  , lcdArguments :: Maybe [Aeson.Value]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LspCommand where
  toJSON LspCommand{..} = object $
    [ "title"   .= lcdTitle
    , "command" .= lcdCommand
    ] ++ catMaybes [("arguments" .=) <$> lcdArguments]

instance FromJSON LspCommand where
  parseJSON = Aeson.withObject "LspCommand" $ \o ->
    LspCommand
      <$> o .: "title"
      <*> o .: "command"
      <*> o .:? "arguments"

-- | LSP Markup Content (for hover)
data LspMarkupContent = LspMarkupContent
  { lmcKind  :: Text  -- ^ "plaintext" or "markdown"
  , lmcValue :: Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LspMarkupContent where
  toJSON LspMarkupContent{..} = object
    [ "kind"  .= lmcKind
    , "value" .= lmcValue
    ]

-- | LSP Hover
data LspHover = LspHover
  { lhContents :: LspMarkupContent
  , lhRange    :: Maybe LspRange
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LspHover where
  toJSON LspHover{..} = object $
    [ "contents" .= lhContents
    ] ++ catMaybes [("range" .=) <$> lhRange]

-- | LSP Location (for go-to-definition, find-references)
data LspLocation = LspLocation
  { llUri   :: Text
  , llRange :: LspRange
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LspLocation where
  toJSON LspLocation{..} = object
    [ "uri"   .= llUri
    , "range" .= llRange
    ]

instance FromJSON LspLocation where
  parseJSON = Aeson.withObject "LspLocation" $ \o ->
    LspLocation <$> o .: "uri" <*> o .: "range"

-- | LSP Symbol Kind (per LSP spec)
data LspSymbolKind
  = SkFile        -- ^ 1
  | SkModule      -- ^ 2
  | SkNamespace   -- ^ 3
  | SkPackage     -- ^ 4
  | SkClass       -- ^ 5
  | SkMethod      -- ^ 6
  | SkProperty    -- ^ 7
  | SkField       -- ^ 8
  | SkConstructor -- ^ 9
  | SkEnum        -- ^ 10
  | SkInterface   -- ^ 11
  | SkFunction    -- ^ 12
  | SkVariable    -- ^ 13
  | SkConstant    -- ^ 14
  | SkString      -- ^ 15
  | SkNumber      -- ^ 16
  | SkBoolean     -- ^ 17
  | SkArray       -- ^ 18
  | SkObject      -- ^ 19
  | SkKey         -- ^ 20
  | SkNull        -- ^ 21
  | SkEnumMember  -- ^ 22
  | SkStruct      -- ^ 23
  | SkEvent       -- ^ 24
  | SkOperator    -- ^ 25
  | SkTypeParameter -- ^ 26
  deriving stock (Eq, Show, Ord, Enum, Bounded)

instance ToJSON LspSymbolKind where
  toJSON sk = toJSON (fromEnum sk + 1 :: Int)

instance FromJSON LspSymbolKind where
  parseJSON = Aeson.withScientific "LspSymbolKind" $ \n ->
    let i = round n - 1
    in if i >= 0 && i <= fromEnum (maxBound :: LspSymbolKind)
       then pure $ toEnum i
       else fail "Invalid LSP symbol kind"

-- | LSP Document Symbol (for outline view)
data LspDocumentSymbol = LspDocumentSymbol
  { ldsName           :: Text
  , ldsKind           :: LspSymbolKind
  , ldsRange          :: LspRange
  , ldsSelectionRange :: LspRange
  , ldsDetail         :: Maybe Text
  , ldsChildren       :: Maybe [LspDocumentSymbol]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LspDocumentSymbol where
  toJSON LspDocumentSymbol{..} = object $
    [ "name"           .= ldsName
    , "kind"           .= ldsKind
    , "range"          .= ldsRange
    , "selectionRange" .= ldsSelectionRange
    ] ++ catMaybes
    [ ("detail" .=) <$> ldsDetail
    , ("children" .=) <$> ldsChildren
    ]

instance FromJSON LspDocumentSymbol where
  parseJSON = Aeson.withObject "LspDocumentSymbol" $ \o ->
    LspDocumentSymbol
      <$> o .: "name"
      <*> o .: "kind"
      <*> o .: "range"
      <*> o .: "selectionRange"
      <*> o .:? "detail"
      <*> o .:? "children"

-- | Workspace Folder
data WorkspaceFolder = WorkspaceFolder
  { wfUri  :: Text
  , wfName :: Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON WorkspaceFolder where
  parseJSON = Aeson.withObject "WorkspaceFolder" $ \o ->
    WorkspaceFolder <$> o .: "uri" <*> o .: "name"

instance ToJSON WorkspaceFolder where
  toJSON WorkspaceFolder{..} = object
    [ "uri"  .= wfUri
    , "name" .= wfName
    ]

--------------------------------------------------------------------------------
-- Conversion Functions
--------------------------------------------------------------------------------

-- | Convert Argus Severity to LSP Severity
severityToLsp :: Severity -> LspSeverity
severityToLsp Error      = LspError
severityToLsp Warning    = LspWarning
severityToLsp Suggestion = LspHint
severityToLsp Info       = LspInformation

-- | Convert Argus SrcSpan to LSP Range (0-indexed)
spanToRange :: SrcSpan -> LspRange
spanToRange SrcSpan{..} = LspRange
  { lrStart = LspPosition (unLine srcSpanStartLine - 1) (unColumn srcSpanStartCol - 1)
  , lrEnd   = LspPosition (unLine srcSpanEndLine - 1) (unColumn srcSpanEndCol - 1)
  }

-- | Convert Argus Diagnostic to LSP Diagnostic
diagToLsp :: Diagnostic -> LspDiagnostic
diagToLsp Diagnostic{..} = LspDiagnostic
  { ldRange    = spanToRange diagSpan
  , ldSeverity = severityToLsp diagSeverity
  , ldCode     = diagCode
  , ldSource   = "argus"
  , ldMessage  = diagMessage
  , ldTags     = diagnosticTags diagKind
  , ldData     = Nothing
  }

-- | Get LSP diagnostic tags based on diagnostic kind
diagnosticTags :: DiagnosticKind -> [Int]
diagnosticTags UnusedCode   = [1]  -- Unnecessary
diagnosticTags UnusedImport = [1]  -- Unnecessary
diagnosticTags RedundantCode = [1] -- Unnecessary
diagnosticTags _            = []

-- | Convert multiple diagnostics to LSP diagnostics
diagsToLsp :: [Diagnostic] -> [LspDiagnostic]
diagsToLsp = map diagToLsp

-- | Convert Argus Fix to LSP Code Action
fixToCodeAction :: FilePath -> Diagnostic -> Fix -> LspCodeAction
fixToCodeAction path diag fix = LspCodeAction
  { lcaTitle       = fixTitle fix
  , lcaKind        = "quickfix"
  , lcaDiagnostics = [diagToLsp diag]
  , lcaIsPreferred = fixIsPreferred fix
  , lcaEdit        = Just $ LspWorkspaceEdit
      { lweChanges = Map.singleton (T.pack $ "file://" ++ path) $
          map editToLspEdit (fixEdits fix)
      }
  , lcaCommand     = Nothing
  }

-- | Convert FixEdit to LSP TextEdit
editToLspEdit :: FixEdit -> LspTextEdit
editToLspEdit FixEdit{..} = LspTextEdit
  { lteRange   = spanToRange fixEditSpan
  , lteNewText = fixEditNewText
  }

--------------------------------------------------------------------------------
-- LSP Server Configuration
--------------------------------------------------------------------------------

-- | LSP Server configuration
data LspConfig = LspConfig
  { lspConfigFile      :: Maybe FilePath  -- ^ Path to argus.toml
  , lspCacheDiags      :: Bool            -- ^ Cache diagnostics between requests
  , lspDebugLog        :: Maybe FilePath  -- ^ Debug log file path
  , lspAnalyzeOnOpen   :: Bool            -- ^ Analyze files when opened
  , lspAnalyzeOnSave   :: Bool            -- ^ Analyze files when saved
  , lspAnalyzeOnChange :: Bool            -- ^ Analyze on every change (expensive)
  , lspDebounceMs      :: Int             -- ^ Debounce time for change analysis
  , lspProgressReporting :: Bool          -- ^ Enable progress reporting
  , lspMaxDiagnostics  :: Maybe Int       -- ^ Maximum diagnostics per file
  }
  deriving stock (Eq, Show, Generic)

-- | Default LSP configuration
defaultLspConfig :: LspConfig
defaultLspConfig = LspConfig
  { lspConfigFile      = Nothing
  , lspCacheDiags      = True
  , lspDebugLog        = Nothing
  , lspAnalyzeOnOpen   = True
  , lspAnalyzeOnSave   = True
  , lspAnalyzeOnChange = False
  , lspDebounceMs      = 500
  , lspProgressReporting = True
  , lspMaxDiagnostics  = Just 100
  }

--------------------------------------------------------------------------------
-- LSP Server State
--------------------------------------------------------------------------------

-- | Debounce entry for a document
data DebounceEntry = DebounceEntry
  { deThreadId  :: ThreadId
  , deTimestamp :: UTCTime
  }

-- | Server state
data ServerState = ServerState
  { ssConfig            :: LspConfig
  , ssLinterConfig      :: TVar Config
  , ssDocuments         :: TVar (Map Text Text)       -- ^ Open documents
  , ssDiagnostics       :: TVar (Map Text [Diagnostic]) -- ^ Cached diagnostics
  , ssWorkspaceFolders  :: TVar [WorkspaceFolder]     -- ^ Workspace folders
  , ssInitialized       :: TVar Bool
  , ssShutdown          :: TVar Bool
  , ssDebounceTimers    :: IORef (Map Text DebounceEntry)  -- ^ Debounce timers for each document
  , ssProgressTokens    :: TVar (Map Text Bool)       -- ^ Active progress tokens
  , ssNextRequestId     :: IORef Int64                -- ^ Counter for request IDs
  }

-- | Create initial server state
newServerState :: LspConfig -> Config -> IO ServerState
newServerState lspCfg lintCfg = do
  lintCfgVar <- newTVarIO lintCfg
  docs <- newTVarIO Map.empty
  diags <- newTVarIO Map.empty
  folders <- newTVarIO []
  init' <- newTVarIO False
  shut <- newTVarIO False
  debounce <- newIORef Map.empty
  progress <- newTVarIO Map.empty
  reqId <- newIORef 1
  pure ServerState
    { ssConfig           = lspCfg
    , ssLinterConfig     = lintCfgVar
    , ssDocuments        = docs
    , ssDiagnostics      = diags
    , ssWorkspaceFolders = folders
    , ssInitialized      = init'
    , ssShutdown         = shut
    , ssDebounceTimers   = debounce
    , ssProgressTokens   = progress
    , ssNextRequestId    = reqId
    }

--------------------------------------------------------------------------------
-- JSON-RPC Message Types
--------------------------------------------------------------------------------

-- | JSON-RPC Request
data JsonRpcRequest = JsonRpcRequest
  { jrId     :: Maybe Aeson.Value
  , jrMethod :: Text
  , jrParams :: Maybe Aeson.Value
  }
  deriving stock (Show, Generic)

instance FromJSON JsonRpcRequest where
  parseJSON = Aeson.withObject "JsonRpcRequest" $ \o ->
    JsonRpcRequest
      <$> o .:? "id"
      <*> o .: "method"
      <*> o .:? "params"

-- | JSON-RPC Response
data JsonRpcResponse = JsonRpcResponse
  { jrsId     :: Maybe Aeson.Value
  , jrsResult :: Maybe Aeson.Value
  , jrsError  :: Maybe JsonRpcError
  }
  deriving stock (Show, Generic)

instance ToJSON JsonRpcResponse where
  toJSON JsonRpcResponse{..} = object $
    [ "jsonrpc" .= ("2.0" :: Text)
    , "id"      .= jrsId
    ] ++ catMaybes
    [ ("result" .=) <$> jrsResult
    , ("error" .=) <$> jrsError
    ]

-- | JSON-RPC Error
data JsonRpcError = JsonRpcError
  { jreCode    :: Int
  , jreMessage :: Text
  , jreData    :: Maybe Aeson.Value
  }
  deriving stock (Show, Generic)

instance ToJSON JsonRpcError where
  toJSON JsonRpcError{..} = object $
    [ "code"    .= jreCode
    , "message" .= jreMessage
    ] ++ catMaybes [("data" .=) <$> jreData]

-- | JSON-RPC Notification (no id)
data JsonRpcNotification = JsonRpcNotification
  { jnMethod :: Text
  , jnParams :: Aeson.Value
  }
  deriving stock (Show, Generic)

instance ToJSON JsonRpcNotification where
  toJSON JsonRpcNotification{..} = object
    [ "jsonrpc" .= ("2.0" :: Text)
    , "method"  .= jnMethod
    , "params"  .= jnParams
    ]

--------------------------------------------------------------------------------
-- LSP Server
--------------------------------------------------------------------------------

-- | Run the LSP server on stdin/stdout
runLspServer :: LspConfig -> IO ()
runLspServer lspCfg = do
  -- Set up buffering for LSP communication
  hSetBuffering stdin NoBuffering
  hSetBuffering stdout NoBuffering
  hSetBuffering stderr LineBuffering

  -- Load linter configuration
  lintCfg <- loadConfig (lspConfigFile lspCfg)

  -- Create server state
  state <- newServerState lspCfg lintCfg

  -- Main message loop
  messageLoop state

-- | Main message processing loop
messageLoop :: ServerState -> IO ()
messageLoop state = do
  shutdown <- readTVarIO (ssShutdown state)
  when (not shutdown) $ do
    maybeMsg <- readMessage
    case maybeMsg of
      Nothing -> pure ()  -- EOF or parse error
      Just msg -> do
        handleMessage state msg
        messageLoop state

-- | Read a single LSP message from stdin
readMessage :: IO (Maybe JsonRpcRequest)
readMessage = do
  -- Read Content-Length header
  headerLine <- TIO.getLine
  let maybeLen = parseContentLength headerLine
  case maybeLen of
    Nothing -> do
      -- Try to skip to next message
      readMessage
    Just len -> do
      -- Skip empty line
      _ <- TIO.getLine
      -- Read content
      content <- BL.hGet stdin len
      pure $ decode content

-- | Parse Content-Length header
parseContentLength :: Text -> Maybe Int
parseContentLength line =
  case T.stripPrefix "Content-Length: " line of
    Just numText -> case reads (T.unpack numText) of
      [(n, "")] -> Just n
      _ -> Nothing
    Nothing -> Nothing

-- | Send an LSP response
sendResponse :: JsonRpcResponse -> IO ()
sendResponse resp = do
  let body = encode resp
      header = "Content-Length: " <> T.pack (show (BL.length body)) <> "\r\n\r\n"
  TIO.putStr header
  BL.putStr body
  hFlush stdout

-- | Send an LSP notification
sendNotification :: JsonRpcNotification -> IO ()
sendNotification notif = do
  let body = encode notif
      header = "Content-Length: " <> T.pack (show (BL.length body)) <> "\r\n\r\n"
  TIO.putStr header
  BL.putStr body
  hFlush stdout

-- | Send an LSP request (server to client)
sendRequest :: ServerState -> Text -> Aeson.Value -> IO ()
sendRequest state method params = do
  reqId <- atomicModifyIORef' (ssNextRequestId state) $ \i -> (i + 1, i)
  let body = encode $ object
        [ "jsonrpc" .= ("2.0" :: Text)
        , "id"      .= reqId
        , "method"  .= method
        , "params"  .= params
        ]
      header = "Content-Length: " <> T.pack (show (BL.length body)) <> "\r\n\r\n"
  TIO.putStr header
  BL.putStr body
  hFlush stdout

-- | Handle an incoming message
handleMessage :: ServerState -> JsonRpcRequest -> IO ()
handleMessage state req = case jrMethod req of
  "initialize"                             -> handleInitialize state req
  "initialized"                            -> handleInitialized state
  "shutdown"                               -> handleShutdown state req
  "exit"                                   -> handleExit state
  "textDocument/didOpen"                   -> handleDidOpen state req
  "textDocument/didChange"                 -> handleDidChange state req
  "textDocument/didSave"                   -> handleDidSave state req
  "textDocument/didClose"                  -> handleDidClose state req
  "textDocument/codeAction"                -> handleCodeAction state req
  "textDocument/hover"                     -> handleHover state req
  "textDocument/definition"                -> handleDefinition state req
  "textDocument/references"                -> handleReferences state req
  "textDocument/documentSymbol"            -> handleDocumentSymbol state req
  "textDocument/completion"                -> handleCompletion state req
  "workspace/didChangeConfiguration"       -> handleDidChangeConfiguration state req
  "workspace/didChangeWorkspaceFolders"    -> handleDidChangeWorkspaceFolders state req
  "workspace/executeCommand"               -> handleExecuteCommand state req
  "$/cancelRequest"                        -> pure ()  -- Ignore cancellation for now
  _ -> when (isRequest req) $ sendResponse $ errorResponse req (-32601) "Method not found"

-- | Check if message is a request (has id)
isRequest :: JsonRpcRequest -> Bool
isRequest = (/= Nothing) . jrId

-- | Create an error response
errorResponse :: JsonRpcRequest -> Int -> Text -> JsonRpcResponse
errorResponse req code msg = JsonRpcResponse
  { jrsId = jrId req
  , jrsResult = Nothing
  , jrsError = Just $ JsonRpcError code msg Nothing
  }

-- | Create a success response
successResponse :: JsonRpcRequest -> Aeson.Value -> JsonRpcResponse
successResponse req result = JsonRpcResponse
  { jrsId = jrId req
  , jrsResult = Just result
  , jrsError = Nothing
  }

--------------------------------------------------------------------------------
-- LSP Request Handlers
--------------------------------------------------------------------------------

-- | Handle initialize request
handleInitialize :: ServerState -> JsonRpcRequest -> IO ()
handleInitialize state req = do
  atomically $ writeTVar (ssInitialized state) True

  -- Parse workspace folders from initialize params
  case jrParams req >>= parseWorkspaceFolders of
    Just folders -> atomically $ writeTVar (ssWorkspaceFolders state) folders
    Nothing -> pure ()

  let capabilities = object
        [ "capabilities" .= object
            [ "textDocumentSync" .= object
                [ "openClose" .= True
                , "change" .= (2 :: Int)  -- Incremental sync
                , "save" .= object ["includeText" .= True]
                ]
            , "codeActionProvider" .= object
                [ "codeActionKinds" .= (["quickfix", "refactor", "source"] :: [Text])
                ]
            , "hoverProvider" .= True
            , "definitionProvider" .= True
            , "referencesProvider" .= True
            , "documentSymbolProvider" .= True
            , "completionProvider" .= object
                [ "triggerCharacters" .= ([".", ":", "<", "@"] :: [Text])
                , "resolveProvider" .= False
                ]
            , "executeCommandProvider" .= object
                [ "commands" .= (["argus.applyFix", "argus.suppressDiagnostic", "argus.reloadConfig"] :: [Text])
                ]
            , "workspace" .= object
                [ "workspaceFolders" .= object
                    [ "supported" .= True
                    , "changeNotifications" .= True
                    ]
                ]
            ]
        , "serverInfo" .= object
            [ "name" .= ("argus" :: Text)
            , "version" .= ("1.0.0" :: Text)
            ]
        ]
  sendResponse $ successResponse req capabilities

-- | Parse workspace folders from initialize params
parseWorkspaceFolders :: Aeson.Value -> Maybe [WorkspaceFolder]
parseWorkspaceFolders = parseMaybe $ Aeson.withObject "params" $ \o -> do
  folders <- o .:? "workspaceFolders" .!= []
  pure folders

-- | Handle initialized notification
handleInitialized :: ServerState -> IO ()
handleInitialized state = do
  -- Register for workspace folder changes
  sendRequest state "client/registerCapability" $ object
    [ "registrations" .= (
        [ object
            [ "id" .= ("workspaceFolders" :: Text)
            , "method" .= ("workspace/didChangeWorkspaceFolders" :: Text)
            ]
        ] :: [Aeson.Value])
    ]

-- | Handle shutdown request
handleShutdown :: ServerState -> JsonRpcRequest -> IO ()
handleShutdown state req = do
  atomically $ writeTVar (ssShutdown state) True
  sendResponse $ successResponse req Aeson.Null

-- | Handle exit notification
handleExit :: ServerState -> IO ()
handleExit state = do
  shutdown <- readTVarIO (ssShutdown state)
  -- Exit with 0 if shutdown was called, 1 otherwise
  if shutdown then pure () else pure ()

-- | Handle textDocument/didOpen
handleDidOpen :: ServerState -> JsonRpcRequest -> IO ()
handleDidOpen state req = do
  case jrParams req >>= parseDidOpenParams of
    Nothing -> pure ()
    Just (uri, text) -> do
      atomically $ modifyTVar' (ssDocuments state) $ Map.insert uri text
      when (lspAnalyzeOnOpen (ssConfig state)) $
        analyzeAndPublishWithProgress state uri text

-- | Handle textDocument/didChange
handleDidChange :: ServerState -> JsonRpcRequest -> IO ()
handleDidChange state req = do
  case jrParams req >>= parseDidChangeParams of
    Nothing -> pure ()
    Just (uri, changes) -> do
      -- Apply incremental changes
      atomically $ modifyTVar' (ssDocuments state) $ \docs ->
        case Map.lookup uri docs of
          Nothing -> docs
          Just oldText ->
            let newText = foldl applyChange oldText changes
            in Map.insert uri newText docs

      when (lspAnalyzeOnChange (ssConfig state)) $ do
        -- Get the updated text
        maybeText <- atomically $ Map.lookup uri <$> readTVar (ssDocuments state)
        case maybeText of
          Nothing -> pure ()
          Just text -> debounceAnalyze state uri text

-- | Apply a single change to text
applyChange :: Text -> TextChange -> Text
applyChange text TextChange{..} =
  let ls = T.lines text
      startLine = tcStartLine
      startChar = tcStartChar
      endLine = tcEndLine
      endChar = tcEndChar
      -- Get text before start
      beforeLines = take startLine ls
      startLineText = if startLine < length ls then ls !! startLine else ""
      beforeStart = T.take startChar startLineText
      -- Get text after end
      endLineText = if endLine < length ls then ls !! endLine else ""
      afterEnd = T.drop endChar endLineText
      afterLines = drop (endLine + 1) ls
      -- Combine
      newContent = beforeStart <> tcNewText <> afterEnd
      newLines = beforeLines ++ T.lines newContent ++ afterLines
  in T.unlines newLines

-- | Text change from incremental sync
data TextChange = TextChange
  { tcStartLine :: Int
  , tcStartChar :: Int
  , tcEndLine   :: Int
  , tcEndChar   :: Int
  , tcNewText   :: Text
  }

-- | Debounce analysis for a document
debounceAnalyze :: ServerState -> Text -> Text -> IO ()
debounceAnalyze state uri text = do
  now <- getCurrentTime
  let debounceMs = lspDebounceMs (ssConfig state)

  -- Cancel existing timer
  oldTimers <- readIORef (ssDebounceTimers state)
  case Map.lookup uri oldTimers of
    Just entry -> killThread (deThreadId entry)
    Nothing -> pure ()

  -- Start new timer
  threadId <- forkIO $ do
    threadDelay (debounceMs * 1000)  -- Convert to microseconds
    analyzeAndPublishWithProgress state uri text

  -- Store new timer
  modifyIORef' (ssDebounceTimers state) $ Map.insert uri $ DebounceEntry
    { deThreadId = threadId
    , deTimestamp = now
    }

-- | Handle textDocument/didSave
handleDidSave :: ServerState -> JsonRpcRequest -> IO ()
handleDidSave state req = do
  case jrParams req >>= parseDidSaveParams of
    Nothing -> pure ()
    Just (uri, maybeText) -> do
      text <- case maybeText of
        Just t -> pure t
        Nothing -> fromMaybe "" <$> atomically (Map.lookup uri <$> readTVar (ssDocuments state))
      when (lspAnalyzeOnSave (ssConfig state)) $
        analyzeAndPublishWithProgress state uri text

-- | Handle textDocument/didClose
handleDidClose :: ServerState -> JsonRpcRequest -> IO ()
handleDidClose state req = do
  case jrParams req >>= parseDidCloseParams of
    Nothing -> pure ()
    Just uri -> do
      -- Cancel any pending debounce timer
      oldTimers <- readIORef (ssDebounceTimers state)
      case Map.lookup uri oldTimers of
        Just entry -> killThread (deThreadId entry)
        Nothing -> pure ()
      modifyIORef' (ssDebounceTimers state) $ Map.delete uri

      atomically $ do
        modifyTVar' (ssDocuments state) $ Map.delete uri
        modifyTVar' (ssDiagnostics state) $ Map.delete uri
      -- Clear diagnostics for this file
      publishDiagnostics uri []

-- | Handle textDocument/codeAction
handleCodeAction :: ServerState -> JsonRpcRequest -> IO ()
handleCodeAction state req = do
  case jrParams req >>= parseCodeActionParams of
    Nothing -> sendResponse $ errorResponse req (-32602) "Invalid params"
    Just (uri, range) -> do
      diags <- fromMaybe [] <$> atomically (Map.lookup uri <$> readTVar (ssDiagnostics state))
      let path = uriToPath uri
          relevantDiags = filter (diagInRange range) diags
          actions = concatMap (diagToCodeActions path) relevantDiags
          -- Also add suppress action for each diagnostic
          suppressActions = map (diagToSuppressAction uri) relevantDiags
      sendResponse $ successResponse req $ toJSON (actions ++ suppressActions)

-- | Create a suppress diagnostic code action
diagToSuppressAction :: Text -> Diagnostic -> LspCodeAction
diagToSuppressAction uri diag = LspCodeAction
  { lcaTitle       = "Suppress this warning"
  , lcaKind        = "source"
  , lcaDiagnostics = [diagToLsp diag]
  , lcaIsPreferred = False
  , lcaEdit        = Nothing
  , lcaCommand     = Just $ LspCommand
      { lcdTitle = "Suppress diagnostic"
      , lcdCommand = "argus.suppressDiagnostic"
      , lcdArguments = Just
          [ toJSON uri
          , toJSON (diagCode diag)
          , toJSON (srcSpanStartLineRaw (diagSpan diag))
          ]
      }
  }

-- | Handle textDocument/hover
handleHover :: ServerState -> JsonRpcRequest -> IO ()
handleHover state req = do
  case jrParams req >>= parseHoverParams of
    Nothing -> sendResponse $ errorResponse req (-32602) "Invalid params"
    Just (uri, pos) -> do
      diags <- fromMaybe [] <$> atomically (Map.lookup uri <$> readTVar (ssDiagnostics state))
      let relevantDiags = filter (diagAtPosition pos) diags
      case relevantDiags of
        [] -> sendResponse $ successResponse req Aeson.Null
        ds -> do
          let hover = diagnosticsToHover ds
          sendResponse $ successResponse req $ toJSON hover

-- | Check if diagnostic is at the given position
diagAtPosition :: LspPosition -> Diagnostic -> Bool
diagAtPosition pos diag =
  let range = spanToRange (diagSpan diag)
  in positionInRange pos range

-- | Check if position is within range
positionInRange :: LspPosition -> LspRange -> Bool
positionInRange pos range =
  not (positionBefore pos (lrStart range)) && not (positionBefore (lrEnd range) pos)

-- | Create hover from diagnostics
diagnosticsToHover :: [Diagnostic] -> LspHover
diagnosticsToHover diags = LspHover
  { lhContents = LspMarkupContent
      { lmcKind = "markdown"
      , lmcValue = T.intercalate "\n\n---\n\n" $ map formatDiagForHover diags
      }
  , lhRange = case diags of
      (d:_) -> Just $ spanToRange $ diagSpan d
      []    -> Nothing
  }

-- | Format a diagnostic for hover display
formatDiagForHover :: Diagnostic -> Text
formatDiagForHover Diagnostic{..} =
  let sevIcon = case diagSeverity of
        Error      -> "🔴"
        Warning    -> "🟡"
        Suggestion -> "💡"
        Info       -> "ℹ️"
      codeText = maybe "" (\c -> " `" <> c <> "`") diagCode
      fixText = if null diagFixes
        then ""
        else "\n\n**Fixes available:** " <> T.pack (show $ length diagFixes)
  in sevIcon <> " **" <> kindToText diagKind <> "**" <> codeText <> "\n\n" <> diagMessage <> fixText

-- | Convert DiagnosticKind to human-readable text
kindToText :: DiagnosticKind -> Text
kindToText = \case
  NamingConvention  -> "Naming Convention"
  UnusedCode        -> "Unused Code"
  UnusedImport      -> "Unused Import"
  RedundantCode     -> "Redundant Code"
  CodePattern       -> "Code Pattern"
  TypeSignature     -> "Type Signature"
  ImportStyle       -> "Import Style"
  TemplateHaskellRef -> "Template Haskell"
  SecurityIssue     -> "Security Issue"
  PerformanceIssue  -> "Performance Issue"
  ArchitecturalIssue -> "Architectural Issue"
  SpaceLeak         -> "Space Leak"
  PartialFunction   -> "Partial Function"
  ComplexityIssue   -> "Complexity Issue"
  Custom t          -> t

--------------------------------------------------------------------------------
-- Navigation Handlers (Definition, References, DocumentSymbol)
--------------------------------------------------------------------------------

-- | Handle textDocument/definition - Go to Definition
handleDefinition :: ServerState -> JsonRpcRequest -> IO ()
handleDefinition state req = do
  case jrParams req >>= parseDefinitionParams of
    Nothing -> sendResponse $ errorResponse req (-32602) "Invalid params"
    Just (uri, pos) -> do
      -- Try to find definition using HIE database
      let _path = uriToPath uri  -- Kept for future use with file-specific queries
          dbPath = ".hie/.hiedb"  -- Default HIE database location
      result <- try @SomeException $ do
        -- Extract symbol name at position from the source
        maybeSource <- atomically $ Map.lookup uri <$> readTVar (ssDocuments state)
        case maybeSource of
          Nothing -> pure Nothing
          Just source -> do
            let symbolName = extractSymbolAtPosition source pos
            case symbolName of
              Nothing -> pure Nothing
              Just name -> do
                -- Query HIE database for definition
                withHieQuery dbPath $ do
                  mSpan <- findSymbolDefinition name Nothing
                  case mSpan of
                    Nothing -> pure Nothing
                    Just span' -> pure $ Just $ spanToLocation span'
      case result of
        Left _ -> sendResponse $ successResponse req Aeson.Null
        Right Nothing -> sendResponse $ successResponse req Aeson.Null
        Right (Just loc) -> sendResponse $ successResponse req $ toJSON loc

-- | Handle textDocument/references - Find All References
handleReferences :: ServerState -> JsonRpcRequest -> IO ()
handleReferences state req = do
  case jrParams req >>= parseReferencesParams of
    Nothing -> sendResponse $ errorResponse req (-32602) "Invalid params"
    Just (uri, pos, _includeDecl) -> do
      let _path = uriToPath uri  -- Kept for future use with file-specific queries
          dbPath = ".hie/.hiedb"
      result <- try @SomeException $ do
        maybeSource <- atomically $ Map.lookup uri <$> readTVar (ssDocuments state)
        case maybeSource of
          Nothing -> pure []
          Just source -> do
            let symbolName = extractSymbolAtPosition source pos
            case symbolName of
              Nothing -> pure []
              Just name -> do
                -- Query HIE database for references
                withHieQuery dbPath $ do
                  refs <- findSymbolReferences name Nothing
                  pure $ map symbolRefToLocation refs
      case result of
        Left _ -> sendResponse $ successResponse req $ toJSON ([] :: [LspLocation])
        Right locs -> sendResponse $ successResponse req $ toJSON locs

-- | Handle textDocument/documentSymbol - Document Outline
handleDocumentSymbol :: ServerState -> JsonRpcRequest -> IO ()
handleDocumentSymbol state req = do
  case jrParams req >>= parseDocumentSymbolParams of
    Nothing -> sendResponse $ errorResponse req (-32602) "Invalid params"
    Just uri -> do
      maybeSource <- atomically $ Map.lookup uri <$> readTVar (ssDocuments state)
      case maybeSource of
        Nothing -> sendResponse $ successResponse req $ toJSON ([] :: [LspDocumentSymbol])
        Just source -> do
          -- Parse the source to extract document symbols
          let symbols = extractDocumentSymbols (uriToPath uri) source
          sendResponse $ successResponse req $ toJSON symbols

-- | Extract the symbol name at a given position from source text
extractSymbolAtPosition :: Text -> LspPosition -> Maybe Text
extractSymbolAtPosition source LspPosition{lpLine, lpCharacter} =
  let ls = T.lines source
  in if lpLine >= 0 && lpLine < length ls
     then
       let lineText = ls !! lpLine
           -- Find word at character position
           wordStart = findWordStart lineText lpCharacter
           wordEnd = findWordEnd lineText lpCharacter
       in if wordStart <= wordEnd && wordStart >= 0 && wordEnd <= T.length lineText
          then Just $ T.take (wordEnd - wordStart) $ T.drop wordStart lineText
          else Nothing
     else Nothing
  where
    findWordStart :: Text -> Int -> Int
    findWordStart txt pos =
      let before = T.take pos txt
          reversed = T.reverse before
      in pos - T.length (T.takeWhile isIdentChar reversed)

    findWordEnd :: Text -> Int -> Int
    findWordEnd txt pos =
      let after = T.drop pos txt
      in pos + T.length (T.takeWhile isIdentChar after)

    isIdentChar :: Char -> Bool
    isIdentChar c = c == '_' || c == '\'' || (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') || (c >= '0' && c <= '9')

-- | Convert a SrcSpan to an LspLocation
spanToLocation :: SrcSpan -> LspLocation
spanToLocation span' = LspLocation
  { llUri = T.pack $ "file://" ++ srcSpanFile span'
  , llRange = spanToRange span'
  }

-- | Convert a SymbolReference to an LspLocation
symbolRefToLocation :: SymbolReference -> LspLocation
symbolRefToLocation ref = spanToLocation (srSpan ref)

-- | Extract document symbols from source text
-- This provides a document outline with functions, types, etc.
extractDocumentSymbols :: FilePath -> Text -> [LspDocumentSymbol]
extractDocumentSymbols _path source =
  let ls = zip [0..] (T.lines source)
      -- Extract top-level definitions
      topLevel = concatMap extractFromLine ls
  in topLevel
  where
    extractFromLine :: (Int, Text) -> [LspDocumentSymbol]
    extractFromLine (lineNum, lineText) =
      let trimmed = T.stripStart lineText
          -- Check if this is a top-level definition (not indented or type signature)
      in if T.null trimmed || T.isPrefixOf "--" trimmed || T.isPrefixOf "{-" trimmed
         then []
         else catMaybes
           [ extractTypeSig lineNum lineText
           , extractFunctionDef lineNum lineText
           , extractDataDef lineNum lineText
           , extractNewtypeDef lineNum lineText
           , extractTypeDef lineNum lineText
           , extractClassDef lineNum lineText
           , extractInstanceDef lineNum lineText
           ]

    extractTypeSig :: Int -> Text -> Maybe LspDocumentSymbol
    extractTypeSig lineNum lineText =
      -- Pattern: name :: Type
      case T.breakOn "::" lineText of
        (before, rest)
          | not (T.null rest) && not (T.null $ T.strip before) ->
              let name = T.strip $ last $ T.words $ T.strip before
                  col = T.length lineText - T.length (T.stripStart lineText)
              in if isValidIdentifier name
                 then Just $ mkSymbol name SkFunction lineNum col (T.length lineText) (Just "type signature")
                 else Nothing
        _ -> Nothing

    extractFunctionDef :: Int -> Text -> Maybe LspDocumentSymbol
    extractFunctionDef lineNum lineText =
      -- Pattern: name args = ...
      let trimmed = T.stripStart lineText
          col = T.length lineText - T.length trimmed
      in case T.words trimmed of
           (name:rest)
             | not (T.null name)
             , isValidIdentifier name
             , any (T.isInfixOf "=") rest || T.isInfixOf " = " lineText ->
                 Just $ mkSymbol name SkFunction lineNum col (T.length lineText) Nothing
           _ -> Nothing

    extractDataDef :: Int -> Text -> Maybe LspDocumentSymbol
    extractDataDef lineNum lineText =
      -- Pattern: data TypeName = ...
      if T.isPrefixOf "data " (T.stripStart lineText)
      then case T.words (T.stripStart lineText) of
             (_:name:_) | isValidTypeName name ->
               let col = T.length lineText - T.length (T.stripStart lineText)
               in Just $ mkSymbol name SkClass lineNum col (T.length lineText) (Just "data type")
             _ -> Nothing
      else Nothing

    extractNewtypeDef :: Int -> Text -> Maybe LspDocumentSymbol
    extractNewtypeDef lineNum lineText =
      -- Pattern: newtype TypeName = ...
      if T.isPrefixOf "newtype " (T.stripStart lineText)
      then case T.words (T.stripStart lineText) of
             (_:name:_) | isValidTypeName name ->
               let col = T.length lineText - T.length (T.stripStart lineText)
               in Just $ mkSymbol name SkClass lineNum col (T.length lineText) (Just "newtype")
             _ -> Nothing
      else Nothing

    extractTypeDef :: Int -> Text -> Maybe LspDocumentSymbol
    extractTypeDef lineNum lineText =
      -- Pattern: type TypeName = ...
      if T.isPrefixOf "type " (T.stripStart lineText)
      then case T.words (T.stripStart lineText) of
             (_:name:_) | isValidTypeName name ->
               let col = T.length lineText - T.length (T.stripStart lineText)
               in Just $ mkSymbol name SkInterface lineNum col (T.length lineText) (Just "type synonym")
             _ -> Nothing
      else Nothing

    extractClassDef :: Int -> Text -> Maybe LspDocumentSymbol
    extractClassDef lineNum lineText =
      -- Pattern: class ClassName where ...
      if T.isPrefixOf "class " (T.stripStart lineText)
      then case T.words (T.stripStart lineText) of
             (_:name:_) | isValidTypeName name ->
               let col = T.length lineText - T.length (T.stripStart lineText)
               in Just $ mkSymbol name SkInterface lineNum col (T.length lineText) (Just "type class")
             _ -> Nothing
      else Nothing

    extractInstanceDef :: Int -> Text -> Maybe LspDocumentSymbol
    extractInstanceDef lineNum lineText =
      -- Pattern: instance ClassName Type where ...
      if T.isPrefixOf "instance " (T.stripStart lineText)
      then
        let trimmed = T.drop 9 (T.stripStart lineText)  -- Remove "instance "
            col = T.length lineText - T.length (T.stripStart lineText)
            instanceName = T.takeWhile (/= ' ') $ T.dropWhile (== ' ') trimmed
        in if not (T.null instanceName)
           then Just $ mkSymbol ("instance " <> T.takeWhile (\c -> c /= 'w' || T.take 5 (T.dropWhile (/= 'w') trimmed) /= "where") trimmed)
                                SkMethod lineNum col (T.length lineText) (Just "instance")
           else Nothing
      else Nothing

    mkSymbol :: Text -> LspSymbolKind -> Int -> Int -> Int -> Maybe Text -> LspDocumentSymbol
    mkSymbol name kind lineNum startCol endCol detail = LspDocumentSymbol
      { ldsName = name
      , ldsKind = kind
      , ldsRange = LspRange
          { lrStart = LspPosition lineNum startCol
          , lrEnd = LspPosition lineNum endCol
          }
      , ldsSelectionRange = LspRange
          { lrStart = LspPosition lineNum startCol
          , lrEnd = LspPosition lineNum (startCol + T.length name)
          }
      , ldsDetail = detail
      , ldsChildren = Nothing
      }

    isValidIdentifier :: Text -> Bool
    isValidIdentifier t =
      case T.uncons t of
        Just (c, _) -> (c >= 'a' && c <= 'z') || c == '_'
        Nothing -> False

    isValidTypeName :: Text -> Bool
    isValidTypeName t =
      case T.uncons t of
        Just (c, _) -> c >= 'A' && c <= 'Z'
        Nothing -> False

--------------------------------------------------------------------------------
-- Parameter Parsers for Navigation
--------------------------------------------------------------------------------

-- | Parse textDocument/definition params
parseDefinitionParams :: Aeson.Value -> Maybe (Text, LspPosition)
parseDefinitionParams = parseMaybe $ Aeson.withObject "params" $ \o -> do
  doc <- o .: "textDocument"
  uri <- doc .: "uri"
  pos <- o .: "position"
  pure (uri, pos)

-- | Parse textDocument/references params
parseReferencesParams :: Aeson.Value -> Maybe (Text, LspPosition, Bool)
parseReferencesParams = parseMaybe $ Aeson.withObject "params" $ \o -> do
  doc <- o .: "textDocument"
  uri <- doc .: "uri"
  pos <- o .: "position"
  ctx <- o .:? "context" .!= Aeson.object []
  includeDecl <- case ctx of
    Aeson.Object c -> c .:? "includeDeclaration" .!= True
    _ -> pure True
  pure (uri, pos, includeDecl)

-- | Parse textDocument/documentSymbol params
parseDocumentSymbolParams :: Aeson.Value -> Maybe Text
parseDocumentSymbolParams = parseMaybe $ Aeson.withObject "params" $ \o -> do
  doc <- o .: "textDocument"
  doc .: "uri"

-- | Handle workspace/didChangeConfiguration
handleDidChangeConfiguration :: ServerState -> JsonRpcRequest -> IO ()
handleDidChangeConfiguration state _req = do
  -- Reload the configuration
  lintCfg <- loadConfig (lspConfigFile (ssConfig state))
  atomically $ writeTVar (ssLinterConfig state) lintCfg

  -- Re-analyze all open documents
  docs <- atomically $ readTVar (ssDocuments state)
  forM_ (Map.toList docs) $ \(uri, text) ->
    analyzeAndPublishWithProgress state uri text

-- | Handle workspace/didChangeWorkspaceFolders
handleDidChangeWorkspaceFolders :: ServerState -> JsonRpcRequest -> IO ()
handleDidChangeWorkspaceFolders state req = do
  case jrParams req >>= parseWorkspaceFolderChange of
    Nothing -> pure ()
    Just (added, removed) -> do
      atomically $ modifyTVar' (ssWorkspaceFolders state) $ \folders ->
        filter (\f -> wfUri f `notElem` map wfUri removed) folders ++ added

-- | Handle workspace/executeCommand
handleExecuteCommand :: ServerState -> JsonRpcRequest -> IO ()
handleExecuteCommand state req = do
  case jrParams req >>= parseExecuteCommandParams of
    Nothing -> sendResponse $ errorResponse req (-32602) "Invalid params"
    Just (cmd, args) -> do
      result <- executeCommand state cmd args
      case result of
        Left err -> sendResponse $ errorResponse req (-32603) err
        Right val -> sendResponse $ successResponse req val

-- | Execute a command
executeCommand :: ServerState -> Text -> [Aeson.Value] -> IO (Either Text Aeson.Value)
executeCommand state cmd args = case cmd of
  "argus.applyFix" -> do
    -- Apply a workspace edit
    case args of
      [editJson] -> case Aeson.fromJSON editJson of
        Aeson.Success edit -> do
          sendRequest state "workspace/applyEdit" $ object ["edit" .= (edit :: LspWorkspaceEdit)]
          pure $ Right Aeson.Null
        Aeson.Error e -> pure $ Left $ T.pack e
      _ -> pure $ Left "Invalid arguments for argus.applyFix"

  "argus.suppressDiagnostic" -> do
    -- Add diagnostic to suppression baseline file
    case args of
      [uriJson, codeJson, lineJson] -> do
        case (Aeson.fromJSON uriJson, Aeson.fromJSON lineJson) of
          (Aeson.Success uri, Aeson.Success lineNum) -> do
            let code = case Aeson.fromJSON codeJson of
                  Aeson.Success (Just c) -> Just c
                  _ -> Nothing
            baselinePath <- addSuppressionEntry uri code lineNum
            pure $ Right $ toJSON $ "Suppression added to " <> T.pack baselinePath
          _ -> pure $ Left "Invalid URI or line number for argus.suppressDiagnostic"
      [uriJson, codeJson] -> do
        -- Legacy format without line number - use line 0
        case Aeson.fromJSON uriJson of
          Aeson.Success uri -> do
            let code = case Aeson.fromJSON codeJson of
                  Aeson.Success (Just c) -> Just c
                  _ -> Nothing
            baselinePath <- addSuppressionEntry uri code 0
            pure $ Right $ toJSON $ "Suppression added to " <> T.pack baselinePath
          _ -> pure $ Left "Invalid URI for argus.suppressDiagnostic"
      _ -> pure $ Left "Invalid arguments for argus.suppressDiagnostic"

  "argus.reloadConfig" -> do
    lintCfg <- loadConfig (lspConfigFile (ssConfig state))
    atomically $ writeTVar (ssLinterConfig state) lintCfg
    pure $ Right $ toJSON ("Configuration reloaded" :: Text)

  _ -> pure $ Left $ "Unknown command: " <> cmd

-- | Check if a diagnostic is in the given range
diagInRange :: LspRange -> Diagnostic -> Bool
diagInRange range diag =
  let diagRange = spanToRange (diagSpan diag)
  in rangesOverlap range diagRange

-- | Check if two ranges overlap
rangesOverlap :: LspRange -> LspRange -> Bool
rangesOverlap r1 r2 =
  not (positionBefore (lrEnd r1) (lrStart r2) || positionBefore (lrEnd r2) (lrStart r1))

-- | Check if position p1 is before position p2
positionBefore :: LspPosition -> LspPosition -> Bool
positionBefore p1 p2 =
  lpLine p1 < lpLine p2 || (lpLine p1 == lpLine p2 && lpCharacter p1 < lpCharacter p2)

-- | Convert a diagnostic to code actions
diagToCodeActions :: FilePath -> Diagnostic -> [LspCodeAction]
diagToCodeActions path diag = map (fixToCodeAction path diag) (diagFixes diag)

--------------------------------------------------------------------------------
-- Parameter Parsers
--------------------------------------------------------------------------------

parseDidOpenParams :: Aeson.Value -> Maybe (Text, Text)
parseDidOpenParams = parseMaybe $ Aeson.withObject "params" $ \o -> do
  doc <- o .: "textDocument"
  uri <- doc .: "uri"
  text <- doc .: "text"
  pure (uri, text)

parseDidChangeParams :: Aeson.Value -> Maybe (Text, [TextChange])
parseDidChangeParams = parseMaybe $ Aeson.withObject "params" $ \o -> do
  doc <- o .: "textDocument"
  uri <- doc .: "uri"
  changes <- o .: "contentChanges"
  parsedChanges <- mapM parseTextChange changes
  pure (uri, parsedChanges)

parseTextChange :: Aeson.Value -> Parser TextChange
parseTextChange = Aeson.withObject "change" $ \o -> do
  maybeRange <- o .:? "range"
  newText <- o .: "text"
  case maybeRange of
    Nothing -> do
      -- Full text change
      pure $ TextChange 0 0 maxBound maxBound newText
    Just rangeObj -> do
      start <- rangeObj .: "start"
      end <- rangeObj .: "end"
      startLine <- start .: "line"
      startChar <- start .: "character"
      endLine <- end .: "line"
      endChar <- end .: "character"
      pure $ TextChange startLine startChar endLine endChar newText

parseDidSaveParams :: Aeson.Value -> Maybe (Text, Maybe Text)
parseDidSaveParams = parseMaybe $ Aeson.withObject "params" $ \o -> do
  doc <- o .: "textDocument"
  uri <- doc .: "uri"
  text <- o .:? "text"
  pure (uri, text)

parseDidCloseParams :: Aeson.Value -> Maybe Text
parseDidCloseParams = parseMaybe $ Aeson.withObject "params" $ \o -> do
  doc <- o .: "textDocument"
  doc .: "uri"

parseCodeActionParams :: Aeson.Value -> Maybe (Text, LspRange)
parseCodeActionParams = parseMaybe $ Aeson.withObject "params" $ \o -> do
  doc <- o .: "textDocument"
  uri <- doc .: "uri"
  range <- o .: "range"
  pure (uri, range)

parseHoverParams :: Aeson.Value -> Maybe (Text, LspPosition)
parseHoverParams = parseMaybe $ Aeson.withObject "params" $ \o -> do
  doc <- o .: "textDocument"
  uri <- doc .: "uri"
  pos <- o .: "position"
  pure (uri, pos)

parseWorkspaceFolderChange :: Aeson.Value -> Maybe ([WorkspaceFolder], [WorkspaceFolder])
parseWorkspaceFolderChange = parseMaybe $ Aeson.withObject "params" $ \o -> do
  event <- o .: "event"
  added <- event .:? "added" .!= []
  removed <- event .:? "removed" .!= []
  pure (added, removed)

parseExecuteCommandParams :: Aeson.Value -> Maybe (Text, [Aeson.Value])
parseExecuteCommandParams = parseMaybe $ Aeson.withObject "params" $ \o -> do
  cmd <- o .: "command"
  args <- o .:? "arguments" .!= []
  pure (cmd, args)

--------------------------------------------------------------------------------
-- Analysis and Diagnostics
--------------------------------------------------------------------------------

-- | Analyze a document and publish diagnostics with progress reporting
analyzeAndPublishWithProgress :: ServerState -> Text -> Text -> IO ()
analyzeAndPublishWithProgress state uri content = do
  let path = uriToPath uri

  -- Start progress if enabled
  progressToken <- if lspProgressReporting (ssConfig state)
    then do
      token <- generateProgressToken
      startProgress state token ("Analyzing " <> T.pack path)
      pure (Just token)
    else pure Nothing

  -- Run analysis
  result <- try @SomeException $ do
    diags <- analyzeDocument state path content
    atomically $ modifyTVar' (ssDiagnostics state) $ Map.insert uri diags

    -- Apply max diagnostics limit
    let limitedDiags = case lspMaxDiagnostics (ssConfig state) of
          Nothing -> diags
          Just n -> take n diags

    publishDiagnostics uri (diagsToLsp limitedDiags)

  -- End progress
  case progressToken of
    Just token -> endProgress state token
    Nothing -> pure ()

  -- Handle errors
  case result of
    Left e -> do
      -- Publish error as diagnostic
      publishDiagnostics uri [LspDiagnostic
        { ldRange = LspRange (LspPosition 0 0) (LspPosition 0 0)
        , ldSeverity = LspError
        , ldCode = Just "argus/parse-error"
        , ldSource = "argus"
        , ldMessage = "Analysis failed: " <> T.pack (show e)
        , ldTags = []
        , ldData = Nothing
        }]
    Right () -> pure ()

-- | Generate a unique progress token
generateProgressToken :: IO Text
generateProgressToken = do
  n <- randomRIO (0 :: Int, maxBound)
  pure $ "argus-progress-" <> T.pack (show n)

-- | Start progress reporting
startProgress :: ServerState -> Text -> Text -> IO ()
startProgress state token title = do
  atomically $ modifyTVar' (ssProgressTokens state) $ Map.insert token True
  sendRequest state "window/workDoneProgress/create" $ object ["token" .= token]
  sendNotification $ JsonRpcNotification
    { jnMethod = "$/progress"
    , jnParams = object
        [ "token" .= token
        , "value" .= object
            [ "kind" .= ("begin" :: Text)
            , "title" .= title
            , "cancellable" .= False
            ]
        ]
    }

-- | End progress reporting
endProgress :: ServerState -> Text -> IO ()
endProgress state token = do
  active <- atomically $ do
    tokens <- readTVar (ssProgressTokens state)
    let isActive = Map.findWithDefault False token tokens
    modifyTVar' (ssProgressTokens state) $ Map.delete token
    pure isActive
  when active $ do
    sendNotification $ JsonRpcNotification
      { jnMethod = "$/progress"
      , jnParams = object
          [ "token" .= token
          , "value" .= object
              [ "kind" .= ("end" :: Text)
              ]
          ]
      }

-- | Analyze a document using Argus.Core analysis pipeline
analyzeDocument :: ServerState -> FilePath -> Text -> IO [Diagnostic]
analyzeDocument state path content = do
  cfg <- readTVarIO (ssLinterConfig state)
  -- Create a minimal analysis context for LSP
  -- We use QuickMode for LSP since we only have the source text, not HIE files
  let opts = defaultOptions { optMode = QuickMode }
      ctx = defaultContext cfg opts defaultRulesConfig
  -- Run the analysis pipeline
  analyzeSource ctx path content

-- | Publish diagnostics to the client
publishDiagnostics :: Text -> [LspDiagnostic] -> IO ()
publishDiagnostics uri diags = sendNotification $ JsonRpcNotification
  { jnMethod = "textDocument/publishDiagnostics"
  , jnParams = object
      [ "uri"         .= uri
      , "diagnostics" .= diags
      ]
  }

-- | Convert file URI to path
uriToPath :: Text -> FilePath
uriToPath uri = T.unpack $ fromMaybe uri $ T.stripPrefix "file://" uri

--------------------------------------------------------------------------------
-- Completion Types and Handler
--------------------------------------------------------------------------------

-- | LSP Completion Item Kind (per LSP spec)
data LspCompletionItemKind
  = CikText            -- ^ 1
  | CikMethod          -- ^ 2
  | CikFunction        -- ^ 3
  | CikConstructor     -- ^ 4
  | CikField           -- ^ 5
  | CikVariable        -- ^ 6
  | CikClass           -- ^ 7
  | CikInterface       -- ^ 8
  | CikModule          -- ^ 9
  | CikProperty        -- ^ 10
  | CikUnit            -- ^ 11
  | CikValue           -- ^ 12
  | CikEnum            -- ^ 13
  | CikKeyword         -- ^ 14
  | CikSnippet         -- ^ 15
  | CikColor           -- ^ 16
  | CikFile            -- ^ 17
  | CikReference       -- ^ 18
  | CikFolder          -- ^ 19
  | CikEnumMember      -- ^ 20
  | CikConstant        -- ^ 21
  | CikStruct          -- ^ 22
  | CikEvent           -- ^ 23
  | CikOperator        -- ^ 24
  | CikTypeParameter   -- ^ 25
  deriving stock (Eq, Show, Ord, Enum, Bounded)

instance ToJSON LspCompletionItemKind where
  toJSON cik = toJSON (fromEnum cik + 1 :: Int)

instance FromJSON LspCompletionItemKind where
  parseJSON = Aeson.withScientific "LspCompletionItemKind" $ \n ->
    let i = round n - 1
    in if i >= 0 && i <= fromEnum (maxBound :: LspCompletionItemKind)
       then pure $ toEnum i
       else fail "Invalid LSP completion item kind"

-- | LSP Completion Item
data LspCompletionItem = LspCompletionItem
  { lciLabel         :: Text
  , lciKind          :: Maybe LspCompletionItemKind
  , lciDetail        :: Maybe Text
  , lciDocumentation :: Maybe LspMarkupContent
  , lciInsertText    :: Maybe Text
  , lciFilterText    :: Maybe Text
  , lciSortText      :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LspCompletionItem where
  toJSON LspCompletionItem{..} = object $ catMaybes
    [ Just ("label" .= lciLabel)
    , ("kind" .=) <$> lciKind
    , ("detail" .=) <$> lciDetail
    , ("documentation" .=) <$> lciDocumentation
    , ("insertText" .=) <$> lciInsertText
    , ("filterText" .=) <$> lciFilterText
    , ("sortText" .=) <$> lciSortText
    ]

-- | LSP Completion List
data LspCompletionList = LspCompletionList
  { lclIsIncomplete :: Bool
  , lclItems        :: [LspCompletionItem]
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LspCompletionList where
  toJSON LspCompletionList{..} = object
    [ "isIncomplete" .= lclIsIncomplete
    , "items"        .= lclItems
    ]

-- | Handle textDocument/completion - Code Completions
handleCompletion :: ServerState -> JsonRpcRequest -> IO ()
handleCompletion state req = do
  case jrParams req >>= parseCompletionParams of
    Nothing -> sendResponse $ errorResponse req (-32602) "Invalid params"
    Just (uri, pos) -> do
      maybeSource <- atomically $ Map.lookup uri <$> readTVar (ssDocuments state)
      case maybeSource of
        Nothing -> sendResponse $ successResponse req $ toJSON ([] :: [LspCompletionItem])
        Just source -> do
          let dbPath = ".hie/.hiedb"
              prefix = getCompletionPrefix source pos
          completions <- getCompletions state dbPath uri source pos prefix
          let completionList = LspCompletionList
                { lclIsIncomplete = length completions >= 100
                , lclItems = take 100 completions
                }
          sendResponse $ successResponse req $ toJSON completionList

-- | Parse textDocument/completion params
parseCompletionParams :: Aeson.Value -> Maybe (Text, LspPosition)
parseCompletionParams = parseMaybe $ Aeson.withObject "params" $ \o -> do
  doc <- o .: "textDocument"
  uri <- doc .: "uri"
  pos <- o .: "position"
  pure (uri, pos)

-- | Get the completion prefix (text being typed before cursor)
getCompletionPrefix :: Text -> LspPosition -> Text
getCompletionPrefix source LspPosition{lpLine, lpCharacter} =
  let ls = T.lines source
  in if lpLine >= 0 && lpLine < length ls
     then
       let lineText = ls !! lpLine
           beforeCursor = T.take lpCharacter lineText
           -- Extract the identifier being typed
           reversed = T.reverse beforeCursor
           prefixReversed = T.takeWhile isCompletionChar reversed
       in T.reverse prefixReversed
     else ""
  where
    isCompletionChar c = c == '_' || c == '\'' || c == '.' ||
                         (c >= 'a' && c <= 'z') ||
                         (c >= 'A' && c <= 'Z') ||
                         (c >= '0' && c <= '9')

-- | Get completions based on context
getCompletions :: ServerState -> FilePath -> Text -> Text -> LspPosition -> Text -> IO [LspCompletionItem]
getCompletions _state dbPath _uri source pos prefix = do
  -- Get completions from multiple sources
  hieCompletions <- getHieCompletions dbPath prefix
  localCompletions <- getLocalCompletions source prefix
  keywordCompletions <- getKeywordCompletions prefix
  pragmaCompletions <- getPragmaCompletions source pos prefix
  extensionCompletions <- getExtensionCompletions source pos prefix

  -- Combine and deduplicate
  let allCompletions = hieCompletions ++ localCompletions ++ keywordCompletions
                       ++ pragmaCompletions ++ extensionCompletions
      deduplicated = Map.elems $ Map.fromList [(lciLabel c, c) | c <- allCompletions]

  pure deduplicated

-- | Get completions from HIE database
getHieCompletions :: FilePath -> Text -> IO [LspCompletionItem]
getHieCompletions dbPath prefix = do
  -- Only query HIE if we have a meaningful prefix (at least 2 chars)
  if T.length prefix < 2
    then pure []
    else do
      result <- try @SomeException $ withHieQuery dbPath $ do
        symbols <- findAllSymbols prefix  -- Search for symbols matching prefix
        pure $ map hieSymbolToCompletion symbols
      case result of
        Left _ -> pure []
        Right items -> pure items

-- | Check if a symbol name matches the completion prefix
matchesPrefix :: Text -> Text -> Bool
matchesPrefix prefix name
  | T.null prefix = True
  | otherwise = T.toLower prefix `T.isPrefixOf` T.toLower name

-- | Convert a HIE symbol to completion item
hieSymbolToCompletion :: HieSymbol -> LspCompletionItem
hieSymbolToCompletion sym = LspCompletionItem
  { lciLabel         = hsName sym
  , lciKind          = Just $ symbolKindToCompletionKind (hsKind sym)
  , lciDetail        = hsType sym
  , lciDocumentation = Nothing
  , lciInsertText    = Nothing
  , lciFilterText    = Nothing
  , lciSortText      = Just $ "0" <> hsName sym  -- HIE symbols first
  }

-- | Convert Argus SymbolKind to LSP CompletionItemKind
symbolKindToCompletionKind :: SymbolKind -> LspCompletionItemKind
symbolKindToCompletionKind = \case
  Function        -> CikFunction
  TypeConstructor -> CikClass
  DataConstructor -> CikConstructor
  TypeClass       -> CikInterface
  TypeClassMethod -> CikMethod
  TypeFamily      -> CikInterface
  PatternSynonym  -> CikValue
  Module          -> CikModule

-- | Get completions from local document symbols
getLocalCompletions :: Text -> Text -> IO [LspCompletionItem]
getLocalCompletions source prefix = do
  let symbols = extractLocalSymbols source
      matching = filter (matchesPrefix prefix . fst) symbols
  pure $ map localSymbolToCompletion matching

-- | Extract local symbols from source text
extractLocalSymbols :: Text -> [(Text, LspCompletionItemKind)]
extractLocalSymbols source =
  let ls = T.lines source
      lineSymbols = concatMap extractLineSymbols ls
  in lineSymbols
  where
    extractLineSymbols :: Text -> [(Text, LspCompletionItemKind)]
    extractLineSymbols line =
      let trimmed = T.stripStart line
      in catMaybes
           [ extractFunctionName trimmed
           , extractTypeName "data " CikStruct trimmed
           , extractTypeName "newtype " CikStruct trimmed
           , extractTypeName "type " CikInterface trimmed
           , extractTypeName "class " CikInterface trimmed
           ]

    extractFunctionName :: Text -> Maybe (Text, LspCompletionItemKind)
    extractFunctionName line =
      case T.words line of
        (name:rest)
          | isValidFunctionName name
          , any (T.isInfixOf "=") rest || T.isInfixOf " :: " line ->
              Just (name, if T.isInfixOf " :: " line then CikFunction else CikVariable)
        _ -> Nothing

    extractTypeName :: Text -> LspCompletionItemKind -> Text -> Maybe (Text, LspCompletionItemKind)
    extractTypeName keyword kind line =
      if T.isPrefixOf keyword line
      then case T.words (T.drop (T.length keyword) line) of
             (name:_) | isValidTypeName name -> Just (name, kind)
             _ -> Nothing
      else Nothing

    isValidFunctionName :: Text -> Bool
    isValidFunctionName t =
      not (isKeyword t) &&
      case T.uncons t of
        Just (c, _) -> (c >= 'a' && c <= 'z') || c == '_'
        Nothing -> False

    isValidTypeName :: Text -> Bool
    isValidTypeName t =
      case T.uncons t of
        Just (c, _) -> c >= 'A' && c <= 'Z'
        Nothing -> False

    isKeyword :: Text -> Bool
    isKeyword w = w `elem`
      ["module", "import", "where", "let", "in", "do", "case", "of",
       "if", "then", "else", "data", "type", "newtype", "class", "instance",
       "deriving", "default", "foreign", "infix", "infixl", "infixr"]

-- | Convert local symbol to completion item
localSymbolToCompletion :: (Text, LspCompletionItemKind) -> LspCompletionItem
localSymbolToCompletion (name, kind) = LspCompletionItem
  { lciLabel         = name
  , lciKind          = Just kind
  , lciDetail        = Just "(local)"
  , lciDocumentation = Nothing
  , lciInsertText    = Nothing
  , lciFilterText    = Nothing
  , lciSortText      = Just $ "1" <> name  -- Local symbols after HIE
  }

-- | Get keyword completions
getKeywordCompletions :: Text -> IO [LspCompletionItem]
getKeywordCompletions prefix = do
  let keywords =
        [ ("module", "module declaration")
        , ("import", "import statement")
        , ("qualified", "qualified import")
        , ("as", "import alias")
        , ("hiding", "hide imports")
        , ("where", "where clause")
        , ("let", "let binding")
        , ("in", "in expression")
        , ("do", "do notation")
        , ("case", "case expression")
        , ("of", "pattern match")
        , ("if", "if expression")
        , ("then", "then branch")
        , ("else", "else branch")
        , ("data", "data type declaration")
        , ("type", "type synonym")
        , ("newtype", "newtype declaration")
        , ("class", "type class")
        , ("instance", "instance declaration")
        , ("deriving", "deriving clause")
        , ("forall", "explicit quantification")
        , ("foreign", "foreign import/export")
        , ("default", "default declaration")
        , ("infix", "infix declaration")
        , ("infixl", "left-associative infix")
        , ("infixr", "right-associative infix")
        ]
      matching = filter (matchesPrefix prefix . fst) keywords
  pure $ map keywordToCompletion matching

-- | Convert keyword to completion item
keywordToCompletion :: (Text, Text) -> LspCompletionItem
keywordToCompletion (kw, desc) = LspCompletionItem
  { lciLabel         = kw
  , lciKind          = Just CikKeyword
  , lciDetail        = Just desc
  , lciDocumentation = Nothing
  , lciInsertText    = Nothing
  , lciFilterText    = Nothing
  , lciSortText      = Just $ "2" <> kw  -- Keywords after local symbols
  }

-- | Get LANGUAGE pragma completions when in pragma context
getPragmaCompletions :: Text -> LspPosition -> Text -> IO [LspCompletionItem]
getPragmaCompletions source LspPosition{lpLine} prefix =
  let ls = T.lines source
      lineText = if lpLine >= 0 && lpLine < length ls then ls !! lpLine else ""
  in if "{-#" `T.isInfixOf` lineText || T.isPrefixOf "LANGUAGE" (T.stripStart lineText)
     then getExtensionCompletions' prefix
     else pure []

-- | Get extension completions when typing LANGUAGE pragma
getExtensionCompletions :: Text -> LspPosition -> Text -> IO [LspCompletionItem]
getExtensionCompletions source LspPosition{lpLine} prefix =
  let ls = T.lines source
      lineText = if lpLine >= 0 && lpLine < length ls then ls !! lpLine else ""
  in if "LANGUAGE" `T.isInfixOf` lineText
     then getExtensionCompletions' prefix
     else pure []

-- | Get extension completions
getExtensionCompletions' :: Text -> IO [LspCompletionItem]
getExtensionCompletions' prefix = do
  let extensions =
        [ "BangPatterns"
        , "BinaryLiterals"
        , "ConstraintKinds"
        , "DataKinds"
        , "DefaultSignatures"
        , "DeriveAnyClass"
        , "DeriveDataTypeable"
        , "DeriveFoldable"
        , "DeriveFunctor"
        , "DeriveGeneric"
        , "DeriveLift"
        , "DeriveTraversable"
        , "DerivingStrategies"
        , "DerivingVia"
        , "DuplicateRecordFields"
        , "EmptyCase"
        , "ExistentialQuantification"
        , "ExplicitForAll"
        , "FlexibleContexts"
        , "FlexibleInstances"
        , "FunctionalDependencies"
        , "GADTs"
        , "GeneralizedNewtypeDeriving"
        , "ImportQualifiedPost"
        , "InstanceSigs"
        , "KindSignatures"
        , "LambdaCase"
        , "MonadComprehensions"
        , "MultiParamTypeClasses"
        , "MultiWayIf"
        , "NamedFieldPuns"
        , "NumericUnderscores"
        , "OverloadedLabels"
        , "OverloadedLists"
        , "OverloadedRecordDot"
        , "OverloadedStrings"
        , "PatternSynonyms"
        , "PolyKinds"
        , "QuantifiedConstraints"
        , "QuasiQuotes"
        , "RankNTypes"
        , "RecordWildCards"
        , "ScopedTypeVariables"
        , "StandaloneDeriving"
        , "StrictData"
        , "TemplateHaskell"
        , "TupleSections"
        , "TypeApplications"
        , "TypeFamilies"
        , "TypeFamilyDependencies"
        , "TypeOperators"
        , "UndecidableInstances"
        , "ViewPatterns"
        ]
      matching = filter (matchesPrefix prefix) extensions
  pure $ map extensionToCompletion matching

-- | Convert extension to completion item
extensionToCompletion :: Text -> LspCompletionItem
extensionToCompletion ext = LspCompletionItem
  { lciLabel         = ext
  , lciKind          = Just CikConstant
  , lciDetail        = Just "Language extension"
  , lciDocumentation = Nothing
  , lciInsertText    = Nothing
  , lciFilterText    = Nothing
  , lciSortText      = Nothing
  }
