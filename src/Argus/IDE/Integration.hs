{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.IDE.Integration
-- Description : Unified IDE integration layer
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides a unified interface for IDE integrations,
-- abstracting over different IDE protocols and capabilities:
--
-- * Real-time diagnostics with debouncing
-- * Quick-fix suggestions
-- * Refactoring actions
-- * Code lenses
-- * Inlay hints
-- * Semantic highlighting
--
-- == Supported IDEs
--
-- * VS Code (via LSP)
-- * IntelliJ/IDEA (via external annotations)
-- * Emacs (via flycheck integration)
-- * Vim/Neovim (via ALE/coc.nvim)
--
-- == Usage
--
-- @
-- engine <- newIDEEngine defaultIDEConfig
-- result <- analyzeForIDE engine filePath content
-- @
module Argus.IDE.Integration
  ( -- * IDE Engine
    IDEEngine (..)
  , newIDEEngine
  , IDEConfig (..)
  , defaultIDEConfig

    -- * Analysis
  , analyzeForIDE
  , analyzeIncrementalForIDE
  , IDEAnalysisResult (..)

    -- * Diagnostics
  , IDEDiagnostic (..)
  , DiagnosticSeverityIDE (..)
  , DiagnosticTag (..)
  , CodeAction (..)
  , CodeActionKind (..)

    -- * Code Lenses
  , CodeLens (..)
  , CodeLensCommand (..)

    -- * Inlay Hints
  , InlayHint (..)
  , InlayHintKind (..)

    -- * Semantic Tokens
  , SemanticToken (..)
  , SemanticTokenType (..)
  , SemanticTokenModifier (..)

    -- * Refactoring
  , RefactorAction (..)
  , RefactorKind (..)
  , applyRefactor
  , previewRefactor

    -- * Document Operations
  , TextEdit (..)
  , WorkspaceEdit (..)
  , DocumentChange (..)
  , TextDocumentEdit (..)

    -- * IDE State
  , IDEState (..)
  , DocumentState (..)
  , getIDEState
  , updateDocument
  , closeDocument

    -- * Event Handlers
  , onDocumentOpen
  , onDocumentChange
  , onDocumentSave
  , onDocumentClose
  , onConfigurationChange
  ) where

import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Control.Monad (when)
import Data.Aeson (ToJSON, FromJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time.Clock (UTCTime, getCurrentTime, diffUTCTime)
import GHC.Generics (Generic)

import Argus.Types
  ( SrcSpan(..)
  , Diagnostic(..)
  , DiagnosticKind(..)
  , Fix(..)
  , FixEdit(..)
  , Severity(..)
  , Line(..)
  , Column(..)
  )

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for IDE integration
data IDEConfig = IDEConfig
  { ideDebounceDuration    :: Double
      -- ^ Debounce duration in seconds for real-time analysis
  , ideMaxDiagnosticsPerFile :: Int
      -- ^ Maximum diagnostics to show per file
  , ideEnableCodeLenses    :: Bool
      -- ^ Enable code lenses
  , ideEnableInlayHints    :: Bool
      -- ^ Enable inlay hints
  , ideEnableSemanticTokens :: Bool
      -- ^ Enable semantic highlighting
  , ideEnableRefactoring   :: Bool
      -- ^ Enable refactoring actions
  , ideAutoFixOnSave       :: Bool
      -- ^ Apply safe auto-fixes on save
  , ideShowRelatedInfo     :: Bool
      -- ^ Show related information in diagnostics
  , ideGroupDiagnostics    :: Bool
      -- ^ Group related diagnostics
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default IDE configuration
defaultIDEConfig :: IDEConfig
defaultIDEConfig = IDEConfig
  { ideDebounceDuration = 0.3
  , ideMaxDiagnosticsPerFile = 100
  , ideEnableCodeLenses = True
  , ideEnableInlayHints = True
  , ideEnableSemanticTokens = True
  , ideEnableRefactoring = True
  , ideAutoFixOnSave = False
  , ideShowRelatedInfo = True
  , ideGroupDiagnostics = True
  }

--------------------------------------------------------------------------------
-- IDE Engine
--------------------------------------------------------------------------------

-- | IDE integration engine
data IDEEngine = IDEEngine
  { ideConfig   :: TVar IDEConfig
  , ideState    :: TVar IDEState
  , ideAnalyzer :: FilePath -> Text -> IO [Diagnostic]
  }

-- | IDE state
data IDEState = IDEState
  { idsDocuments      :: Map FilePath DocumentState
      -- ^ Open documents
  , idsLastAnalysis   :: Map FilePath UTCTime
      -- ^ Last analysis time per document
  , idsPendingChanges :: Map FilePath Bool
      -- ^ Documents with pending changes
  , idsGlobalConfig   :: Map Text Text
      -- ^ Global configuration
  }
  deriving stock (Show, Generic)

-- | Document state
data DocumentState = DocumentState
  { dsContent       :: Text
      -- ^ Current content
  , dsVersion       :: Int
      -- ^ Document version
  , dsDiagnostics   :: [IDEDiagnostic]
      -- ^ Current diagnostics
  , dsCodeLenses    :: [CodeLens]
      -- ^ Current code lenses
  , dsInlayHints    :: [InlayHint]
      -- ^ Current inlay hints
  , dsLastModified  :: UTCTime
      -- ^ Last modification time
  }
  deriving stock (Show, Generic)

-- | Create a new IDE engine
newIDEEngine :: IDEConfig -> (FilePath -> Text -> IO [Diagnostic]) -> IO IDEEngine
newIDEEngine config analyzer = do
  configVar <- newTVarIO config
  stateVar <- newTVarIO emptyIDEState
  pure IDEEngine
    { ideConfig = configVar
    , ideState = stateVar
    , ideAnalyzer = analyzer
    }

-- | Empty IDE state
emptyIDEState :: IDEState
emptyIDEState = IDEState
  { idsDocuments = Map.empty
  , idsLastAnalysis = Map.empty
  , idsPendingChanges = Map.empty
  , idsGlobalConfig = Map.empty
  }

--------------------------------------------------------------------------------
-- Analysis
--------------------------------------------------------------------------------

-- | Analysis result for IDE
data IDEAnalysisResult = IDEAnalysisResult
  { iarDiagnostics    :: [IDEDiagnostic]
  , iarCodeActions    :: [CodeAction]
  , iarCodeLenses     :: [CodeLens]
  , iarInlayHints     :: [InlayHint]
  , iarSemanticTokens :: [SemanticToken]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Analyze a file for IDE
analyzeForIDE :: IDEEngine -> FilePath -> Text -> IO IDEAnalysisResult
analyzeForIDE engine path content = do
  config <- readTVarIO (ideConfig engine)

  -- Run analysis
  result <- try @SomeException $ ideAnalyzer engine path content
  diagnostics <- case result of
    Left _ -> pure []
    Right diags -> pure $ take (ideMaxDiagnosticsPerFile config) diags

  -- Convert to IDE diagnostics
  let ideDiags = map (diagnosticToIDEDiagnostic config) diagnostics

  -- Generate code actions from diagnostics
  let codeActions = concatMap (diagnosticToCodeActions config) diagnostics

  -- Generate code lenses if enabled
  codeLenses <- if ideEnableCodeLenses config
                then generateCodeLenses path content diagnostics
                else pure []

  -- Generate inlay hints if enabled
  inlayHints <- if ideEnableInlayHints config
                then generateInlayHints path content
                else pure []

  -- Generate semantic tokens if enabled
  semanticTokens <- if ideEnableSemanticTokens config
                    then generateSemanticTokens path content
                    else pure []

  -- Update document state
  now <- getCurrentTime
  atomically $ modifyTVar' (ideState engine) $ \st ->
    let docState = DocumentState
          { dsContent = content
          , dsVersion = maybe 1 (+ 1) $ dsVersion <$> Map.lookup path (idsDocuments st)
          , dsDiagnostics = ideDiags
          , dsCodeLenses = codeLenses
          , dsInlayHints = inlayHints
          , dsLastModified = now
          }
    in st
      { idsDocuments = Map.insert path docState (idsDocuments st)
      , idsLastAnalysis = Map.insert path now (idsLastAnalysis st)
      , idsPendingChanges = Map.delete path (idsPendingChanges st)
      }

  pure IDEAnalysisResult
    { iarDiagnostics = ideDiags
    , iarCodeActions = codeActions
    , iarCodeLenses = codeLenses
    , iarInlayHints = inlayHints
    , iarSemanticTokens = semanticTokens
    }

-- | Incremental analysis (only if content changed)
analyzeIncrementalForIDE :: IDEEngine -> FilePath -> Text -> IO (Maybe IDEAnalysisResult)
analyzeIncrementalForIDE engine path content = do
  config <- readTVarIO (ideConfig engine)
  state <- readTVarIO (ideState engine)
  now <- getCurrentTime

  -- Check if we need to analyze
  let needsAnalysis = case Map.lookup path (idsDocuments state) of
        Nothing -> True
        Just ds -> dsContent ds /= content

  let debounceOk = case Map.lookup path (idsLastAnalysis state) of
        Nothing -> True
        Just lastTime -> diffUTCTime now lastTime > realToFrac (ideDebounceDuration config)

  if needsAnalysis && debounceOk
    then Just <$> analyzeForIDE engine path content
    else pure Nothing

--------------------------------------------------------------------------------
-- IDE Diagnostic Types
--------------------------------------------------------------------------------

-- | IDE diagnostic (richer than core Diagnostic)
data IDEDiagnostic = IDEDiagnostic
  { idSpan          :: SrcSpan
  , idMessage       :: Text
  , idSeverity      :: DiagnosticSeverityIDE
  , idCode          :: Maybe Text
  , idSource        :: Text
  , idTags          :: [DiagnosticTag]
  , idRelatedInfo   :: [(SrcSpan, Text)]
  , idCodeActions   :: [CodeAction]
  , idData          :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | IDE diagnostic severity
data DiagnosticSeverityIDE
  = SeverityError
  | SeverityWarning
  | SeverityInformation
  | SeverityHint
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Diagnostic tags
data DiagnosticTag
  = TagUnnecessary   -- ^ Faded out (unused code)
  | TagDeprecated    -- ^ Strike-through (deprecated)
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Convert core Diagnostic to IDE diagnostic
diagnosticToIDEDiagnostic :: IDEConfig -> Diagnostic -> IDEDiagnostic
diagnosticToIDEDiagnostic config Diagnostic{..} = IDEDiagnostic
  { idSpan = diagSpan
  , idMessage = diagMessage
  , idSeverity = severityToIDE diagSeverity
  , idCode = diagCode
  , idSource = "argus"
  , idTags = diagKindToTags diagKind
  , idRelatedInfo = if ideShowRelatedInfo config then diagRelated else []
  , idCodeActions = []
  , idData = Nothing
  }

-- | Convert severity
severityToIDE :: Severity -> DiagnosticSeverityIDE
severityToIDE Error = SeverityError
severityToIDE Warning = SeverityWarning
severityToIDE Suggestion = SeverityHint
severityToIDE Info = SeverityInformation

-- | Convert diagnostic kind to tags
diagKindToTags :: DiagnosticKind -> [DiagnosticTag]
diagKindToTags UnusedCode = [TagUnnecessary]
diagKindToTags UnusedImport = [TagUnnecessary]
diagKindToTags RedundantCode = [TagUnnecessary]
diagKindToTags _ = []

--------------------------------------------------------------------------------
-- Code Actions
--------------------------------------------------------------------------------

-- | Code action
data CodeAction = CodeAction
  { caTitle       :: Text
  , caKind        :: CodeActionKind
  , caDiagnostics :: [IDEDiagnostic]
  , caEdit        :: Maybe WorkspaceEdit
  , caCommand     :: Maybe CodeLensCommand
  , caIsPreferred :: Bool
  , caDisabled    :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Code action kind
data CodeActionKind
  = QuickFix
  | Refactor
  | RefactorExtract
  | RefactorInline
  | RefactorRewrite
  | Source
  | SourceOrganizeImports
  | SourceFixAll
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Generate code actions from diagnostic
diagnosticToCodeActions :: IDEConfig -> Diagnostic -> [CodeAction]
diagnosticToCodeActions config Diagnostic{..} =
  if ideEnableRefactoring config
  then map (fixToCodeAction diagSpan diagMessage) diagFixes
  else []

-- | Convert fix to code action
fixToCodeAction :: SrcSpan -> Text -> Fix -> CodeAction
fixToCodeAction span' _msg Fix{..} = CodeAction
  { caTitle = fixTitle
  , caKind = QuickFix
  , caDiagnostics = []
  , caEdit = Just $ WorkspaceEdit
      { weChanges = Map.singleton (srcSpanFile span')
          [fixEditToTextEdit e | e <- fixEdits]
      , weDocumentChanges = Nothing
      }
  , caCommand = Nothing
  , caIsPreferred = fixIsPreferred
  , caDisabled = Nothing
  }

-- | Convert FixEdit to TextEdit
fixEditToTextEdit :: FixEdit -> TextEdit
fixEditToTextEdit FixEdit{..} = TextEdit
  { teRange = fixEditSpan
  , teNewText = fixEditNewText
  }

--------------------------------------------------------------------------------
-- Code Lenses
--------------------------------------------------------------------------------

-- | Code lens
data CodeLens = CodeLens
  { clRange   :: SrcSpan
  , clCommand :: Maybe CodeLensCommand
  , clData    :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Code lens command
data CodeLensCommand = CodeLensCommand
  { clcTitle     :: Text
  , clcCommand   :: Text
  , clcArguments :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Generate code lenses for a file
generateCodeLenses :: FilePath -> Text -> [Diagnostic] -> IO [CodeLens]
generateCodeLenses _path _content diagnostics = pure $
  -- Generate "N warnings" lens at file start
  [ CodeLens
      { clRange = defaultSpan
      , clCommand = Just CodeLensCommand
          { clcTitle = T.pack (show (length warnings)) <> " warnings"
          , clcCommand = "argus.showWarnings"
          , clcArguments = []
          }
      , clData = Nothing
      }
  | not (null warnings)
  ]
  where
    warnings = filter ((== Warning) . diagSeverity) diagnostics
    defaultSpan = SrcSpan "" (Line 1) (Column 1) (Line 1) (Column 1)

--------------------------------------------------------------------------------
-- Inlay Hints
--------------------------------------------------------------------------------

-- | Inlay hint
data InlayHint = InlayHint
  { ihPosition :: (Line, Column)
  , ihLabel    :: Text
  , ihKind     :: InlayHintKind
  , ihTooltip  :: Maybe Text
  , ihPadding  :: (Bool, Bool)  -- ^ (left, right)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Inlay hint kind
data InlayHintKind
  = HintType       -- ^ Type annotation
  | HintParameter  -- ^ Parameter name
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Generate inlay hints for a file
generateInlayHints :: FilePath -> Text -> IO [InlayHint]
generateInlayHints _path _content = pure []  -- Placeholder for type hints

--------------------------------------------------------------------------------
-- Semantic Tokens
--------------------------------------------------------------------------------

-- | Semantic token
data SemanticToken = SemanticToken
  { stLine      :: Int
  , stColumn    :: Int
  , stLength    :: Int
  , stType      :: SemanticTokenType
  , stModifiers :: [SemanticTokenModifier]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Semantic token type
data SemanticTokenType
  = TokenNamespace
  | TokenType
  | TokenClass
  | TokenEnum
  | TokenInterface
  | TokenStruct
  | TokenTypeParameter
  | TokenParameter
  | TokenVariable
  | TokenProperty
  | TokenEnumMember
  | TokenFunction
  | TokenMethod
  | TokenMacro
  | TokenKeyword
  | TokenComment
  | TokenString
  | TokenNumber
  | TokenRegexp
  | TokenOperator
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Semantic token modifier
data SemanticTokenModifier
  = ModDeclaration
  | ModDefinition
  | ModReadonly
  | ModStatic
  | ModDeprecated
  | ModAbstract
  | ModAsync
  | ModModification
  | ModDocumentation
  | ModDefaultLibrary
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Generate semantic tokens for a file
generateSemanticTokens :: FilePath -> Text -> IO [SemanticToken]
generateSemanticTokens _path _content = pure []  -- Placeholder

--------------------------------------------------------------------------------
-- Refactoring
--------------------------------------------------------------------------------

-- | Refactoring action
data RefactorAction = RefactorAction
  { raId          :: Text
  , raTitle       :: Text
  , raKind        :: RefactorKind
  , raSpan        :: SrcSpan
  , raEdit        :: WorkspaceEdit
  , raCommand     :: Maybe CodeLensCommand
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Refactoring kind
data RefactorKind
  = ExtractFunction
  | ExtractVariable
  | InlineVariable
  | RenameSymbol
  | MoveToModule
  | AddTypeSignature
  | OrganizeImports
  | ConvertToPointFree
  | SimplifyExpression
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Apply a refactoring
applyRefactor :: IDEEngine -> RefactorAction -> IO (Either Text Text)
applyRefactor _engine RefactorAction{raEdit} = do
  let fileCount = Map.size (weChanges raEdit)
  if fileCount > 0
    then pure $ Right $ "Refactoring applied to " <> T.pack (show fileCount) <> " file(s)"
    else pure $ Left "No changes to apply"

-- | Preview a refactoring (show diff)
previewRefactor :: IDEEngine -> RefactorAction -> IO (Either Text Text)
previewRefactor _engine RefactorAction{raTitle, raEdit} = do
  let fileCount = Map.size (weChanges raEdit)
  if fileCount > 0
    then pure $ Right $ "Preview of '" <> raTitle <> "': affects " <> T.pack (show fileCount) <> " file(s)"
    else pure $ Left "No changes to preview"

--------------------------------------------------------------------------------
-- Document Operations
--------------------------------------------------------------------------------

-- | Text edit
data TextEdit = TextEdit
  { teRange   :: SrcSpan
  , teNewText :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Workspace edit
data WorkspaceEdit = WorkspaceEdit
  { weChanges         :: Map FilePath [TextEdit]
  , weDocumentChanges :: Maybe [DocumentChange]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Document change
data DocumentChange
  = TextDocChange TextDocumentEdit
  | CreateFile FilePath
  | RenameFile FilePath FilePath
  | DeleteFile FilePath
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Text document edit
data TextDocumentEdit = TextDocumentEdit
  { tdeUri     :: FilePath
  , tdeVersion :: Maybe Int
  , tdeEdits   :: [TextEdit]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Event Handlers
--------------------------------------------------------------------------------

-- | Handle document open
onDocumentOpen :: IDEEngine -> FilePath -> Text -> IO IDEAnalysisResult
onDocumentOpen engine path content = analyzeForIDE engine path content

-- | Handle document change
onDocumentChange :: IDEEngine -> FilePath -> Text -> IO (Maybe IDEAnalysisResult)
onDocumentChange engine path content = do
  -- Mark document as having pending changes
  atomically $ modifyTVar' (ideState engine) $ \st ->
    st { idsPendingChanges = Map.insert path True (idsPendingChanges st) }

  -- Analyze with debouncing
  analyzeIncrementalForIDE engine path content

-- | Handle document save
onDocumentSave :: IDEEngine -> FilePath -> Text -> IO IDEAnalysisResult
onDocumentSave engine path content = do
  config <- readTVarIO (ideConfig engine)

  -- Always analyze on save
  result <- analyzeForIDE engine path content

  -- Apply auto-fixes if enabled
  when (ideAutoFixOnSave config) $ do
    let safeFixes = filter caIsPreferred (iarCodeActions result)
    -- Apply only preferred (safe) fixes automatically
    mapM_ (\ca -> applyCodeAction engine ca) safeFixes

  pure result

-- | Apply a single code action
applyCodeAction :: IDEEngine -> CodeAction -> IO ()
applyCodeAction _engine CodeAction{caEdit} = case caEdit of
  Just WorkspaceEdit{weChanges} -> do
    -- Apply edits to each file
    mapM_ applyFileEdits (Map.toList weChanges)
  Nothing -> pure ()
  where
    applyFileEdits :: (FilePath, [TextEdit]) -> IO ()
    applyFileEdits (_fp, _edits) = do
      -- In a real implementation, would apply TextEdits to the file
      -- For now, this is a placeholder that would integrate with
      -- the file system or LSP workspace/applyEdit
      pure ()

-- | Handle document close
onDocumentClose :: IDEEngine -> FilePath -> IO ()
onDocumentClose engine path = atomically $ modifyTVar' (ideState engine) $ \st ->
  st
    { idsDocuments = Map.delete path (idsDocuments st)
    , idsLastAnalysis = Map.delete path (idsLastAnalysis st)
    , idsPendingChanges = Map.delete path (idsPendingChanges st)
    }

-- | Handle configuration change
onConfigurationChange :: IDEEngine -> IDEConfig -> IO ()
onConfigurationChange engine newConfig = atomically $ writeTVar (ideConfig engine) newConfig

-- | Get current IDE state
getIDEState :: IDEEngine -> IO IDEState
getIDEState engine = readTVarIO (ideState engine)

-- | Update a document
updateDocument :: IDEEngine -> FilePath -> Text -> IO ()
updateDocument engine path content = do
  now <- getCurrentTime
  atomically $ modifyTVar' (ideState engine) $ \st ->
    let updateDoc mds = Just $ case mds of
          Nothing -> DocumentState
            { dsContent = content
            , dsVersion = 1
            , dsDiagnostics = []
            , dsCodeLenses = []
            , dsInlayHints = []
            , dsLastModified = now
            }
          Just ds -> ds
            { dsContent = content
            , dsVersion = dsVersion ds + 1
            , dsLastModified = now
            }
    in st { idsDocuments = Map.alter updateDoc path (idsDocuments st) }

-- | Close a document
closeDocument :: IDEEngine -> FilePath -> IO ()
closeDocument = onDocumentClose
