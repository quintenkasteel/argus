{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.LSP.Hover
-- Description : Enhanced hover information with type info, documentation, and diagnostics
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides comprehensive hover functionality for the Argus LSP server:
--
-- * Type information from HIE database
-- * Inline documentation extraction
-- * Diagnostic information at hover position
-- * Symbol kind and definition location
-- * Cross-module type information
--
-- == Usage
--
-- @
-- hoverInfo <- getHoverInfo engine filePath position content
-- case hoverInfo of
--   Just hover -> sendHoverResponse hover
--   Nothing -> sendNullResponse
-- @
module Argus.LSP.Hover
  ( -- * Hover Engine
    HoverEngine (..)
  , newHoverEngine
  , HoverConfig (..)
  , defaultHoverConfig

    -- * Hover Information
  , HoverInfo (..)
  , HoverSection (..)
  , getHoverInfo
  , getHoverAtPosition

    -- * Type Information
  , TypeHover (..)
  , getTypeAtPosition
  , formatTypeForHover

    -- * Documentation
  , DocHover (..)
  , extractDocumentation
  , formatDocForHover

    -- * Symbol Information
  , SymbolHover (..)
  , getSymbolAtPosition
  , formatSymbolForHover

    -- * Diagnostic Hover
  , DiagnosticHover (..)
  , getDiagnosticsAtPosition
  , formatDiagnosticsForHover

    -- * Conversion
  , hoverToLsp
  ) where

import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe, listToMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Argus.Types
  ( Diagnostic(..)
  , Severity(..)
  , DiagnosticKind(..)
  , SrcSpan(..)
  , Line(..)
  , Column(..)
  , SymbolKind(..)
  )
import Argus.HIE.Types (TypeInfo(..))
import Argus.HIE.Query (withHieQuery, getSymbolType, findSymbolDefinition)
import Argus.LSP.RuleInfo (RuleInfo(..), getRuleExplanation, getRuleExplanationByCode, formatRuleInfo)

--------------------------------------------------------------------------------
-- Hover Configuration
--------------------------------------------------------------------------------

-- | Hover engine configuration
data HoverConfig = HoverConfig
  { hcShowTypes        :: Bool       -- ^ Show type information
  , hcShowDocs         :: Bool       -- ^ Show documentation
  , hcShowDiagnostics  :: Bool       -- ^ Show diagnostics at position
  , hcShowDefinition   :: Bool       -- ^ Show definition location
  , hcShowModule       :: Bool       -- ^ Show module information
  , hcMaxDocLines      :: Int        -- ^ Maximum documentation lines
  , hcTypeFormatWidth  :: Int        -- ^ Maximum type width before wrapping
  , hcHieDbPath        :: FilePath   -- ^ Path to HIE database
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default hover configuration
defaultHoverConfig :: HoverConfig
defaultHoverConfig = HoverConfig
  { hcShowTypes       = True
  , hcShowDocs        = True
  , hcShowDiagnostics = True
  , hcShowDefinition  = True
  , hcShowModule      = True
  , hcMaxDocLines     = 20
  , hcTypeFormatWidth = 80
  , hcHieDbPath       = ".hie/.hiedb"
  }

--------------------------------------------------------------------------------
-- Hover Engine
--------------------------------------------------------------------------------

-- | Hover engine with caching
data HoverEngine = HoverEngine
  { heConfig     :: HoverConfig
  , heTypeCache  :: TVar (Map (FilePath, Int, Int) (Maybe TypeHover))
  , heDocCache   :: TVar (Map Text DocHover)
  }

-- | Create a new hover engine
newHoverEngine :: HoverConfig -> IO HoverEngine
newHoverEngine config = do
  typeCache <- newTVarIO Map.empty
  docCache <- newTVarIO Map.empty
  pure HoverEngine
    { heConfig    = config
    , heTypeCache = typeCache
    , heDocCache  = docCache
    }

--------------------------------------------------------------------------------
-- Hover Information Types
--------------------------------------------------------------------------------

-- | Complete hover information
data HoverInfo = HoverInfo
  { hiSections :: [HoverSection]   -- ^ Sections to display
  , hiRange    :: Maybe SrcSpan    -- ^ Range of the hovered element
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A section of hover content
data HoverSection
  = TypeSection TypeHover
  | DocSection DocHover
  | DiagSection DiagnosticHover
  | SymbolSection SymbolHover
  | CodeSection Text              -- ^ Code snippet
  | MarkdownSection Text          -- ^ Raw markdown content
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Type information for hover
data TypeHover = TypeHover
  { thName       :: Text           -- ^ Symbol name
  , thType       :: Text           -- ^ Type signature
  , thKind       :: Maybe Text     -- ^ Kind (for type-level entities)
  , thModule     :: Maybe Text     -- ^ Defining module
  , thIsInferred :: Bool           -- ^ Whether type is inferred
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Documentation for hover
data DocHover = DocHover
  { dhSummary    :: Text           -- ^ Short summary
  , dhDetails    :: Maybe Text     -- ^ Detailed documentation
  , dhExamples   :: [Text]         -- ^ Usage examples
  , dhSince      :: Maybe Text     -- ^ Version since available
  , dhSeeAlso    :: [Text]         -- ^ Related symbols
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Symbol information for hover
data SymbolHover = SymbolHover
  { shName       :: Text
  , shKind       :: SymbolKind
  , shModule     :: Text
  , shDefinedAt  :: Maybe SrcSpan
  , shExported   :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Diagnostic information for hover
data DiagnosticHover = DiagnosticHover
  { dthDiagnostics :: [DiagnosticInfo]
  , dthFixCount    :: Int
  , dthRuleInfo    :: Maybe RuleInfo  -- ^ Rule explanation if available
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Simplified diagnostic for hover
data DiagnosticInfo = DiagnosticInfo
  { diSeverity :: Severity
  , diKind     :: DiagnosticKind
  , diMessage  :: Text
  , diCode     :: Maybe Text
  , diHasFix   :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Main Hover Functions
--------------------------------------------------------------------------------

-- | Get hover information at a position
getHoverInfo :: HoverEngine
             -> FilePath
             -> (Int, Int)           -- ^ (line, column) 1-indexed
             -> Text                 -- ^ File content
             -> [Diagnostic]         -- ^ Current diagnostics
             -> IO (Maybe HoverInfo)
getHoverInfo engine path (line, col) content diagnostics = do
  let config = heConfig engine

  -- Gather hover sections
  sections <- catMaybes <$> sequence
    [ if hcShowTypes config
      then fmap TypeSection <$> getTypeAtPosition engine path line col content
      else pure Nothing
    , if hcShowDocs config
      then fmap DocSection <$> getDocAtPosition engine path line col content
      else pure Nothing
    , if hcShowDiagnostics config
      then getDiagSectionAtPosition config line col diagnostics
      else pure Nothing
    , if hcShowDefinition config
      then fmap SymbolSection <$> getSymbolAtPosition engine path line col content
      else pure Nothing
    ]

  if null sections
    then pure Nothing
    else do
      -- Try to get range for the hovered symbol
      let symbolRange = extractSymbolRange content line col
      pure $ Just HoverInfo
        { hiSections = sections
        , hiRange    = symbolRange
        }

-- | Simplified hover function
getHoverAtPosition :: HoverEngine
                   -> FilePath
                   -> Int              -- ^ Line (1-indexed)
                   -> Int              -- ^ Column (1-indexed)
                   -> Text             -- ^ Content
                   -> [Diagnostic]
                   -> IO (Maybe HoverInfo)
getHoverAtPosition engine path line col content diags =
  getHoverInfo engine path (line, col) content diags

--------------------------------------------------------------------------------
-- Type Information
--------------------------------------------------------------------------------

-- | Get type information at position
getTypeAtPosition :: HoverEngine
                  -> FilePath
                  -> Int
                  -> Int
                  -> Text
                  -> IO (Maybe TypeHover)
getTypeAtPosition engine path line col content = do
  -- Check cache first
  cached <- atomically $ Map.lookup (path, line, col) <$> readTVar (heTypeCache engine)
  case cached of
    Just result -> pure result
    Nothing -> do
      result <- computeTypeAtPosition engine path line col content
      -- Cache the result
      atomically $ modifyTVar' (heTypeCache engine) $ Map.insert (path, line, col) result
      pure result

-- | Compute type information (not cached)
computeTypeAtPosition :: HoverEngine
                      -> FilePath
                      -> Int
                      -> Int
                      -> Text
                      -> IO (Maybe TypeHover)
computeTypeAtPosition engine _path line col content = do
  let config = heConfig engine
      dbPath = hcHieDbPath config

  -- Extract symbol name at position
  let maybeSymbol = extractSymbolAtPos content line col

  case maybeSymbol of
    Nothing -> pure Nothing
    Just symbolName -> do
      -- Try to get type from HIE database
      result <- try @SomeException $ withHieQuery dbPath $ do
        mTypeInfo <- getSymbolType symbolName Nothing
        pure mTypeInfo

      case result of
        Left _ -> do
          -- Fallback: try to extract type from source
          pure $ extractTypeFromSource content symbolName line
        Right Nothing ->
          pure $ extractTypeFromSource content symbolName line
        Right (Just typeInfo) ->
          pure $ Just $ TypeHover
            { thName       = symbolName
            , thType       = tiType typeInfo
            , thKind       = Just $ tiKind typeInfo
            , thModule     = Nothing  -- TypeInfo doesn't include module
            , thIsInferred = False
            }

-- | Extract type signature from source code
extractTypeFromSource :: Text -> Text -> Int -> Maybe TypeHover
extractTypeFromSource content symbolName _line =
  let allLines = zip ([1..] :: [Int]) (T.lines content)
      -- Look for type signature: name :: Type
      findTypeSig = listToMaybe $ catMaybes
        [ if symbolName `T.isPrefixOf` T.stripStart l && " :: " `T.isInfixOf` l
          then Just $ T.strip $ T.drop (T.length symbolName) $ snd $ T.breakOn "::" l
          else Nothing
        | (_, l) <- allLines
        ]
  in case findTypeSig of
       Nothing -> Nothing
       Just typeSig -> Just TypeHover
         { thName       = symbolName
         , thType       = T.drop 3 typeSig  -- Remove " :: "
         , thKind       = Nothing
         , thModule     = Nothing
         , thIsInferred = True
         }

-- | Format type for hover display
formatTypeForHover :: TypeHover -> Text
formatTypeForHover TypeHover{..} =
  let header = "```haskell\n" <> thName <> " :: " <> thType <> "\n```"
      moduleInfo = case thModule of
        Nothing -> ""
        Just m  -> "\n*Defined in* `" <> m <> "`"
      inferredNote = if thIsInferred
        then "\n*(inferred)*"
        else ""
  in header <> moduleInfo <> inferredNote

--------------------------------------------------------------------------------
-- Documentation
--------------------------------------------------------------------------------

-- | Get documentation at position
getDocAtPosition :: HoverEngine
                 -> FilePath
                 -> Int
                 -> Int
                 -> Text
                 -> IO (Maybe DocHover)
getDocAtPosition _engine _path line col content = do
  let maybeSymbol = extractSymbolAtPos content line col
  case maybeSymbol of
    Nothing -> pure Nothing
    Just symbolName -> pure $ extractDocumentation content symbolName line

-- | Extract documentation from source
extractDocumentation :: Text -> Text -> Int -> Maybe DocHover
extractDocumentation content _symbolName targetLine =
  let allLines = zip ([1..] :: [Int]) (T.lines content)
      -- Look for Haddock comments above the definition
      docLines = findDocAbove allLines targetLine
  in if null docLines
     then Nothing
     else Just DocHover
       { dhSummary  = T.intercalate " " $ take 3 docLines
       , dhDetails  = if length docLines > 3
                      then Just $ T.intercalate "\n" $ drop 3 docLines
                      else Nothing
       , dhExamples = findExamples docLines
       , dhSince    = findSince docLines
       , dhSeeAlso  = findSeeAlso docLines
       }

-- | Find documentation lines above a position
findDocAbove :: [(Int, Text)] -> Int -> [Text]
findDocAbove allLines targetLine =
  let linesAbove = reverse $ takeWhile ((< targetLine) . fst) allLines
      -- Look for Haddock comments (-- | or -- ^)
      docStart = dropWhile (not . isHaddockLine . snd) linesAbove
      docContent = takeWhile (isDocOrEmpty . snd) docStart
  in map (cleanDocLine . snd) $ reverse docContent
  where
    isHaddockLine l =
      let stripped = T.stripStart l
      in "-- |" `T.isPrefixOf` stripped || "-- ^" `T.isPrefixOf` stripped

    isDocOrEmpty l =
      let stripped = T.stripStart l
      in T.null stripped ||
         "--" `T.isPrefixOf` stripped ||
         "{-" `T.isPrefixOf` stripped

    cleanDocLine l =
      let stripped = T.stripStart l
      in if "-- |" `T.isPrefixOf` stripped
         then T.strip $ T.drop 4 stripped
         else if "-- ^" `T.isPrefixOf` stripped
         then T.strip $ T.drop 4 stripped
         else if "--" `T.isPrefixOf` stripped
         then T.strip $ T.drop 2 stripped
         else T.strip stripped

-- | Find examples in documentation
findExamples :: [Text] -> [Text]
findExamples docs =
  let inExample = False
      collectExamples _ [] = []
      collectExamples False (l:ls)
        | ">>>" `T.isPrefixOf` T.stripStart l = l : collectExamples True ls
        | otherwise = collectExamples False ls
      collectExamples True (l:ls)
        | ">>>" `T.isPrefixOf` T.stripStart l = l : collectExamples True ls
        | T.null (T.strip l) = collectExamples False ls
        | otherwise = l : collectExamples True ls
  in collectExamples inExample docs

-- | Find @since annotation
findSince :: [Text] -> Maybe Text
findSince docs =
  listToMaybe $ catMaybes
    [ if "@since" `T.isInfixOf` l
      then Just $ T.strip $ snd $ T.breakOn "@since" l
      else Nothing
    | l <- docs
    ]

-- | Find @see also references
findSeeAlso :: [Text] -> [Text]
findSeeAlso docs =
  concatMap extractRefs docs
  where
    extractRefs l
      | "See also" `T.isInfixOf` l || "@see" `T.isInfixOf` l =
          let afterSee = snd $ T.breakOnEnd ":" l
          in map T.strip $ T.splitOn "," afterSee
      | otherwise = []

-- | Format documentation for hover
formatDocForHover :: DocHover -> Text
formatDocForHover DocHover{..} =
  let summary = dhSummary
      details = fromMaybe "" dhDetails
      examples = if null dhExamples
        then ""
        else "\n\n**Examples:**\n```haskell\n" <> T.intercalate "\n" dhExamples <> "\n```"
      since = case dhSince of
        Nothing -> ""
        Just v  -> "\n\n*Since:* " <> v
      seeAlso = if null dhSeeAlso
        then ""
        else "\n\n*See also:* " <> T.intercalate ", " (map (\s -> "`" <> s <> "`") dhSeeAlso)
  in summary <>
     (if T.null details then "" else "\n\n" <> details) <>
     examples <> since <> seeAlso

--------------------------------------------------------------------------------
-- Symbol Information
--------------------------------------------------------------------------------

-- | Get symbol information at position
getSymbolAtPosition :: HoverEngine
                    -> FilePath
                    -> Int
                    -> Int
                    -> Text
                    -> IO (Maybe SymbolHover)
getSymbolAtPosition engine path line col content = do
  let config = heConfig engine
      dbPath = hcHieDbPath config
      maybeSymbol = extractSymbolAtPos content line col

  case maybeSymbol of
    Nothing -> pure Nothing
    Just symbolName -> do
      result <- try @SomeException $ withHieQuery dbPath $ do
        mDefSpan <- findSymbolDefinition symbolName Nothing
        pure mDefSpan

      case result of
        Left _ ->
          -- Fallback to source-based detection
          pure $ extractSymbolFromSource content symbolName line
        Right mSpan ->
          pure $ Just SymbolHover
            { shName      = symbolName
            , shKind      = detectSymbolKind content symbolName
            , shModule    = T.pack path
            , shDefinedAt = mSpan
            , shExported  = True  -- Would need module analysis for accuracy
            }

-- | Extract symbol info from source
extractSymbolFromSource :: Text -> Text -> Int -> Maybe SymbolHover
extractSymbolFromSource content symbolName _line =
  Just SymbolHover
    { shName      = symbolName
    , shKind      = detectSymbolKind content symbolName
    , shModule    = "(local)"
    , shDefinedAt = Nothing
    , shExported  = False
    }

-- | Detect symbol kind from context
detectSymbolKind :: Text -> Text -> SymbolKind
detectSymbolKind content symbolName
  | isUpperCase (T.head symbolName) = detectUpperCaseKind content symbolName
  | otherwise = Function
  where
    isUpperCase c = c >= 'A' && c <= 'Z'

    detectUpperCaseKind src name
      | ("data " <> name) `T.isInfixOf` src = DataConstructor
      | ("newtype " <> name) `T.isInfixOf` src = DataConstructor
      | ("type " <> name) `T.isInfixOf` src = TypeConstructor
      | ("class " <> name) `T.isInfixOf` src = TypeClass
      | otherwise = TypeConstructor

-- | Format symbol for hover
formatSymbolForHover :: SymbolHover -> Text
formatSymbolForHover SymbolHover{..} =
  let kindText = case shKind of
        Function        -> "function"
        TypeConstructor -> "type"
        DataConstructor -> "constructor"
        TypeClass       -> "class"
        TypeClassMethod -> "method"
        TypeFamily      -> "type family"
        PatternSynonym  -> "pattern"
        Module          -> "module"
      defLocation = case shDefinedAt of
        Nothing -> ""
        Just span' -> "\n*Defined at* " <> T.pack (srcSpanFile span') <>
                      ":" <> T.pack (show $ unLine $ srcSpanStartLine span')
  in "**" <> kindText <> "** `" <> shName <> "`" <>
     "\n*Module:* `" <> shModule <> "`" <>
     (if shExported then " (exported)" else "") <>
     defLocation

--------------------------------------------------------------------------------
-- Diagnostic Hover
--------------------------------------------------------------------------------

-- | Get diagnostics section at position
getDiagSectionAtPosition :: HoverConfig -> Int -> Int -> [Diagnostic] -> IO (Maybe HoverSection)
getDiagSectionAtPosition _config line col diagnostics = do
  let atPos = getDiagnosticsAtPosition line col diagnostics
  if null atPos
    then pure Nothing
    else do
      -- Try to get rule explanation from the first diagnostic
      let ruleInfo = case atPos of
            (d:_) -> diagCode d >>= getRuleExplanationByCode
            [] -> Nothing
      pure $ Just $ DiagSection $ DiagnosticHover
        { dthDiagnostics = map toDiagInfo atPos
        , dthFixCount = sum $ map (length . diagFixes) atPos
        , dthRuleInfo = ruleInfo
        }

-- | Get diagnostics at a specific position
getDiagnosticsAtPosition :: Int -> Int -> [Diagnostic] -> [Diagnostic]
getDiagnosticsAtPosition line col = filter (diagContainsPos line col)

-- | Check if diagnostic contains position
diagContainsPos :: Int -> Int -> Diagnostic -> Bool
diagContainsPos line col diag =
  let span' = diagSpan diag
      startLine = unLine $ srcSpanStartLine span'
      endLine = unLine $ srcSpanEndLine span'
      startCol = unColumn $ srcSpanStartCol span'
      endCol = unColumn $ srcSpanEndCol span'
  in line >= startLine && line <= endLine &&
     (line /= startLine || col >= startCol) &&
     (line /= endLine || col <= endCol)

-- | Convert to simplified diagnostic info
toDiagInfo :: Diagnostic -> DiagnosticInfo
toDiagInfo Diagnostic{..} = DiagnosticInfo
  { diSeverity = diagSeverity
  , diKind     = diagKind
  , diMessage  = diagMessage
  , diCode     = diagCode
  , diHasFix   = not $ null diagFixes
  }

-- | Format diagnostics for hover
formatDiagnosticsForHover :: DiagnosticHover -> Text
formatDiagnosticsForHover DiagnosticHover{..} =
  let diagTexts = map formatOneDiag dthDiagnostics
      fixNote = if dthFixCount > 0
        then "\n\n**" <> T.pack (show dthFixCount) <> " quick fix(es) available**"
        else ""
      ruleExplanation = case dthRuleInfo of
        Nothing -> ""
        Just info -> "\n\n---\n\n" <> formatRuleInfo info
  in T.intercalate "\n\n---\n\n" diagTexts <> fixNote <> ruleExplanation
  where
    formatOneDiag DiagnosticInfo{..} =
      let icon = case diSeverity of
            Error      -> ":x:"
            Warning    -> ":warning:"
            Suggestion -> ":bulb:"
            Info       -> ":information_source:"
          codeText = maybe "" (\c -> " `" <> c <> "`") diCode
      in icon <> " **" <> kindToText diKind <> "**" <> codeText <> "\n\n" <> diMessage

    kindToText = \case
      NamingConvention   -> "Naming"
      UnusedCode         -> "Unused"
      UnusedImport       -> "Unused Import"
      RedundantCode      -> "Redundant"
      CodePattern        -> "Pattern"
      TypeSignature      -> "Type"
      ImportStyle        -> "Import"
      TemplateHaskellRef -> "Template Haskell"
      SecurityIssue      -> "Security"
      PerformanceIssue   -> "Performance"
      ArchitecturalIssue -> "Architecture"
      SpaceLeak          -> "Space Leak"
      PartialFunction    -> "Partial Function"
      ComplexityIssue    -> "Complexity"
      Custom t           -> t

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

-- | Extract symbol at position from source text
extractSymbolAtPos :: Text -> Int -> Int -> Maybe Text
extractSymbolAtPos content line col =
  let allLines = T.lines content
  in if line > 0 && line <= length allLines
     then
       let lineText = allLines !! (line - 1)
           col' = col - 1  -- Convert to 0-indexed
       in extractWordAt lineText col'
     else Nothing

-- | Extract word at column position
extractWordAt :: Text -> Int -> Maybe Text
extractWordAt lineText col
  | col < 0 || col >= T.length lineText = Nothing
  | otherwise =
      let before = T.take col lineText
          after = T.drop col lineText
          wordStart = T.takeWhileEnd isIdentChar before
          wordEnd = T.takeWhile isIdentChar after
          word = wordStart <> wordEnd
      in if T.null word then Nothing else Just word
  where
    isIdentChar c = c == '_' || c == '\'' ||
                    (c >= 'a' && c <= 'z') ||
                    (c >= 'A' && c <= 'Z') ||
                    (c >= '0' && c <= '9')

-- | Extract symbol range for hover highlighting
extractSymbolRange :: Text -> Int -> Int -> Maybe SrcSpan
extractSymbolRange content line col =
  let allLines = T.lines content
  in if line > 0 && line <= length allLines
     then
       let lineText = allLines !! (line - 1)
           col' = col - 1
           before = T.take col' lineText
           after = T.drop col' lineText
           startCol = col' - T.length (T.takeWhileEnd isIdentChar before)
           endCol = col' + T.length (T.takeWhile isIdentChar after)
       in if startCol < endCol
          then Just SrcSpan
            { srcSpanFile = ""
            , srcSpanStartLine = Line line
            , srcSpanStartCol = Column (startCol + 1)
            , srcSpanEndLine = Line line
            , srcSpanEndCol = Column (endCol + 1)
            }
          else Nothing
     else Nothing
  where
    isIdentChar c = c == '_' || c == '\'' ||
                    (c >= 'a' && c <= 'z') ||
                    (c >= 'A' && c <= 'Z') ||
                    (c >= '0' && c <= '9')

--------------------------------------------------------------------------------
-- LSP Conversion
--------------------------------------------------------------------------------

-- | Convert hover info to LSP format
hoverToLsp :: HoverInfo -> Aeson.Value
hoverToLsp HoverInfo{..} =
  let content = T.intercalate "\n\n---\n\n" $ map sectionToMarkdown hiSections
      rangeObj = case hiRange of
        Nothing -> []
        Just span' -> [("range", spanToLspRange span')]
  in object $
       [ "contents" .= object
           [ "kind" .= ("markdown" :: Text)
           , "value" .= content
           ]
       ] ++ rangeObj

-- | Convert section to markdown
sectionToMarkdown :: HoverSection -> Text
sectionToMarkdown = \case
  TypeSection th -> formatTypeForHover th
  DocSection dh -> formatDocForHover dh
  DiagSection dth -> formatDiagnosticsForHover dth
  SymbolSection sh -> formatSymbolForHover sh
  CodeSection code -> "```haskell\n" <> code <> "\n```"
  MarkdownSection md -> md

-- | Convert SrcSpan to LSP range
spanToLspRange :: SrcSpan -> Aeson.Value
spanToLspRange SrcSpan{..} = object
  [ "start" .= object
      [ "line" .= (unLine srcSpanStartLine - 1)
      , "character" .= (unColumn srcSpanStartCol - 1)
      ]
  , "end" .= object
      [ "line" .= (unLine srcSpanEndLine - 1)
      , "character" .= (unColumn srcSpanEndCol - 1)
      ]
  ]
