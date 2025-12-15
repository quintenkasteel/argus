{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.LSP.Diagnostics
-- Description : Enhanced LSP diagnostics with code actions and quick fixes
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides enhanced diagnostic handling for the Argus LSP server:
--
-- * Rich diagnostic conversion with structured data
-- * Code action generation with multiple fix types
-- * Diagnostic aggregation and deduplication
-- * Diagnostic filtering and prioritization
-- * Quick fix preview generation
-- * Batch fix support for "fix all" actions
--
-- == Usage
--
-- @
-- diags <- analyzeAndEnhance ctx filePath content
-- let actions = generateCodeActions filePath diags range
-- @
module Argus.LSP.Diagnostics
  ( -- * Enhanced Diagnostic Types
    EnhancedDiagnostic (..)
  , DiagnosticCategory (..)
  , DiagnosticData (..)

    -- * Diagnostic Enhancement
  , enhanceDiagnostic
  , enhanceDiagnostics

    -- * Code Action Generation
  , CodeActionContext (..)
  , CodeActionResult (..)
  , generateCodeActions
  , generateFixAllActions
  , generateCategoryActions

    -- * Diagnostic Aggregation
  , DiagnosticSummary (..)
  , aggregateDiagnostics
  , groupByCategory
  , groupByRule

    -- * Diagnostic Filtering
  , DiagnosticFilter (..)
  , applyFilter
  , filterBySeverity
  , filterByCategory
  , filterByRange

    -- * Quick Fix Preview
  , FixPreview (..)
  , generatePreview
  , generateBatchPreview

    -- * Conversion
  , toEnhancedLspDiagnostic
  , fromEnhancedToLsp
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON(..), FromJSON(..), object, (.=))
import Data.Aeson qualified as Aeson
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Ord (comparing, Down(..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Argus.Types
  ( Diagnostic(..)
  , Severity(..)
  , DiagnosticKind(..)
  , SrcSpan(..)
  , Fix(..)
  , FixEdit(..)
  , Line(..)
  , Column(..)
  )
import Argus.Refactor.ExactPrint (applyFix)
import Argus.LSP.RuleInfo (RuleInfo(..), getRuleExplanationByCode, formatShortSummary)

--------------------------------------------------------------------------------
-- Enhanced Diagnostic Types
--------------------------------------------------------------------------------

-- | Enhanced diagnostic with additional LSP-specific metadata
data EnhancedDiagnostic = EnhancedDiagnostic
  { edOriginal    :: Diagnostic        -- ^ Original Argus diagnostic
  , edCategory    :: DiagnosticCategory -- ^ High-level category
  , edPriority    :: Int               -- ^ Priority (higher = more important)
  , edRelatedInfo :: [RelatedInfo]     -- ^ Related locations/info
  , edTags        :: [DiagnosticTag]   -- ^ LSP diagnostic tags
  , edData        :: Maybe DiagnosticData -- ^ Structured data for code actions
  , edRuleInfo    :: Maybe RuleInfo    -- ^ Rule explanation for hover tooltips
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | High-level diagnostic categories
data DiagnosticCategory
  = CatCodeQuality      -- ^ General code quality issues
  | CatPerformance      -- ^ Performance-related issues
  | CatSecurity         -- ^ Security vulnerabilities
  | CatStyle            -- ^ Style/formatting issues
  | CatUnused           -- ^ Unused code
  | CatPartial          -- ^ Partial function usage
  | CatNaming           -- ^ Naming convention issues
  | CatImports          -- ^ Import-related issues
  | CatTypes            -- ^ Type-related issues
  | CatComplexity       -- ^ Code complexity issues
  | CatRefactoring      -- ^ Refactoring suggestions
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON, Aeson.ToJSONKey, Aeson.FromJSONKey)

-- | Related diagnostic information
data RelatedInfo = RelatedInfo
  { riLocation :: SrcSpan
  , riMessage  :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | LSP diagnostic tags
data DiagnosticTag
  = TagUnnecessary  -- ^ 1 - Unused/redundant code
  | TagDeprecated   -- ^ 2 - Deprecated API usage
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

-- | Structured data for code action resolution
data DiagnosticData = DiagnosticData
  { ddRuleId       :: Maybe Text    -- ^ Rule that triggered this diagnostic
  , ddFixIds       :: [Text]        -- ^ IDs of available fixes
  , ddIsAutofix    :: Bool          -- ^ Can be auto-fixed
  , ddConflicts    :: [Text]        -- ^ IDs of conflicting diagnostics
  , ddBatchable    :: Bool          -- ^ Can be fixed in batch with similar issues
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Diagnostic Enhancement
--------------------------------------------------------------------------------

-- | Enhance a single diagnostic with additional metadata
enhanceDiagnostic :: Diagnostic -> EnhancedDiagnostic
enhanceDiagnostic diag = EnhancedDiagnostic
  { edOriginal    = diag
  , edCategory    = categorize (diagKind diag)
  , edPriority    = calculatePriority diag
  , edRelatedInfo = map toRelatedInfo (diagRelated diag)
  , edTags        = determineTags (diagKind diag)
  , edData        = Just $ extractDiagData diag
  , edRuleInfo    = diagCode diag >>= getRuleExplanationByCode
  }

-- | Enhance multiple diagnostics
enhanceDiagnostics :: [Diagnostic] -> [EnhancedDiagnostic]
enhanceDiagnostics = map enhanceDiagnostic

-- | Categorize a diagnostic kind into high-level category
categorize :: DiagnosticKind -> DiagnosticCategory
categorize = \case
  NamingConvention   -> CatNaming
  UnusedCode         -> CatUnused
  UnusedImport       -> CatUnused
  RedundantCode      -> CatCodeQuality
  CodePattern        -> CatRefactoring
  TypeSignature      -> CatTypes
  ImportStyle        -> CatImports
  TemplateHaskellRef -> CatCodeQuality
  SecurityIssue      -> CatSecurity
  PerformanceIssue   -> CatPerformance
  ArchitecturalIssue -> CatCodeQuality
  SpaceLeak          -> CatPerformance
  PartialFunction    -> CatPartial
  ComplexityIssue    -> CatComplexity
  Custom _           -> CatCodeQuality

-- | Calculate diagnostic priority (higher = more important)
calculatePriority :: Diagnostic -> Int
calculatePriority diag =
  severityPriority (diagSeverity diag) + kindPriority (diagKind diag) + fixBonus
  where
    severityPriority Error      = 1000
    severityPriority Warning    = 100
    severityPriority Suggestion = 10
    severityPriority Info       = 1

    kindPriority SecurityIssue     = 500
    kindPriority PartialFunction   = 200
    kindPriority PerformanceIssue  = 100
    kindPriority SpaceLeak         = 100
    kindPriority _                 = 0

    fixBonus = if null (diagFixes diag) then 0 else 50

-- | Convert related location/message tuple to related info
toRelatedInfo :: (SrcSpan, Text) -> RelatedInfo
toRelatedInfo (span', msg) = RelatedInfo
  { riLocation = span'
  , riMessage  = msg
  }

-- | Determine LSP diagnostic tags
determineTags :: DiagnosticKind -> [DiagnosticTag]
determineTags = \case
  UnusedCode    -> [TagUnnecessary]
  UnusedImport  -> [TagUnnecessary]
  RedundantCode -> [TagUnnecessary]
  _             -> []

-- | Extract structured data from diagnostic
extractDiagData :: Diagnostic -> DiagnosticData
extractDiagData diag = DiagnosticData
  { ddRuleId    = diagCode diag
  , ddFixIds    = zipWith (\i f -> fromMaybe (T.pack $ "fix-" ++ show i) (Just $ fixTitle f))
                          [0 :: Int ..] (diagFixes diag)
  , ddIsAutofix = any fixIsPreferred (diagFixes diag)
  , ddConflicts = []  -- Would need conflict detection context
  , ddBatchable = isBatchable (diagKind diag)
  }
  where
    isBatchable UnusedImport = True
    isBatchable UnusedCode   = True
    isBatchable ImportStyle  = True
    isBatchable _            = False

--------------------------------------------------------------------------------
-- Code Action Generation
--------------------------------------------------------------------------------

-- | Context for generating code actions
data CodeActionContext = CodeActionContext
  { cacFilePath    :: FilePath
  , cacFileContent :: Text
  , cacRange       :: (SrcSpan, SrcSpan)  -- ^ Start and end of selection
  , cacTriggerKind :: CodeActionTrigger
  , cacOnlyKinds   :: Maybe [Text]        -- ^ Filter to specific action kinds
  }
  deriving stock (Eq, Show, Generic)

-- | What triggered the code action request
data CodeActionTrigger
  = TriggerInvoked     -- ^ Manually invoked (Ctrl+.)
  | TriggerAutomatic   -- ^ Automatic (lightbulb)
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

-- | Result of code action generation
data CodeActionResult = CodeActionResult
  { carQuickFixes    :: [QuickFixAction]     -- ^ Individual quick fixes
  , carRefactors     :: [RefactorAction]     -- ^ Refactoring actions
  , carSourceActions :: [SourceAction]       -- ^ Source-level actions
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Quick fix action
data QuickFixAction = QuickFixAction
  { qfaTitle       :: Text
  , qfaDiagnostic  :: EnhancedDiagnostic
  , qfaFix         :: Fix
  , qfaIsPreferred :: Bool
  , qfaEdits       :: [TextEdit]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Refactoring action (larger scale changes)
data RefactorAction = RefactorAction
  { raTitle       :: Text
  , raKind        :: Text         -- ^ e.g., "refactor.extract", "refactor.inline"
  , raDescription :: Maybe Text
  , raEdits       :: [TextEdit]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Source action (file-level operations)
data SourceAction = SourceAction
  { saTitle   :: Text
  , saKind    :: Text         -- ^ e.g., "source.organizeImports"
  , saCommand :: Maybe CommandInfo
  , saEdits   :: [TextEdit]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Command information
data CommandInfo = CommandInfo
  { ciCommand   :: Text
  , ciTitle     :: Text
  , ciArguments :: [Aeson.Value]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Text edit for workspace changes
data TextEdit = TextEdit
  { teRange   :: SrcSpan
  , teNewText :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Generate code actions for diagnostics in a range
generateCodeActions :: CodeActionContext -> [EnhancedDiagnostic] -> CodeActionResult
generateCodeActions ctx diags =
  let inRange = filter (diagInRange ctx) diags
      quickFixes = concatMap (generateQuickFixes ctx) inRange
      refactors = generateRefactors ctx inRange
      sourceActions = generateSourceActions ctx inRange
  in CodeActionResult
       { carQuickFixes    = prioritizeQuickFixes quickFixes
       , carRefactors     = refactors
       , carSourceActions = sourceActions
       }

-- | Check if diagnostic is in the code action range
diagInRange :: CodeActionContext -> EnhancedDiagnostic -> Bool
diagInRange CodeActionContext{..} ed =
  let diagSpan' = diagSpan (edOriginal ed)
      (startRange, endRange) = cacRange
  in spansOverlap diagSpan' startRange || spansOverlap diagSpan' endRange

-- | Check if two spans overlap
spansOverlap :: SrcSpan -> SrcSpan -> Bool
spansOverlap s1 s2 =
  not (spanBefore s1 s2 || spanBefore s2 s1)
  where
    spanBefore a b =
      srcSpanEndLine a < srcSpanStartLine b ||
      (srcSpanEndLine a == srcSpanStartLine b && srcSpanEndCol a <= srcSpanStartCol b)

-- | Generate quick fixes for a single diagnostic
generateQuickFixes :: CodeActionContext -> EnhancedDiagnostic -> [QuickFixAction]
generateQuickFixes CodeActionContext{..} ed =
  let diag = edOriginal ed
  in map (fixToQuickFix cacFilePath ed) (diagFixes diag)

-- | Convert a Fix to a QuickFixAction
fixToQuickFix :: FilePath -> EnhancedDiagnostic -> Fix -> QuickFixAction
fixToQuickFix _path ed fix = QuickFixAction
  { qfaTitle       = fixTitle fix
  , qfaDiagnostic  = ed
  , qfaFix         = fix
  , qfaIsPreferred = fixIsPreferred fix
  , qfaEdits       = map fixEditToTextEdit (fixEdits fix)
  }

-- | Convert FixEdit to TextEdit
fixEditToTextEdit :: FixEdit -> TextEdit
fixEditToTextEdit FixEdit{..} = TextEdit
  { teRange   = fixEditSpan
  , teNewText = fixEditNewText
  }

-- | Prioritize quick fixes (preferred first, then by priority)
prioritizeQuickFixes :: [QuickFixAction] -> [QuickFixAction]
prioritizeQuickFixes = sortBy compareFixes
  where
    compareFixes a b =
      compare (Down $ qfaIsPreferred a) (Down $ qfaIsPreferred b) <>
      compare (Down $ edPriority $ qfaDiagnostic a) (Down $ edPriority $ qfaDiagnostic b)

-- | Generate refactoring actions
generateRefactors :: CodeActionContext -> [EnhancedDiagnostic] -> [RefactorAction]
generateRefactors _ctx diags =
  let categories = Set.fromList $ map edCategory diags
  in catMaybes
       [ if CatImports `Set.member` categories
         then Just RefactorAction
           { raTitle = "Organize imports"
           , raKind = "refactor.rewrite"
           , raDescription = Just "Sort and clean up import statements"
           , raEdits = []  -- Would be computed by import organizer
           }
         else Nothing
       ]

-- | Generate source-level actions
generateSourceActions :: CodeActionContext -> [EnhancedDiagnostic] -> [SourceAction]
generateSourceActions _ctx diags =
  let hasUnused = any ((== CatUnused) . edCategory) diags
      hasImportIssues = any ((== CatImports) . edCategory) diags
  in catMaybes
       [ if hasUnused
         then Just SourceAction
           { saTitle = "Remove all unused code"
           , saKind = "source.removeUnused"
           , saCommand = Just CommandInfo
               { ciCommand = "argus.removeAllUnused"
               , ciTitle = "Remove all unused code"
               , ciArguments = []
               }
           , saEdits = []
           }
         else Nothing
       , if hasImportIssues
         then Just SourceAction
           { saTitle = "Organize imports"
           , saKind = "source.organizeImports"
           , saCommand = Just CommandInfo
               { ciCommand = "argus.organizeImports"
               , ciTitle = "Organize imports"
               , ciArguments = []
               }
           , saEdits = []
           }
         else Nothing
       ]

-- | Generate "fix all" actions for a category
generateFixAllActions :: DiagnosticCategory -> [EnhancedDiagnostic] -> [SourceAction]
generateFixAllActions cat diags =
  let matching = filter ((== cat) . edCategory) diags
      fixable = filter (not . null . diagFixes . edOriginal) matching
  in if null fixable
     then []
     else [SourceAction
       { saTitle = "Fix all " <> categoryName cat <> " issues (" <> T.pack (show (length fixable)) <> ")"
       , saKind = "source.fixAll"
       , saCommand = Just CommandInfo
           { ciCommand = "argus.fixAllCategory"
           , ciTitle = "Fix all " <> categoryName cat
           , ciArguments = [Aeson.String $ T.pack $ show cat]
           }
       , saEdits = []
       }]

-- | Get human-readable category name
categoryName :: DiagnosticCategory -> Text
categoryName = \case
  CatCodeQuality  -> "code quality"
  CatPerformance  -> "performance"
  CatSecurity     -> "security"
  CatStyle        -> "style"
  CatUnused       -> "unused code"
  CatPartial      -> "partial function"
  CatNaming       -> "naming"
  CatImports      -> "import"
  CatTypes        -> "type"
  CatComplexity   -> "complexity"
  CatRefactoring  -> "refactoring"

-- | Generate category-specific actions
generateCategoryActions :: [EnhancedDiagnostic] -> [SourceAction]
generateCategoryActions diags =
  let categories = Set.toList $ Set.fromList $ map edCategory diags
  in concatMap (\cat -> generateFixAllActions cat diags) categories

--------------------------------------------------------------------------------
-- Diagnostic Aggregation
--------------------------------------------------------------------------------

-- | Summary of diagnostics in a file or project
data DiagnosticSummary = DiagnosticSummary
  { dsTotal       :: Int
  , dsErrors      :: Int
  , dsWarnings    :: Int
  , dsSuggestions :: Int
  , dsByCategory  :: Map DiagnosticCategory Int
  , dsByRule      :: Map Text Int
  , dsFixable     :: Int
  , dsAutoFixable :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

-- | Aggregate diagnostics into a summary
aggregateDiagnostics :: [EnhancedDiagnostic] -> DiagnosticSummary
aggregateDiagnostics diags = DiagnosticSummary
  { dsTotal       = length diags
  , dsErrors      = countBySeverity Error
  , dsWarnings    = countBySeverity Warning
  , dsSuggestions = countBySeverity Suggestion
  , dsByCategory  = Map.fromListWith (+) [(edCategory d, 1) | d <- diags]
  , dsByRule      = Map.fromListWith (+)
      [(fromMaybe "unknown" (diagCode $ edOriginal d), 1) | d <- diags]
  , dsFixable     = length [d | d <- diags, not $ null $ diagFixes $ edOriginal d]
  , dsAutoFixable = length [d | d <- diags
                              , any fixIsPreferred $ diagFixes $ edOriginal d]
  }
  where
    countBySeverity sev = length [d | d <- diags, diagSeverity (edOriginal d) == sev]

-- | Group diagnostics by category
groupByCategory :: [EnhancedDiagnostic] -> Map DiagnosticCategory [EnhancedDiagnostic]
groupByCategory diags =
  Map.fromListWith (++) [(edCategory d, [d]) | d <- diags]

-- | Group diagnostics by rule
groupByRule :: [EnhancedDiagnostic] -> Map Text [EnhancedDiagnostic]
groupByRule diags =
  Map.fromListWith (++)
    [(fromMaybe "unknown" (diagCode $ edOriginal d), [d]) | d <- diags]

--------------------------------------------------------------------------------
-- Diagnostic Filtering
--------------------------------------------------------------------------------

-- | Diagnostic filter configuration
data DiagnosticFilter = DiagnosticFilter
  { dfMinSeverity   :: Maybe Severity
  , dfCategories    :: Maybe (Set DiagnosticCategory)
  , dfExcludeRules  :: Set Text
  , dfIncludeRules  :: Maybe (Set Text)
  , dfRange         :: Maybe (SrcSpan, SrcSpan)
  , dfOnlyFixable   :: Bool
  , dfOnlyAutofix   :: Bool
  }
  deriving stock (Eq, Show, Generic)

-- | Apply a filter to diagnostics
applyFilter :: DiagnosticFilter -> [EnhancedDiagnostic] -> [EnhancedDiagnostic]
applyFilter DiagnosticFilter{..} = filter matchesFilter
  where
    matchesFilter ed =
      matchesSeverity ed &&
      matchesCategory ed &&
      matchesRule ed &&
      matchesRange ed &&
      matchesFixable ed &&
      matchesAutofix ed

    matchesSeverity ed = case dfMinSeverity of
      Nothing -> True
      Just minSev -> diagSeverity (edOriginal ed) >= minSev

    matchesCategory ed = case dfCategories of
      Nothing -> True
      Just cats -> edCategory ed `Set.member` cats

    matchesRule ed =
      let rule = fromMaybe "" $ diagCode $ edOriginal ed
      in not (rule `Set.member` dfExcludeRules) &&
         case dfIncludeRules of
           Nothing -> True
           Just include -> rule `Set.member` include

    matchesRange ed = case dfRange of
      Nothing -> True
      Just (start, end) ->
        let span' = diagSpan (edOriginal ed)
        in spansOverlap span' start || spansOverlap span' end

    matchesFixable ed =
      not dfOnlyFixable || (not $ null $ diagFixes $ edOriginal ed)

    matchesAutofix ed =
      not dfOnlyAutofix || any fixIsPreferred (diagFixes $ edOriginal ed)

-- | Filter diagnostics by minimum severity
filterBySeverity :: Severity -> [EnhancedDiagnostic] -> [EnhancedDiagnostic]
filterBySeverity minSev = filter $ \ed ->
  diagSeverity (edOriginal ed) >= minSev

-- | Filter diagnostics by category
filterByCategory :: Set DiagnosticCategory -> [EnhancedDiagnostic] -> [EnhancedDiagnostic]
filterByCategory cats = filter $ \ed ->
  edCategory ed `Set.member` cats

-- | Filter diagnostics by range
filterByRange :: SrcSpan -> SrcSpan -> [EnhancedDiagnostic] -> [EnhancedDiagnostic]
filterByRange start end = filter $ \ed ->
  let span' = diagSpan (edOriginal ed)
  in spansOverlap span' start || spansOverlap span' end

--------------------------------------------------------------------------------
-- Quick Fix Preview
--------------------------------------------------------------------------------

-- | Preview of applying a fix
data FixPreview = FixPreview
  { fpOriginalLines :: [Text]    -- ^ Lines affected before fix
  , fpNewLines      :: [Text]    -- ^ Lines after fix
  , fpStartLine     :: Int       -- ^ First affected line (1-indexed)
  , fpEndLine       :: Int       -- ^ Last affected line (1-indexed)
  , fpDiff          :: Text      -- ^ Unified diff format
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

-- | Generate a preview of applying a fix
generatePreview :: Text -> Fix -> FixPreview
generatePreview content fix =
  let newContent = applyFix content fix
      contentLines = T.lines content
      newLines = T.lines newContent
      -- Find the affected line range
      (startLine, endLine) = findAffectedRange (fixEdits fix)
      origAffected = take (endLine - startLine + 3) $ drop (max 0 (startLine - 2)) contentLines
      newAffected = take (endLine - startLine + 3) $ drop (max 0 (startLine - 2)) newLines
  in FixPreview
       { fpOriginalLines = origAffected
       , fpNewLines      = newAffected
       , fpStartLine     = max 1 (startLine - 1)
       , fpEndLine       = endLine + 1
       , fpDiff          = generateUnifiedDiff startLine origAffected newAffected
       }

-- | Find the line range affected by edits
findAffectedRange :: [FixEdit] -> (Int, Int)
findAffectedRange [] = (1, 1)
findAffectedRange edits =
  let starts = map (unLine . srcSpanStartLine . fixEditSpan) edits
      ends = map (unLine . srcSpanEndLine . fixEditSpan) edits
  in (minimum starts, maximum ends)

-- | Generate unified diff format
generateUnifiedDiff :: Int -> [Text] -> [Text] -> Text
generateUnifiedDiff startLine origLines newLines =
  let header = "@@ -" <> T.pack (show startLine) <> "," <>
               T.pack (show (length origLines)) <> " +" <>
               T.pack (show startLine) <> "," <>
               T.pack (show (length newLines)) <> " @@"
      origWithMinus = map ("-" <>) origLines
      newWithPlus = map ("+" <>) newLines
  in T.unlines $ header : origWithMinus ++ newWithPlus

-- | Generate preview for batch fixes
generateBatchPreview :: Text -> [Fix] -> [FixPreview]
generateBatchPreview content fixes =
  -- Filter out fixes with no edits, then sort by position
  let validFixes = filter (not . null . fixEdits) fixes
      getFirstSpan f = case fixEdits f of
        (e:_) -> srcSpanStartLine (fixEditSpan e)
        [] -> Line 0
      sorted = sortBy (comparing getFirstSpan) validFixes
  in map (generatePreview content) sorted

--------------------------------------------------------------------------------
-- LSP Conversion
--------------------------------------------------------------------------------

-- | Convert enhanced diagnostic to LSP format with structured data
toEnhancedLspDiagnostic :: EnhancedDiagnostic -> Aeson.Value
toEnhancedLspDiagnostic ed =
  let diag = edOriginal ed
      lspTags = map tagToInt (edTags ed)
      -- Enrich message with short rule summary if available
      enrichedMessage = case edRuleInfo ed of
        Nothing -> diagMessage diag
        Just ruleInfo -> diagMessage diag <> "\n\n" <> formatShortSummary ruleInfo
  in object $ catMaybes
       [ Just $ "range" .= spanToLspRange (diagSpan diag)
       , Just $ "severity" .= severityToLspInt (diagSeverity diag)
       , ("code" .=) <$> diagCode diag
       , Just $ "source" .= ("argus" :: Text)
       , Just $ "message" .= enrichedMessage
       , if null lspTags then Nothing else Just $ "tags" .= lspTags
       , ("data" .=) <$> edData ed
       , if null (edRelatedInfo ed)
         then Nothing
         else Just $ "relatedInformation" .= map relatedInfoToLsp (edRelatedInfo ed)
       ]

-- | Convert diagnostic tag to LSP integer
tagToInt :: DiagnosticTag -> Int
tagToInt TagUnnecessary = 1
tagToInt TagDeprecated  = 2

-- | Convert severity to LSP integer
severityToLspInt :: Severity -> Int
severityToLspInt Error      = 1
severityToLspInt Warning    = 2
severityToLspInt Info       = 3
severityToLspInt Suggestion = 4

-- | Convert SrcSpan to LSP range format
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

-- | Convert related info to LSP format
relatedInfoToLsp :: RelatedInfo -> Aeson.Value
relatedInfoToLsp RelatedInfo{..} = object
  [ "location" .= object
      [ "uri" .= ("file://" <> T.pack (srcSpanFile riLocation))
      , "range" .= spanToLspRange riLocation
      ]
  , "message" .= riMessage
  ]

-- | Convert enhanced diagnostic back to basic LSP diagnostic (for compatibility)
fromEnhancedToLsp :: EnhancedDiagnostic -> Aeson.Value
fromEnhancedToLsp = toEnhancedLspDiagnostic
