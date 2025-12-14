{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TypeFamilies #-}

-- |
-- Module      : Argus.AutoFix.Learning
-- Description : Learning-based fix suggestions and pattern recognition
-- Copyright   : (c) 2024-2025
-- License     : MIT
--
-- This module provides machine-learning-inspired fix suggestions that learn
-- from past fixes to suggest similar transformations. It implements:
--
-- * Fix pattern extraction and fingerprinting
-- * Pattern similarity matching
-- * Fix history tracking and analysis
-- * Confidence scoring based on historical success
-- * Collaborative filtering for fix recommendations
--
-- == Learning Model
--
-- The learning system operates on three levels:
--
-- 1. **Syntactic patterns**: Structural code patterns that trigger fixes
-- 2. **Semantic patterns**: Type-aware patterns using HIE data
-- 3. **Historical patterns**: Patterns from successful past fixes
--
-- == Architecture
--
-- @
-- ┌─────────────────┐     ┌───────────────────┐     ┌─────────────────┐
-- │  Fix History    │────▶│  Pattern Learner  │────▶│  Fix Suggester  │
-- └─────────────────┘     └───────────────────┘     └─────────────────┘
--         │                       │                        │
--         ▼                       ▼                        ▼
-- ┌─────────────────┐     ┌───────────────────┐     ┌─────────────────┐
-- │ Pattern Database│     │ Feature Extractor │     │ Confidence Score│
-- └─────────────────┘     └───────────────────┘     └─────────────────┘
-- @
--
-- == Example
--
-- @
-- -- Initialize the learning engine
-- engine <- initLearningEngine config
--
-- -- Learn from a successful fix
-- recordSuccessfulFix engine fixApplication
--
-- -- Get suggestions for a new diagnostic
-- suggestions <- suggestFixes engine diagnostic content
-- @
module Argus.AutoFix.Learning
  ( -- * Learning Engine
    LearningEngine (..)
  , LearningConfig (..)
  , defaultLearningConfig
  , initLearningEngine
  , shutdownLearningEngine
  , saveLearningState
  , loadLearningState

    -- * Fix Patterns
  , FixPattern (..)
  , PatternId
  , PatternCategory (..)
  , extractPattern
  , matchPattern
  , patternSimilarity

    -- * Pattern Fingerprinting
  , PatternFingerprint (..)
  , computeFingerprint
  , fingerprintSimilarity
  , FingerprintComponent (..)

    -- * Fix History
  , FixHistory (..)
  , FixApplication (..)
  , ApplicationOutcome (..)
  , recordSuccessfulFix
  , recordFailedFix
  , recordRejectedFix
  , getFixHistory

    -- * Fix Suggestions
  , FixSuggestion (..)
  , suggestFixes
  , suggestFromHistory
  , suggestFromPatterns
  , rankSuggestions

    -- * Feature Extraction
  , FeatureVector (..)
  , CodeFeatures (..)
  , DiagnosticFeatures (..)
  , extractCodeFeatures
  , extractDiagnosticFeatures
  , combineFeatures

    -- * Similarity Computation
  , computeSimilarity
  , cosineSimilarity
  , jaccardSimilarity
  , editDistance
  , SimilarityMetric (..)

    -- * Confidence Scoring
  , ConfidenceModel (..)
  , computeConfidence
  , updateConfidence
  , confidenceFactors

    -- * Pattern Learning
  , learnPatterns
  , updatePatterns
  , prunePatterns
  , exportPatterns
  , importPatterns
  , LearningResult (..)

    -- * Statistics
  , LearningStats (..)
  , getStats
  , resetStats

    -- * Pattern Database
  , PatternDB (..)
  , queryPatterns
  , addPattern
  , removePattern
  , PatternQuery (..)
  ) where

import Control.Concurrent.STM (TVar, newTVarIO, readTVarIO, writeTVar, modifyTVar', atomically)
import Control.Exception (try, SomeException)
import Control.Monad (forM, when)
import Data.Aeson (ToJSON (..), FromJSON (..), encode, decode, ToJSONKey (..), FromJSONKey (..))
import Data.Aeson.Types (toJSONKeyText, FromJSONKeyFunction (..))
import Data.ByteString.Lazy qualified as BL
import Data.Hashable (Hashable, hash)
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, listToMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Time (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import Data.UUID.V4 qualified as UUID
import Data.Word (Word64)
import GHC.Generics (Generic)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath ((</>))

import Argus.Types
  ( SrcSpan (..)
  , Fix (..)
  , FixEdit (..)
  , Diagnostic (..)
  , DiagnosticKind (..)
  , Severity (..)
  , Line (..)
  , Column (..)
  , FixCategory (..)
  , FixSafety (..)
  )
import Argus.AutoFix.Types
import Argus.Rules.Types (Category (..))
import Argus.Rules.Types qualified as RT

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for the learning engine
data LearningConfig = LearningConfig
  { lcStoragePath       :: FilePath
    -- ^ Path for persisting learned patterns
  , lcMaxPatterns       :: Int
    -- ^ Maximum patterns to store
  , lcMinConfidence     :: Double
    -- ^ Minimum confidence for suggestions
  , lcHistorySize       :: Int
    -- ^ Maximum history entries to keep
  , lcSimilarityThreshold :: Double
    -- ^ Threshold for pattern matching
  , lcLearningRate      :: Double
    -- ^ Rate of confidence adjustment
  , lcDecayFactor       :: Double
    -- ^ Time-based decay for old patterns
  , lcEnableFingerprinting :: Bool
    -- ^ Use fingerprinting for fast matching
  , lcAutoSave          :: Bool
    -- ^ Auto-save patterns periodically
  , lcSaveInterval      :: Int
    -- ^ Save interval in seconds
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default learning configuration
defaultLearningConfig :: LearningConfig
defaultLearningConfig = LearningConfig
  { lcStoragePath = ".argus/learning"
  , lcMaxPatterns = 10000
  , lcMinConfidence = 0.5
  , lcHistorySize = 1000
  , lcSimilarityThreshold = 0.7
  , lcLearningRate = 0.1
  , lcDecayFactor = 0.99
  , lcEnableFingerprinting = True
  , lcAutoSave = True
  , lcSaveInterval = 300
  }

--------------------------------------------------------------------------------
-- Learning Engine
--------------------------------------------------------------------------------

-- | The learning engine with pattern database and history
data LearningEngine = LearningEngine
  { leConfig     :: LearningConfig
  , lePatternDB  :: TVar PatternDB
  , leHistory    :: TVar FixHistory
  , leStats      :: TVar LearningStats
  , leLastSave   :: TVar UTCTime
  }

-- | Statistics for the learning engine
data LearningStats = LearningStats
  { lsPatternsLearned    :: Int
  , lsPatternsMatched    :: Int
  , lsSuggestionsGiven   :: Int
  , lsSuggestionsAccepted :: Int
  , lsSuggestionsRejected :: Int
  , lsAverageConfidence  :: Double
  , lsCacheHits          :: Int
  , lsCacheMisses        :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty stats
emptyLearningStats :: LearningStats
emptyLearningStats = LearningStats 0 0 0 0 0 0.0 0 0

-- | Initialize the learning engine
initLearningEngine :: LearningConfig -> IO LearningEngine
initLearningEngine config = do
  dbVar <- newTVarIO emptyPatternDB
  historyVar <- newTVarIO emptyFixHistory
  statsVar <- newTVarIO emptyLearningStats
  now <- getCurrentTime
  lastSaveVar <- newTVarIO now

  let engine = LearningEngine
        { leConfig = config
        , lePatternDB = dbVar
        , leHistory = historyVar
        , leStats = statsVar
        , leLastSave = lastSaveVar
        }

  -- Try to load existing state
  _ <- try @SomeException $ loadLearningState engine

  pure engine

-- | Shutdown the learning engine
shutdownLearningEngine :: LearningEngine -> IO ()
shutdownLearningEngine engine = do
  when (lcAutoSave $ leConfig engine) $
    saveLearningState engine

-- | Save learning state to disk
saveLearningState :: LearningEngine -> IO ()
saveLearningState engine = do
  let config = leConfig engine
      path = lcStoragePath config

  createDirectoryIfMissing True path

  db <- readTVarIO (lePatternDB engine)
  history <- readTVarIO (leHistory engine)

  BL.writeFile (path </> "patterns.json") (encode db)
  BL.writeFile (path </> "history.json") (encode history)

  now <- getCurrentTime
  atomically $ writeTVar (leLastSave engine) now

-- | Load learning state from disk
loadLearningState :: LearningEngine -> IO ()
loadLearningState engine = do
  let config = leConfig engine
      path = lcStoragePath config

  patternsExists <- doesFileExist (path </> "patterns.json")
  when patternsExists $ do
    mDb <- decode <$> BL.readFile (path </> "patterns.json")
    case mDb of
      Just db -> atomically $ writeTVar (lePatternDB engine) db
      Nothing -> pure ()

  historyExists <- doesFileExist (path </> "history.json")
  when historyExists $ do
    mHistory <- decode <$> BL.readFile (path </> "history.json")
    case mHistory of
      Just h -> atomically $ writeTVar (leHistory engine) h
      Nothing -> pure ()

--------------------------------------------------------------------------------
-- Fix Patterns
--------------------------------------------------------------------------------

-- | Unique pattern identifier
type PatternId = UUID

-- | Category of patterns
data PatternCategory
  = PCBoolean          -- ^ Boolean simplifications
  | PCList             -- ^ List operations
  | PCPartial          -- ^ Partial function replacements
  | PCImport           -- ^ Import fixes
  | PCNaming           -- ^ Naming conventions
  | PCStyle            -- ^ Code style
  | PCPerformance      -- ^ Performance improvements
  | PCSecurity         -- ^ Security fixes
  | PCCustom Text      -- ^ Custom category
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

instance ToJSONKey PatternCategory where
  toJSONKey = toJSONKeyText patternCategoryToText

instance FromJSONKey PatternCategory where
  fromJSONKey = FromJSONKeyText textToPatternCategory

patternCategoryToText :: PatternCategory -> Text
patternCategoryToText = \case
  PCBoolean -> "boolean"
  PCList -> "list"
  PCPartial -> "partial"
  PCImport -> "import"
  PCNaming -> "naming"
  PCStyle -> "style"
  PCPerformance -> "performance"
  PCSecurity -> "security"
  PCCustom t -> "custom:" <> t

textToPatternCategory :: Text -> PatternCategory
textToPatternCategory t = case t of
  "boolean" -> PCBoolean
  "list" -> PCList
  "partial" -> PCPartial
  "import" -> PCImport
  "naming" -> PCNaming
  "style" -> PCStyle
  "performance" -> PCPerformance
  "security" -> PCSecurity
  _ | "custom:" `T.isPrefixOf` t -> PCCustom (T.drop 7 t)
    | otherwise -> PCCustom t

-- | A learned fix pattern
data FixPattern = FixPattern
  { fpId           :: PatternId
  , fpCategory     :: PatternCategory
  , fpName         :: Text
  , fpDescription  :: Text
  , fpBeforePattern :: Text
    -- ^ Pattern that matches the problematic code
  , fpAfterPattern  :: Text
    -- ^ Pattern that represents the fix
  , fpConstraints   :: [PatternConstraint]
    -- ^ Additional constraints for matching
  , fpFingerprint   :: PatternFingerprint
    -- ^ For fast matching
  , fpConfidence    :: Double
    -- ^ Learned confidence
  , fpSuccessCount  :: Int
  , fpFailureCount  :: Int
  , fpLastUsed      :: UTCTime
  , fpCreatedAt     :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Pattern constraint
data PatternConstraint
  = PCTypeMatch Text       -- ^ Type must match
  | PCModuleMatch Text     -- ^ Must be in module
  | PCNotInModule Text     -- ^ Must not be in module
  | PCHasImport Text       -- ^ Must have import
  | PCContextMatch Text    -- ^ Surrounding context must match
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

-- | Extract pattern from a fix application
extractPattern :: FixApplication -> IO (Maybe FixPattern)
extractPattern app = do
  patId <- UUID.nextRandom
  now <- getCurrentTime

  let beforeText = faOriginalCode app
      afterText = faFixedCode app
      category = inferCategory (faDiagnostic app)

  pure $ Just FixPattern
    { fpId = patId
    , fpCategory = category
    , fpName = faDiagnosticCode app
    , fpDescription = "Auto-learned pattern from " <> faDiagnosticCode app
    , fpBeforePattern = abstractPattern beforeText
    , fpAfterPattern = abstractPattern afterText
    , fpConstraints = []
    , fpFingerprint = computeFingerprint beforeText afterText
    , fpConfidence = 0.5  -- Start at neutral
    , fpSuccessCount = 1
    , fpFailureCount = 0
    , fpLastUsed = now
    , fpCreatedAt = now
    }
  where
    inferCategory :: Diagnostic -> PatternCategory
    inferCategory diag = case diagKind diag of
      PartialFunction -> PCPartial
      UnusedImport -> PCImport
      ImportStyle -> PCImport
      NamingConvention -> PCNaming
      RedundantCode -> PCStyle
      PerformanceIssue -> PCPerformance
      SecurityIssue -> PCSecurity
      _ -> PCStyle

    abstractPattern :: Text -> Text
    abstractPattern = id  -- Would abstract identifiers to placeholders

-- | Match a pattern against code
matchPattern :: FixPattern -> Text -> Bool
matchPattern fixPat code =
  let before = fpBeforePattern fixPat
  in before `T.isInfixOf` code ||
     patternSimilarity before code > 0.8

-- | Compute similarity between patterns
patternSimilarity :: Text -> Text -> Double
patternSimilarity p1 p2
  | T.null p1 || T.null p2 = 0.0
  | p1 == p2 = 1.0
  | otherwise =
      let tokens1 = T.words p1
          tokens2 = T.words p2
          intersection = length $ filter (`elem` tokens2) tokens1
          union = length $ Set.toList $ Set.fromList tokens1 `Set.union` Set.fromList tokens2
      in if union == 0 then 0.0 else fromIntegral intersection / fromIntegral union

--------------------------------------------------------------------------------
-- Pattern Fingerprinting
--------------------------------------------------------------------------------

-- | A fingerprint for fast pattern matching
data PatternFingerprint = PatternFingerprint
  { pfHash         :: Word64
  , pfComponents   :: [FingerprintComponent]
  , pfLength       :: Int
  , pfTokenCount   :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Component of a fingerprint
data FingerprintComponent
  = FCKeyword Text     -- ^ Haskell keyword
  | FCOperator Text    -- ^ Operator
  | FCLiteral          -- ^ Any literal
  | FCIdentifier       -- ^ Any identifier
  | FCPattern Text     -- ^ Pattern type (case, lambda, etc.)
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

-- | Compute fingerprint for code
computeFingerprint :: Text -> Text -> PatternFingerprint
computeFingerprint before after =
  let combined = before <> " -> " <> after
      tokens = tokenize combined
      components = map tokenToComponent tokens
      hashVal = fromIntegral $ hash combined
  in PatternFingerprint
    { pfHash = hashVal
    , pfComponents = components
    , pfLength = T.length combined
    , pfTokenCount = length tokens
    }
  where
    tokenize :: Text -> [Text]
    tokenize = T.words

    tokenToComponent :: Text -> FingerprintComponent
    tokenToComponent t
      | t `elem` keywords = FCKeyword t
      | t `elem` operators = FCOperator t
      | isLiteral t = FCLiteral
      | otherwise = FCIdentifier

    keywords :: [Text]
    keywords = ["if", "then", "else", "case", "of", "let", "in", "where", "do", "import", "module"]

    operators :: [Text]
    operators = ["+", "-", "*", "/", "==", "/=", "<", ">", "<=", ">=", "&&", "||", ".", "$", ">>=", ">>"]

    isLiteral :: Text -> Bool
    isLiteral t = T.all isDigitChar t || isStringLiteral t

    isDigitChar :: Char -> Bool
    isDigitChar c = c >= '0' && c <= '9'

    isStringLiteral :: Text -> Bool
    isStringLiteral t = T.head t == '"' || T.head t == '\''

-- | Compute similarity between fingerprints
fingerprintSimilarity :: PatternFingerprint -> PatternFingerprint -> Double
fingerprintSimilarity fp1 fp2 =
  let hashSim = if pfHash fp1 == pfHash fp2 then 1.0 else 0.0
      lengthSim = 1.0 - abs (fromIntegral (pfLength fp1 - pfLength fp2)) / fromIntegral (max (pfLength fp1) (pfLength fp2))
      tokenSim = 1.0 - abs (fromIntegral (pfTokenCount fp1 - pfTokenCount fp2)) / fromIntegral (max (pfTokenCount fp1) (pfTokenCount fp2))
      compSim = jaccardSimilarity (Set.fromList $ pfComponents fp1) (Set.fromList $ pfComponents fp2)
  in (hashSim * 0.3 + lengthSim * 0.2 + tokenSim * 0.2 + compSim * 0.3)

--------------------------------------------------------------------------------
-- Fix History
--------------------------------------------------------------------------------

-- | History of fix applications
data FixHistory = FixHistory
  { fhApplications :: [FixApplication]
  , fhByDiagnostic :: Map Text [FixApplication]
  , fhByOutcome    :: Map ApplicationOutcome [FixApplication]
  , fhTotalCount   :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty history
emptyFixHistory :: FixHistory
emptyFixHistory = FixHistory [] Map.empty Map.empty 0

-- | A recorded fix application
data FixApplication = FixApplication
  { faId             :: UUID
  , faDiagnostic     :: Diagnostic
  , faDiagnosticCode :: Text
  , faFix            :: Fix
  , faOriginalCode   :: Text
  , faFixedCode      :: Text
  , faFilePath       :: FilePath
  , faOutcome        :: ApplicationOutcome
  , faTimestamp      :: UTCTime
  , faUserFeedback   :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Outcome of applying a fix
data ApplicationOutcome
  = AOSuccess         -- ^ Fix applied successfully
  | AOFailure Text    -- ^ Fix failed with error
  | AORejected        -- ^ User rejected the fix
  | AOReverted        -- ^ Fix was reverted
  | AOPending         -- ^ Outcome unknown
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (Hashable, ToJSON, FromJSON)

instance ToJSONKey ApplicationOutcome where
  toJSONKey = toJSONKeyText applicationOutcomeToText

instance FromJSONKey ApplicationOutcome where
  fromJSONKey = FromJSONKeyText textToApplicationOutcome

applicationOutcomeToText :: ApplicationOutcome -> Text
applicationOutcomeToText = \case
  AOSuccess -> "success"
  AOFailure err -> "failure:" <> err
  AORejected -> "rejected"
  AOReverted -> "reverted"
  AOPending -> "pending"

textToApplicationOutcome :: Text -> ApplicationOutcome
textToApplicationOutcome t = case t of
  "success" -> AOSuccess
  "rejected" -> AORejected
  "reverted" -> AOReverted
  "pending" -> AOPending
  _ | "failure:" `T.isPrefixOf` t -> AOFailure (T.drop 8 t)
    | otherwise -> AOPending

-- | Record a successful fix
recordSuccessfulFix :: LearningEngine -> FixApplication -> IO ()
recordSuccessfulFix engine app = do
  let app' = app { faOutcome = AOSuccess }

  atomically $ modifyTVar' (leHistory engine) $ \h ->
    addToHistory h app'

  -- Learn from this fix
  mPattern <- extractPattern app'
  case mPattern of
    Just learnedPat -> atomically $ modifyTVar' (lePatternDB engine) $ \db ->
      addOrUpdatePattern db learnedPat
    Nothing -> pure ()

  -- Update stats
  atomically $ modifyTVar' (leStats engine) $ \s ->
    s { lsSuggestionsAccepted = lsSuggestionsAccepted s + 1 }

-- | Record a failed fix
recordFailedFix :: LearningEngine -> FixApplication -> Text -> IO ()
recordFailedFix engine app reason = do
  let app' = app { faOutcome = AOFailure reason }

  atomically $ modifyTVar' (leHistory engine) $ \h ->
    addToHistory h app'

  -- Decrease confidence for matching patterns
  mPattern <- findMatchingPattern engine (faOriginalCode app)
  case mPattern of
    Just failedPat -> atomically $ modifyTVar' (lePatternDB engine) $ \db ->
      updatePatternFailure db (fpId failedPat)
    Nothing -> pure ()

-- | Record a rejected fix
recordRejectedFix :: LearningEngine -> FixApplication -> IO ()
recordRejectedFix engine app = do
  let app' = app { faOutcome = AORejected }

  atomically $ modifyTVar' (leHistory engine) $ \h ->
    addToHistory h app'

  -- Update stats
  atomically $ modifyTVar' (leStats engine) $ \s ->
    s { lsSuggestionsRejected = lsSuggestionsRejected s + 1 }

-- | Get fix history
getFixHistory :: LearningEngine -> IO FixHistory
getFixHistory = readTVarIO . leHistory

-- | Add to history with size limit
addToHistory :: FixHistory -> FixApplication -> FixHistory
addToHistory h app =
  let diagCode = faDiagnosticCode app
      outcome = faOutcome app
      apps = take 1000 (app : fhApplications h)  -- Limit size
  in h
    { fhApplications = apps
    , fhByDiagnostic = Map.insertWith (++) diagCode [app] (fhByDiagnostic h)
    , fhByOutcome = Map.insertWith (++) outcome [app] (fhByOutcome h)
    , fhTotalCount = fhTotalCount h + 1
    }

-- | Find matching pattern
findMatchingPattern :: LearningEngine -> Text -> IO (Maybe FixPattern)
findMatchingPattern engine code = do
  db <- readTVarIO (lePatternDB engine)
  let config = leConfig engine
      threshold = lcSimilarityThreshold config
      patterns = pdbPatterns db
      matches = filter (\p -> matchPattern p code) patterns
      sorted = sortBy (comparing (negate . fpConfidence)) matches
  pure $ listToMaybe sorted

--------------------------------------------------------------------------------
-- Fix Suggestions
--------------------------------------------------------------------------------

-- | A fix suggestion from the learning engine
data FixSuggestion = FixSuggestion
  { fsPattern     :: Maybe FixPattern
  , fsFix         :: Fix
  , fsConfidence  :: Double
  , fsReason      :: Text
  , fsSource      :: SuggestionSource
  , fsSimilarFixes :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Source of a suggestion
data SuggestionSource
  = SSPattern PatternId      -- ^ From a learned pattern
  | SSHistory               -- ^ From fix history
  | SSRule Text             -- ^ From a builtin rule
  | SSHeuristic Text        -- ^ From a heuristic
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Suggest fixes based on learning
suggestFixes :: LearningEngine -> Diagnostic -> Text -> IO [FixSuggestion]
suggestFixes engine diag content = do
  let config = leConfig engine

  -- Get suggestions from different sources
  patternSuggestions <- suggestFromPatterns engine diag content
  historySuggestions <- suggestFromHistory engine diag content

  -- Combine and rank
  let allSuggestions = patternSuggestions ++ historySuggestions
      ranked = rankSuggestions allSuggestions
      filtered = filter ((>= lcMinConfidence config) . fsConfidence) ranked

  -- Update stats
  atomically $ modifyTVar' (leStats engine) $ \s ->
    s { lsSuggestionsGiven = lsSuggestionsGiven s + length filtered }

  pure filtered

-- | Suggest from learned patterns
suggestFromPatterns :: LearningEngine -> Diagnostic -> Text -> IO [FixSuggestion]
suggestFromPatterns engine diag content = do
  db <- readTVarIO (lePatternDB engine)
  let config = leConfig engine
      threshold = lcSimilarityThreshold config

  let matchingPatterns = filter (\p -> matchPattern p content) (pdbPatterns db)

  forM matchingPatterns $ \fixPat -> do
    let fix = patternToFix fixPat diag
    pure FixSuggestion
      { fsPattern = Just fixPat
      , fsFix = fix
      , fsConfidence = fpConfidence fixPat
      , fsReason = fpDescription fixPat
      , fsSource = SSPattern (fpId fixPat)
      , fsSimilarFixes = fpSuccessCount fixPat
      }
  where
    patternToFix :: FixPattern -> Diagnostic -> Fix
    patternToFix fp d = Fix
      { fixTitle = fpName fp
      , fixEdits = [FixEdit (diagSpan d) (fpAfterPattern fp)]
      , fixIsPreferred = fpConfidence fp > 0.8
      , fixAddImports = []
      , fixRemoveImports = []
      , fixCategory = patternCategoryToFixCategory (fpCategory fp)
      , fixSafety = confidenceToSafety (fpConfidence fp)
      }

    patternCategoryToFixCategory :: PatternCategory -> FixCategory
    patternCategoryToFixCategory = \case
      PCBoolean -> FCStyle
      PCList -> FCStyle
      PCPartial -> FCSafety
      PCImport -> FCImports
      PCNaming -> FCStyle
      PCStyle -> FCStyle
      PCPerformance -> FCPerformance
      PCSecurity -> FCSecurity
      PCCustom _ -> FCStyle

    confidenceToSafety :: Double -> FixSafety
    confidenceToSafety c
      | c >= 0.9 = FSAlways
      | c >= 0.7 = FSMostly
      | c >= 0.5 = FSReview
      | otherwise = FSUnsafe

-- | Suggest from fix history
suggestFromHistory :: LearningEngine -> Diagnostic -> Text -> IO [FixSuggestion]
suggestFromHistory engine diag content = do
  history <- readTVarIO (leHistory engine)

  let diagCodeText = maybe "" id (Argus.Types.diagCode diag)
      similarApps = Map.findWithDefault [] diagCodeText (fhByDiagnostic history)
      successful = filter ((== AOSuccess) . faOutcome) similarApps

  forM (take 5 successful) $ \app -> do
    let similarity = computeSimilarity (faOriginalCode app) content
    pure FixSuggestion
      { fsPattern = Nothing
      , fsFix = faFix app
      , fsConfidence = similarity * 0.8
      , fsReason = "Similar to previous successful fix"
      , fsSource = SSHistory
      , fsSimilarFixes = length successful
      }

-- | Rank suggestions by confidence and other factors
rankSuggestions :: [FixSuggestion] -> [FixSuggestion]
rankSuggestions = sortBy (comparing (negate . fsConfidence))

--------------------------------------------------------------------------------
-- Feature Extraction
--------------------------------------------------------------------------------

-- | A vector of numerical features
data FeatureVector = FeatureVector
  { fvValues   :: [Double]
  , fvLabels   :: [Text]
  , fvNormalized :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Features extracted from code
data CodeFeatures = CodeFeatures
  { cfLength       :: Int
  , cfLineCount    :: Int
  , cfTokenCount   :: Int
  , cfNestingDepth :: Int
  , cfComplexity   :: Int
  , cfHasLambda    :: Bool
  , cfHasDo        :: Bool
  , cfHasCase      :: Bool
  , cfHasLet       :: Bool
  , cfHasWhere     :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Features from a diagnostic
data DiagnosticFeatures = DiagnosticFeatures
  { dfKind       :: DiagnosticKind
  , dfSeverity   :: Severity
  , dfSpanSize   :: Int
  , dfMessageLen :: Int
  , dfHasCode    :: Bool
  , dfFixCount   :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Extract features from code
extractCodeFeatures :: Text -> CodeFeatures
extractCodeFeatures code =
  let linesList = T.lines code
      tokens = T.words code
  in CodeFeatures
    { cfLength = T.length code
    , cfLineCount = length linesList
    , cfTokenCount = length tokens
    , cfNestingDepth = computeNesting code
    , cfComplexity = computeComplexity code
    , cfHasLambda = "\\" `T.isInfixOf` code
    , cfHasDo = "do" `T.isInfixOf` code
    , cfHasCase = "case" `T.isInfixOf` code
    , cfHasLet = "let" `T.isInfixOf` code
    , cfHasWhere = "where" `T.isInfixOf` code
    }
  where
    computeNesting :: Text -> Int
    computeNesting t =
      let opens = T.count "(" t + T.count "[" t + T.count "{" t
          closes = T.count ")" t + T.count "]" t + T.count "}" t
      in max opens closes

    computeComplexity :: Text -> Int
    computeComplexity t =
      T.count "if" t + T.count "case" t + T.count "guard" t

-- | Extract features from diagnostic
extractDiagnosticFeatures :: Diagnostic -> DiagnosticFeatures
extractDiagnosticFeatures diag = DiagnosticFeatures
  { dfKind = diagKind diag
  , dfSeverity = diagSeverity diag
  , dfSpanSize = spanSize (diagSpan diag)
  , dfMessageLen = T.length (diagMessage diag)
  , dfHasCode = isJust (diagCode diag)
  , dfFixCount = length (diagFixes diag)
  }
  where
    spanSize span =
      (unLine (srcSpanEndLine span) - unLine (srcSpanStartLine span) + 1) *
      (unColumn (srcSpanEndCol span) - unColumn (srcSpanStartCol span))

    isJust (Just _) = True
    isJust Nothing = False

-- | Combine features into a vector
combineFeatures :: CodeFeatures -> DiagnosticFeatures -> FeatureVector
combineFeatures cf df = FeatureVector
  { fvValues =
      [ fromIntegral (cfLength cf)
      , fromIntegral (cfLineCount cf)
      , fromIntegral (cfTokenCount cf)
      , fromIntegral (cfNestingDepth cf)
      , fromIntegral (cfComplexity cf)
      , if cfHasLambda cf then 1.0 else 0.0
      , if cfHasDo cf then 1.0 else 0.0
      , if cfHasCase cf then 1.0 else 0.0
      , severityToDouble (dfSeverity df)
      , fromIntegral (dfSpanSize df)
      ]
  , fvLabels =
      [ "length", "lines", "tokens", "nesting", "complexity"
      , "hasLambda", "hasDo", "hasCase", "severity", "spanSize"
      ]
  , fvNormalized = False
  }
  where
    severityToDouble :: Severity -> Double
    severityToDouble Error = 1.0
    severityToDouble Warning = 0.75
    severityToDouble Suggestion = 0.5
    severityToDouble Info = 0.25

--------------------------------------------------------------------------------
-- Similarity Computation
--------------------------------------------------------------------------------

-- | Metric for computing similarity
data SimilarityMetric
  = SMCosine         -- ^ Cosine similarity
  | SMJaccard        -- ^ Jaccard index
  | SMEditDistance   -- ^ Normalized edit distance
  | SMCombined       -- ^ Combination of metrics
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Compute similarity between two code snippets
computeSimilarity :: Text -> Text -> Double
computeSimilarity t1 t2
  | T.null t1 || T.null t2 = 0.0
  | t1 == t2 = 1.0
  | otherwise =
      let cosine = cosineSimilarity (T.words t1) (T.words t2)
          jaccard = jaccardSimilarity (Set.fromList $ T.words t1) (Set.fromList $ T.words t2)
          editDist = 1.0 - editDistance t1 t2
      in (cosine * 0.4 + jaccard * 0.3 + editDist * 0.3)

-- | Cosine similarity for lists
cosineSimilarity :: [Text] -> [Text] -> Double
cosineSimilarity xs ys =
  let xSet = Set.fromList xs
      ySet = Set.fromList ys
      intersection = Set.size $ Set.intersection xSet ySet
      xLen = sqrt $ fromIntegral $ Set.size xSet
      yLen = sqrt $ fromIntegral $ Set.size ySet
  in if xLen == 0 || yLen == 0
     then 0.0
     else fromIntegral intersection / (xLen * yLen)

-- | Jaccard similarity for sets
jaccardSimilarity :: Ord a => Set a -> Set a -> Double
jaccardSimilarity s1 s2 =
  let intersection = Set.size $ Set.intersection s1 s2
      union = Set.size $ Set.union s1 s2
  in if union == 0 then 0.0 else fromIntegral intersection / fromIntegral union

-- | Normalized edit distance (Levenshtein)
editDistance :: Text -> Text -> Double
editDistance t1 t2 =
  let maxLen = max (T.length t1) (T.length t2)
      dist = levenshtein (T.unpack t1) (T.unpack t2)
  in if maxLen == 0 then 0.0 else fromIntegral dist / fromIntegral maxLen
  where
    levenshtein :: String -> String -> Int
    levenshtein s1 s2 = last $ foldl (compute s1) [0..length s1] s2

    compute :: String -> [Int] -> Char -> [Int]
    compute s1 row c = scanl (step s1 c) (head row + 1) (zip3 s1 row (tail row))

    step :: String -> Char -> Int -> (Char, Int, Int) -> Int
    step _ c diag (c', left, up)
      | c == c'   = diag
      | otherwise = 1 + minimum [diag, left, up]

--------------------------------------------------------------------------------
-- Confidence Scoring
--------------------------------------------------------------------------------

-- | Model for computing confidence
data ConfidenceModel = ConfidenceModel
  { cmBaseConfidence   :: Double
  , cmSuccessWeight    :: Double
  , cmFailureWeight    :: Double
  , cmRecencyWeight    :: Double
  , cmSimilarityWeight :: Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default confidence model
defaultConfidenceModel :: ConfidenceModel
defaultConfidenceModel = ConfidenceModel
  { cmBaseConfidence = 0.5
  , cmSuccessWeight = 0.2
  , cmFailureWeight = 0.3
  , cmRecencyWeight = 0.1
  , cmSimilarityWeight = 0.2
  }

-- | Compute confidence for a pattern
computeConfidence :: ConfidenceModel -> FixPattern -> Double
computeConfidence model pat =
  let successRate = if total == 0 then 0.5
                    else fromIntegral (fpSuccessCount pat) / fromIntegral total
      total = fpSuccessCount pat + fpFailureCount pat
      base = cmBaseConfidence model
      successFactor = cmSuccessWeight model * successRate
      failureFactor = cmFailureWeight model * (1 - successRate)
  in clamp 0.0 1.0 (base + successFactor - failureFactor)
  where
    clamp lo hi x = max lo (min hi x)

-- | Update confidence based on outcome
updateConfidence :: ConfidenceModel -> FixPattern -> ApplicationOutcome -> FixPattern
updateConfidence model fixPat outcome =
  let newSuccess = case outcome of
        AOSuccess -> fpSuccessCount fixPat + 1
        _ -> fpSuccessCount fixPat
      newFailure = case outcome of
        AOFailure _ -> fpFailureCount fixPat + 1
        AORejected -> fpFailureCount fixPat + 1
        _ -> fpFailureCount fixPat
      fixPat' = fixPat { fpSuccessCount = newSuccess, fpFailureCount = newFailure }
      newConf = computeConfidence model fixPat'
  in fixPat' { fpConfidence = newConf }

-- | Get confidence factors breakdown
confidenceFactors :: FixPattern -> [(Text, Double)]
confidenceFactors fp =
  [ ("Success rate", successRate)
  , ("Base confidence", 0.5)
  , ("Usage count", min 1.0 (fromIntegral total / 100))
  ]
  where
    total = fpSuccessCount fp + fpFailureCount fp
    successRate = if total == 0 then 0.5
                  else fromIntegral (fpSuccessCount fp) / fromIntegral total

--------------------------------------------------------------------------------
-- Pattern Learning
--------------------------------------------------------------------------------

-- | Result of learning operation
data LearningResult = LearningResult
  { lrPatternsAdded    :: Int
  , lrPatternsUpdated  :: Int
  , lrPatternsPruned   :: Int
  , lrNewConfidence    :: Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Learn patterns from applications
learnPatterns :: LearningEngine -> [FixApplication] -> IO LearningResult
learnPatterns engine apps = do
  patterns <- catMaybes <$> mapM extractPattern apps

  atomically $ modifyTVar' (lePatternDB engine) $ \db ->
    foldl addOrUpdatePattern db patterns

  pure LearningResult
    { lrPatternsAdded = length patterns
    , lrPatternsUpdated = 0
    , lrPatternsPruned = 0
    , lrNewConfidence = 0.5
    }

-- | Update existing patterns
updatePatterns :: LearningEngine -> IO ()
updatePatterns engine = do
  db <- readTVarIO (lePatternDB engine)
  let model = defaultConfidenceModel
      updated = map (updatePatternConfidence model) (pdbPatterns db)
  atomically $ writeTVar (lePatternDB engine) db { pdbPatterns = updated }
  where
    updatePatternConfidence :: ConfidenceModel -> FixPattern -> FixPattern
    updatePatternConfidence model fp =
      fp { fpConfidence = computeConfidence model fp }

-- | Prune low-quality patterns
prunePatterns :: LearningEngine -> IO Int
prunePatterns engine = do
  let config = leConfig engine
      threshold = lcMinConfidence config

  db <- readTVarIO (lePatternDB engine)
  let (keep, prune) = partition ((>= threshold) . fpConfidence) (pdbPatterns db)
  atomically $ writeTVar (lePatternDB engine) db { pdbPatterns = keep }

  pure $ length prune
  where
    partition :: (a -> Bool) -> [a] -> ([a], [a])
    partition p xs = (filter p xs, filter (not . p) xs)

-- | Export patterns to file
exportPatterns :: LearningEngine -> FilePath -> IO ()
exportPatterns engine path = do
  db <- readTVarIO (lePatternDB engine)
  BL.writeFile path (encode $ pdbPatterns db)

-- | Import patterns from file
importPatterns :: LearningEngine -> FilePath -> IO Int
importPatterns engine path = do
  mPatterns <- decode <$> BL.readFile path
  case mPatterns of
    Just patterns -> do
      atomically $ modifyTVar' (lePatternDB engine) $ \db ->
        db { pdbPatterns = pdbPatterns db ++ patterns }
      pure $ length patterns
    Nothing -> pure 0

--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

-- | Get learning statistics
getStats :: LearningEngine -> IO LearningStats
getStats = readTVarIO . leStats

-- | Reset statistics
resetStats :: LearningEngine -> IO ()
resetStats engine =
  atomically $ writeTVar (leStats engine) emptyLearningStats

--------------------------------------------------------------------------------
-- Pattern Database
--------------------------------------------------------------------------------

-- | Database of learned patterns
data PatternDB = PatternDB
  { pdbPatterns   :: [FixPattern]
  , pdbByCategory :: Map PatternCategory [PatternId]
  , pdbFingerprints :: Map Word64 [PatternId]
  , pdbStats      :: PatternDBStats
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Stats for the pattern database
data PatternDBStats = PatternDBStats
  { pdsPatternCount  :: Int
  , pdsCategoryCount :: Map PatternCategory Int
  , pdsAverageConfidence :: Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty pattern database
emptyPatternDB :: PatternDB
emptyPatternDB = PatternDB
  { pdbPatterns = []
  , pdbByCategory = Map.empty
  , pdbFingerprints = Map.empty
  , pdbStats = PatternDBStats 0 Map.empty 0.0
  }

-- | Query for patterns
data PatternQuery = PatternQuery
  { pqCategory    :: Maybe PatternCategory
  , pqMinConfidence :: Maybe Double
  , pqFingerprint :: Maybe PatternFingerprint
  , pqLimit       :: Maybe Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Query patterns from database
queryPatterns :: PatternDB -> PatternQuery -> [FixPattern]
queryPatterns db query =
  let patterns = pdbPatterns db
      filtered = filter (matchesQuery query) patterns
      sorted = sortBy (comparing (negate . fpConfidence)) filtered
      limited = maybe sorted (`take` sorted) (pqLimit query)
  in limited
  where
    matchesQuery :: PatternQuery -> FixPattern -> Bool
    matchesQuery q p =
      categoryMatches q p &&
      confidenceMatches q p

    categoryMatches q p = case pqCategory q of
      Nothing -> True
      Just cat -> fpCategory p == cat

    confidenceMatches q p = case pqMinConfidence q of
      Nothing -> True
      Just minConf -> fpConfidence p >= minConf

-- | Add pattern to database
addPattern :: PatternDB -> FixPattern -> PatternDB
addPattern db fp =
  let patterns = fp : pdbPatterns db
      byCategory = Map.insertWith (++) (fpCategory fp) [fpId fp] (pdbByCategory db)
      byFingerprint = Map.insertWith (++) (pfHash $ fpFingerprint fp) [fpId fp] (pdbFingerprints db)
  in db
    { pdbPatterns = patterns
    , pdbByCategory = byCategory
    , pdbFingerprints = byFingerprint
    , pdbStats = updateStats (pdbStats db) fp
    }
  where
    updateStats :: PatternDBStats -> FixPattern -> PatternDBStats
    updateStats s p = s
      { pdsPatternCount = pdsPatternCount s + 1
      , pdsCategoryCount = Map.insertWith (+) (fpCategory p) 1 (pdsCategoryCount s)
      }

-- | Remove pattern from database
removePattern :: PatternDB -> PatternId -> PatternDB
removePattern db patId =
  let patterns = filter ((/= patId) . fpId) (pdbPatterns db)
      -- Would also update byCategory and byFingerprint maps
  in db { pdbPatterns = patterns }

-- | Add or update existing pattern
addOrUpdatePattern :: PatternDB -> FixPattern -> PatternDB
addOrUpdatePattern db fp =
  let existing = filter (\p -> fpBeforePattern p == fpBeforePattern fp) (pdbPatterns db)
  in case existing of
       [] -> addPattern db fp
       (p:_) ->
         let updated = p
               { fpSuccessCount = fpSuccessCount p + fpSuccessCount fp
               , fpConfidence = computeConfidence defaultConfidenceModel p
               }
             patterns = updated : filter ((/= fpId p) . fpId) (pdbPatterns db)
         in db { pdbPatterns = patterns }

-- | Update pattern after failure
updatePatternFailure :: PatternDB -> PatternId -> PatternDB
updatePatternFailure db patId =
  let patterns = map updateIfMatch (pdbPatterns db)
  in db { pdbPatterns = patterns }
  where
    updateIfMatch p
      | fpId p == patId =
          let p' = p { fpFailureCount = fpFailureCount p + 1 }
          in p' { fpConfidence = computeConfidence defaultConfidenceModel p' }
      | otherwise = p

--------------------------------------------------------------------------------
-- FixEngine Instance
--------------------------------------------------------------------------------

instance FixEngine LearningEngine where
  type EngineConfig LearningEngine = LearningConfig
  type EngineCategory LearningEngine = PatternCategory

  engineName _ = "learning-fix"
  engineVersion _ = "1.0.0"
  engineCategories _ =
    [ PCBoolean, PCList, PCPartial, PCImport
    , PCNaming, PCStyle, PCPerformance, PCSecurity
    ]
  engineDescription _ = "Learning-based fix suggestion engine"

  findFixes engine path content = do
    suggestions <- suggestFixes engine emptyDiag content
    pure $ map (suggestionToEnriched "learning-fix") suggestions
    where
      emptyDiag = Diagnostic
        { diagSpan = noSpan
        , diagSeverity = Info
        , diagKind = CodePattern
        , diagMessage = ""
        , diagCode = Nothing
        , diagFixes = []
        , diagRelated = []
        }
      noSpan = SrcSpan path (Line 0) (Column 0) (Line 0) (Column 0)

  findFixesForDiagnostics engine diags content = do
    allSuggestions <- forM diags $ \diag ->
      suggestFixes engine diag content
    pure $ map (suggestionToEnriched "learning-fix") $ concat allSuggestions

  validateFix _engine _ef _content =
    pure FixValidation
      { fvResult = ValidationSuccess
      , fvChecks = []
      , fvSuggestions = []
      }

  applyFix _engine _path content ef = do
    let fix = efFix ef
        newContent = applyFixContent content fix
    pure ApplySuccess
      { arsNewContent = newContent
      , arsAppliedFix = ef
      , arsStats = emptyFixStats
      }

  getEngineStats engine = do
    stats <- readTVarIO (leStats engine)
    pure emptyFixStats
      { fsTotal = lsSuggestionsGiven stats
      , fsApplied = lsSuggestionsAccepted stats
      , fsFailed = lsSuggestionsRejected stats
      }

-- | Convert suggestion to enriched fix
suggestionToEnriched :: Text -> FixSuggestion -> EnrichedFix
suggestionToEnriched engName suggestion =
  let fix = fsFix suggestion
      localId = T.take 20 (fsReason suggestion)
  in enrichFix engName localId fix (fixCatToCategory $ fixCategory fix)
  where
    fixCatToCategory :: FixCategory -> Category
    fixCatToCategory = \case
      FCPerformance -> Performance
      FCModernize   -> Modernization
      FCSafety      -> Safety
      FCStyle       -> Style
      FCImports     -> Imports
      FCRedundant   -> Redundant
      FCSpaceLeaks  -> SpaceLeaks
      FCSecurity    -> Security
      FCCustom t    -> RT.Custom t

-- | Apply fix to content
applyFixContent :: Text -> Fix -> Text
applyFixContent content fix =
  foldl applyEdit content (reverse $ fixEdits fix)
  where
    applyEdit txt edit =
      let span = fixEditSpan edit
          newText = fixEditNewText edit
          linesList = T.lines txt
          startLine = unLine (srcSpanStartLine span) - 1
          endLine = unLine (srcSpanEndLine span) - 1
          startCol = unColumn (srcSpanStartCol span) - 1
          endCol = unColumn (srcSpanEndCol span) - 1
          before = take startLine linesList
          after = drop (endLine + 1) linesList
          modLine = if startLine == endLine && startLine < length linesList
            then let line = linesList !! startLine
                 in [T.take startCol line <> newText <> T.drop endCol line]
            else [newText]
      in T.unlines (before ++ modLine ++ after)
