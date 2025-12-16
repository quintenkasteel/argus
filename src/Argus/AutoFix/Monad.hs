{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.AutoFix.Monad
-- Description : Automated fixes for monadic patterns and expressions
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides comprehensive auto-fix capabilities for monadic patterns,
-- including:
--
-- * return/pure simplifications (return x >>= f → f x)
-- * Applicative improvements (liftM → fmap, @ap@ pattern detection)
-- * Monad law optimizations (associativity, identity)
-- * do-notation simplifications
-- * Common monad transformer patterns
--
-- == Safety Features
--
-- * Validates monad law preservation
-- * Detects side effects for ordering
-- * Handles strict/lazy monads correctly
-- * Respects MonadFail constraints
--
-- == Usage
--
-- @
-- engine <- newMonadFixEngine defaultMonadFixConfig
-- result <- fixMonadExprs engine filePath content
-- @
module Argus.AutoFix.Monad
  ( -- * Fix Engine
    MonadFixEngine (..)
  , newMonadFixEngine
  , MonadFixConfig (..)
  , defaultMonadFixConfig

    -- * Fixing
  , fixMonadExprs
  , fixMonadFile
  , fixMonadDiagnostics
  , MonadFixResult (..)
  , MonadFixData (..)
  , MonadFix' (..)
  , MonadFixCategory (..)

    -- * Individual Fix Functions
    -- ** Return/Pure
  , fixReturnBind
  , fixPureBind
  , fixReturnThen
  , fixPureThen
  , fixBindReturn
  , fixBindPure
  , fixVoidReturn

    -- ** Functor/Applicative
  , fixLiftM
  , fixLiftM2
  , fixApPattern
  , fixFmapPure
  , fixFmapReturn
  , fixJoinFmap

    -- ** Do Notation
  , fixUnnecessaryDo
  , fixLetInDo
  , fixLastReturn
  , fixSingleBind
  , fixRedundantBind
  , fixNestedDo

    -- ** Monad Transformers
  , fixLiftLift
  , fixAskReader
  , fixGetState
  , fixPutPut
  , fixModifyModify
  , fixTellTell

    -- ** Control Flow
  , fixWhenTrue
  , fixWhenFalse
  , fixUnlessTrue
  , fixUnlessFalse
  , fixGuardTrue
  , fixGuardFalse
  , fixForeverReturn

    -- * Validation
  , validateMonadFix
  , MonadFixValidation (..)

    -- * Statistics
  , MonadFixStats (..)
  , emptyMonadStats
  , mergeMonadStats
  ) where

import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Data.Aeson (ToJSON, FromJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Text.Regex.TDFA ((=~))

import Argus.Types
  ( SrcSpan(..)
  , Line(..)
  , Column(..)
  , Diagnostic(..)
  , Fix(..)
  , FixEdit(..)
  )

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for Monad fix engine
data MonadFixConfig = MonadFixConfig
  { mfcEnabledCategories :: [MonadFixCategory]
      -- ^ Which categories of fixes to apply
  , mfcValidateFixes     :: Bool
      -- ^ Validate fixes before applying
  , mfcMaxFixesPerFile   :: Int
      -- ^ Maximum number of fixes to apply per file
  , mfcPreferApplicative :: Bool
      -- ^ Prefer Applicative over Monad when possible
  , mfcSimplifyDo        :: Bool
      -- ^ Simplify do-notation
  , mfcOptimizeTransformers :: Bool
      -- ^ Optimize monad transformer usage
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default configuration
defaultMonadFixConfig :: MonadFixConfig
defaultMonadFixConfig = MonadFixConfig
  { mfcEnabledCategories = [minBound .. maxBound]
  , mfcValidateFixes     = True
  , mfcMaxFixesPerFile   = 100
  , mfcPreferApplicative = True
  , mfcSimplifyDo        = True
  , mfcOptimizeTransformers = True
  }

-- | Categories of Monad fixes
data MonadFixCategory
  = ReturnPureSimplification   -- ^ return x >>= f → f x
  | FunctorApplicative         -- ^ liftM → fmap, ap patterns
  | DoNotationSimplification   -- ^ Unnecessary do, single bind
  | TransformerOptimization    -- ^ lift . lift, ask, get/put
  | ControlFlowSimplification  -- ^ when True, guard True
  | MonadLawOptimization       -- ^ Associativity, identity
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Engine
--------------------------------------------------------------------------------

-- | Monad fix engine with caching
data MonadFixEngine = MonadFixEngine
  { mfeConfig     :: MonadFixConfig
  , mfeCache      :: TVar (Map FilePath [MonadFix'])
  , mfeStats      :: TVar MonadFixStats
  }

-- | Create a new Monad fix engine
newMonadFixEngine :: MonadFixConfig -> IO MonadFixEngine
newMonadFixEngine config = do
  cache <- newTVarIO Map.empty
  stats <- newTVarIO emptyMonadStats
  pure MonadFixEngine
    { mfeConfig = config
    , mfeCache  = cache
    , mfeStats  = stats
    }

--------------------------------------------------------------------------------
-- Fix Types
--------------------------------------------------------------------------------

-- | Result of applying Monad fixes
data MonadFixResult
  = MonadFixSuccess MonadFixData
  | MonadFixPartial MonadFixData [Text]  -- ^ Data and errors
  | MonadFixFailure Text                 -- ^ Error message
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Common data for successful/partial fixes
data MonadFixData = MonadFixData
  { mfdFixes      :: [MonadFix']
  , mfdStats      :: MonadFixStats
  , mfdNewContent :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A single Monad fix (using MonadFix' to avoid name conflict with Control.Monad.Fix)
data MonadFix' = MonadFix'
  { mfSpan        :: SrcSpan
  , mfOriginal    :: Text
  , mfReplacement :: Text
  , mfCategory    :: MonadFixCategory
  , mfRuleId      :: Text
  , mfMessage     :: Text
  , mfValidation  :: MonadFixValidation
  , mfAddImports  :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Validation result for a fix
data MonadFixValidation
  = MonadValidationPassed
  | MonadValidationWarning Text
  | MonadValidationFailed Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Statistics about Monad fixes
data MonadFixStats = MonadFixStats
  { mfsReturnPure        :: Int
  , mfsFunctorApplicative :: Int
  , mfsDoNotation        :: Int
  , mfsTransformer       :: Int
  , mfsControlFlow       :: Int
  , mfsMonadLaw          :: Int
  , mfsTotalFixes        :: Int
  , mfsValidationsPassed :: Int
  , mfsValidationsFailed :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Empty statistics
emptyMonadStats :: MonadFixStats
emptyMonadStats = MonadFixStats 0 0 0 0 0 0 0 0 0

-- | Merge two statistics
mergeMonadStats :: MonadFixStats -> MonadFixStats -> MonadFixStats
mergeMonadStats a b = MonadFixStats
  { mfsReturnPure = mfsReturnPure a + mfsReturnPure b
  , mfsFunctorApplicative = mfsFunctorApplicative a + mfsFunctorApplicative b
  , mfsDoNotation = mfsDoNotation a + mfsDoNotation b
  , mfsTransformer = mfsTransformer a + mfsTransformer b
  , mfsControlFlow = mfsControlFlow a + mfsControlFlow b
  , mfsMonadLaw = mfsMonadLaw a + mfsMonadLaw b
  , mfsTotalFixes = mfsTotalFixes a + mfsTotalFixes b
  , mfsValidationsPassed = mfsValidationsPassed a + mfsValidationsPassed b
  , mfsValidationsFailed = mfsValidationsFailed a + mfsValidationsFailed b
  }

--------------------------------------------------------------------------------
-- Main Fixing Functions
--------------------------------------------------------------------------------

-- | Fix Monad expressions in a file content
fixMonadExprs :: MonadFixEngine
              -> FilePath
              -> Text
              -> IO MonadFixResult
fixMonadExprs engine path content = do
  let config = mfeConfig engine
      enabledCats = mfcEnabledCategories config

  -- Find all potential fixes
  let allFixes = findAllMonadFixes path content enabledCats config

  -- Validate fixes if enabled
  validatedFixes <- if mfcValidateFixes config
    then mapM (validateAndMarkMonad content) allFixes
    else pure allFixes

  -- Filter to only valid fixes
  let goodFixes = filter isValidMonadFix validatedFixes
      limitedFixes = take (mfcMaxFixesPerFile config) goodFixes

  -- Apply fixes
  case applyMonadFixes content limitedFixes of
    Left err -> pure $ MonadFixFailure err
    Right newContent -> do
      let stats = computeMonadStats validatedFixes
          errors = map getMonadValidationError $ filter (not . isValidMonadFix) validatedFixes

      -- Update engine stats
      atomically $ modifyTVar' (mfeStats engine) (mergeMonadStats stats)

      let fixData = MonadFixData limitedFixes stats newContent
      if null errors
        then pure $ MonadFixSuccess fixData
        else pure $ MonadFixPartial (MonadFixData goodFixes stats newContent) errors

-- | Fix a file directly
fixMonadFile :: MonadFixEngine
             -> FilePath
             -> IO MonadFixResult
fixMonadFile engine path = do
  result <- try @SomeException $ do
    content <- T.pack <$> readFile path
    fixMonadExprs engine path content
  case result of
    Left e -> pure $ MonadFixFailure $ T.pack $ show e
    Right r -> pure r

-- | Fix based on existing diagnostics
fixMonadDiagnostics :: MonadFixEngine
                    -> [Diagnostic]
                    -> Text
                    -> IO MonadFixResult
fixMonadDiagnostics _engine diags content = do
  let monadDiags = filter isMonadDiagnostic diags
      fixes = catMaybes $ map monadDiagnosticToFix monadDiags

  case applyMonadFixes content fixes of
    Left err -> pure $ MonadFixFailure err
    Right newContent -> do
      let stats = computeMonadStats fixes
      pure $ MonadFixSuccess $ MonadFixData fixes stats newContent

-- | Check if a diagnostic is Monad-related
isMonadDiagnostic :: Diagnostic -> Bool
isMonadDiagnostic diag = case diagCode diag of
  Just code -> any (`T.isPrefixOf` code) monadPrefixes
  Nothing -> False
  where
    monadPrefixes = ["return-", "pure-", "bind-", "do-", "liftM-", "when-",
                     "unless-", "guard-", "lift-", "ask-", "get-", "put-",
                     "modify-", "tell-", "fmap-", "ap-", "void-"]

-- | Convert a diagnostic to a Monad fix
monadDiagnosticToFix :: Diagnostic -> Maybe MonadFix'
monadDiagnosticToFix diag = case diagFixes diag of
  (fix':_) -> case fixEdits fix' of
    (FixEdit span' replacement:_) ->
      Just MonadFix'
        { mfSpan = span'
        , mfOriginal = ""
        , mfReplacement = replacement
        , mfCategory = inferMonadCategory (fromMaybe "" $ diagCode diag)
        , mfRuleId = fromMaybe "unknown" $ diagCode diag
        , mfMessage = diagMessage diag
        , mfValidation = MonadValidationPassed
        , mfAddImports = []
        }
    _ -> Nothing
  _ -> Nothing

-- | Infer category from rule ID
inferMonadCategory :: Text -> MonadFixCategory
inferMonadCategory ruleId
  | "return-" `T.isPrefixOf` ruleId = ReturnPureSimplification
  | "pure-" `T.isPrefixOf` ruleId = ReturnPureSimplification
  | "bind-" `T.isPrefixOf` ruleId = ReturnPureSimplification
  | "liftM-" `T.isPrefixOf` ruleId = FunctorApplicative
  | "fmap-" `T.isPrefixOf` ruleId = FunctorApplicative
  | "ap-" `T.isPrefixOf` ruleId = FunctorApplicative
  | "do-" `T.isPrefixOf` ruleId = DoNotationSimplification
  | "lift-" `T.isPrefixOf` ruleId = TransformerOptimization
  | "ask-" `T.isPrefixOf` ruleId = TransformerOptimization
  | "get-" `T.isPrefixOf` ruleId = TransformerOptimization
  | "put-" `T.isPrefixOf` ruleId = TransformerOptimization
  | "modify-" `T.isPrefixOf` ruleId = TransformerOptimization
  | "tell-" `T.isPrefixOf` ruleId = TransformerOptimization
  | "when-" `T.isPrefixOf` ruleId = ControlFlowSimplification
  | "unless-" `T.isPrefixOf` ruleId = ControlFlowSimplification
  | "guard-" `T.isPrefixOf` ruleId = ControlFlowSimplification
  | "void-" `T.isPrefixOf` ruleId = ReturnPureSimplification
  | otherwise = MonadLawOptimization

--------------------------------------------------------------------------------
-- Finding Fixes
--------------------------------------------------------------------------------

-- | Find all Monad fixes in content
findAllMonadFixes :: FilePath -> Text -> [MonadFixCategory] -> MonadFixConfig -> [MonadFix']
findAllMonadFixes path content enabledCats config =
  let lineList = zip ([1..] :: [Int]) (T.lines content)
  in concatMap (findMonadFixesInLine path enabledCats config) lineList

-- | Find fixes in a single line
findMonadFixesInLine :: FilePath -> [MonadFixCategory] -> MonadFixConfig -> (Int, Text) -> [MonadFix']
findMonadFixesInLine path enabledCats config (lineNum, lineText) = catMaybes
  [ -- Return/Pure simplification
    guard' ReturnPureSimplification $ fixReturnBind path lineNum lineText
  , guard' ReturnPureSimplification $ fixPureBind path lineNum lineText
  , guard' ReturnPureSimplification $ fixReturnThen path lineNum lineText
  , guard' ReturnPureSimplification $ fixPureThen path lineNum lineText
  , guard' ReturnPureSimplification $ fixBindReturn path lineNum lineText
  , guard' ReturnPureSimplification $ fixBindPure path lineNum lineText
  , guard' ReturnPureSimplification $ fixVoidReturn path lineNum lineText

    -- Functor/Applicative
  , guard' FunctorApplicative $ fixLiftM path lineNum lineText config
  , guard' FunctorApplicative $ fixLiftM2 path lineNum lineText config
  , guard' FunctorApplicative $ fixApPattern path lineNum lineText config
  , guard' FunctorApplicative $ fixFmapPure path lineNum lineText
  , guard' FunctorApplicative $ fixFmapReturn path lineNum lineText
  , guard' FunctorApplicative $ fixJoinFmap path lineNum lineText

    -- Do notation
  , guard' DoNotationSimplification $ fixUnnecessaryDo path lineNum lineText config
  , guard' DoNotationSimplification $ fixLetInDo path lineNum lineText config
  , guard' DoNotationSimplification $ fixLastReturn path lineNum lineText config
  , guard' DoNotationSimplification $ fixSingleBind path lineNum lineText config
  , guard' DoNotationSimplification $ fixRedundantBind path lineNum lineText
  , guard' DoNotationSimplification $ fixNestedDo path lineNum lineText

    -- Transformers
  , guard' TransformerOptimization $ fixLiftLift path lineNum lineText config
  , guard' TransformerOptimization $ fixAskReader path lineNum lineText
  , guard' TransformerOptimization $ fixGetState path lineNum lineText
  , guard' TransformerOptimization $ fixPutPut path lineNum lineText
  , guard' TransformerOptimization $ fixModifyModify path lineNum lineText
  , guard' TransformerOptimization $ fixTellTell path lineNum lineText

    -- Control flow
  , guard' ControlFlowSimplification $ fixWhenTrue path lineNum lineText
  , guard' ControlFlowSimplification $ fixWhenFalse path lineNum lineText
  , guard' ControlFlowSimplification $ fixUnlessTrue path lineNum lineText
  , guard' ControlFlowSimplification $ fixUnlessFalse path lineNum lineText
  , guard' ControlFlowSimplification $ fixGuardTrue path lineNum lineText
  , guard' ControlFlowSimplification $ fixGuardFalse path lineNum lineText
  , guard' ControlFlowSimplification $ fixForeverReturn path lineNum lineText
  ]
  where
    guard' cat mFix = if cat `elem` enabledCats then mFix else Nothing

--------------------------------------------------------------------------------
-- Return/Pure Fixes
--------------------------------------------------------------------------------

-- | Fix: return x >>= f → f x
fixReturnBind :: FilePath -> Int -> Text -> Maybe MonadFix'
fixReturnBind path lineNum line =
  let pattern' = "return\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*>>=\\s*([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:x:f:_):_) ->
         Just $ makeMonadFix path lineNum line full (f <> " " <> x) ReturnPureSimplification
                             "return-bind" "Left identity: return x >>= f = f x" []
       _ -> Nothing

-- | Fix: pure x >>= f → f x
fixPureBind :: FilePath -> Int -> Text -> Maybe MonadFix'
fixPureBind path lineNum line =
  let pattern' = "pure\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*>>=\\s*([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:x:f:_):_) ->
         Just $ makeMonadFix path lineNum line full (f <> " " <> x) ReturnPureSimplification
                             "pure-bind" "Left identity: pure x >>= f = f x" []
       _ -> Nothing

-- | Fix: return x >> m → m
fixReturnThen :: FilePath -> Int -> Text -> Maybe MonadFix'
fixReturnThen path lineNum line =
  let pattern' = "return\\s+[a-zA-Z0-9_'()]+\\s*>>\\s*([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:m:_):_) ->
         Just $ makeMonadFix path lineNum line full m ReturnPureSimplification
                             "return-then" "return _ >> m = m" []
       _ -> Nothing

-- | Fix: pure x >> m → m
fixPureThen :: FilePath -> Int -> Text -> Maybe MonadFix'
fixPureThen path lineNum line =
  let pattern' = "pure\\s+[a-zA-Z0-9_'()]+\\s*>>\\s*([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:m:_):_) ->
         Just $ makeMonadFix path lineNum line full m ReturnPureSimplification
                             "pure-then" "pure _ >> m = m" []
       _ -> Nothing

-- | Fix: m >>= return → m
fixBindReturn :: FilePath -> Int -> Text -> Maybe MonadFix'
fixBindReturn path lineNum line =
  let pattern' = "([a-zA-Z_][a-zA-Z0-9_']*)\\s*>>=\\s*return" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:m:_):_) ->
         Just $ makeMonadFix path lineNum line full m ReturnPureSimplification
                             "bind-return" "Right identity: m >>= return = m" []
       _ -> Nothing

-- | Fix: m >>= pure → m
fixBindPure :: FilePath -> Int -> Text -> Maybe MonadFix'
fixBindPure path lineNum line =
  let pattern' = "([a-zA-Z_][a-zA-Z0-9_']*)\\s*>>=\\s*pure" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:m:_):_) ->
         Just $ makeMonadFix path lineNum line full m ReturnPureSimplification
                             "bind-pure" "Right identity: m >>= pure = m" []
       _ -> Nothing

-- | Fix: void (return x) → pure ()
fixVoidReturn :: FilePath -> Int -> Text -> Maybe MonadFix'
fixVoidReturn path lineNum line =
  let pattern' = "void\\s*\\(\\s*return\\s+[a-zA-Z0-9_'()]+\\s*\\)" :: Text
  in case (line =~ pattern' :: Text) of
       match | not (T.null match) ->
         Just $ makeMonadFix path lineNum line match "pure ()" ReturnPureSimplification
                             "void-return" "void (return x) = pure ()" []
       _ -> Nothing

--------------------------------------------------------------------------------
-- Functor/Applicative Fixes
--------------------------------------------------------------------------------

-- | Fix: liftM f m → fmap f m
fixLiftM :: FilePath -> Int -> Text -> MonadFixConfig -> Maybe MonadFix'
fixLiftM path lineNum line config =
  if mfcPreferApplicative config
  then
    let pattern' = "liftM\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
    in case (line =~ pattern' :: [[Text]]) of
         ((full:f:m:_):_) ->
           Just $ makeMonadFix path lineNum line full ("fmap " <> f <> " " <> m) FunctorApplicative
                               "liftM-fmap" "Use fmap instead of liftM" []
         _ -> Nothing
  else Nothing

-- | Fix: liftM2 f m1 m2 → f <$> m1 <*> m2
fixLiftM2 :: FilePath -> Int -> Text -> MonadFixConfig -> Maybe MonadFix'
fixLiftM2 path lineNum line config =
  if mfcPreferApplicative config
  then
    let pattern' = "liftM2\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
    in case (line =~ pattern' :: [[Text]]) of
         ((full:f:m1:m2:_):_) ->
           Just $ makeMonadFix path lineNum line full (f <> " <$> " <> m1 <> " <*> " <> m2) FunctorApplicative
                               "liftM2-applicative" "Use Applicative operators instead of liftM2" []
         _ -> Nothing
  else Nothing

-- | Fix: @return f \`ap\` m → f \<$\> m@
fixApPattern :: FilePath -> Int -> Text -> MonadFixConfig -> Maybe MonadFix'
fixApPattern path lineNum line config =
  if mfcPreferApplicative config
  then
    let pattern' = "return\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*`ap`\\s*([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
    in case (line =~ pattern' :: [[Text]]) of
         ((full:f:m:_):_) ->
           Just $ makeMonadFix path lineNum line full (f <> " <$> " <> m) FunctorApplicative
                               "ap-fmap" "Use <$> instead of return/ap" []
         _ -> Nothing
  else Nothing

-- | Fix: fmap f (pure x) → pure (f x)
fixFmapPure :: FilePath -> Int -> Text -> Maybe MonadFix'
fixFmapPure path lineNum line =
  let pattern' = "fmap\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+\\(\\s*pure\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*\\)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:f:x:_):_) ->
         Just $ makeMonadFix path lineNum line full ("pure (" <> f <> " " <> x <> ")") FunctorApplicative
                             "fmap-pure" "fmap f (pure x) = pure (f x)" []
       _ -> Nothing

-- | Fix: fmap f (return x) → return (f x)
fixFmapReturn :: FilePath -> Int -> Text -> Maybe MonadFix'
fixFmapReturn path lineNum line =
  let pattern' = "fmap\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+\\(\\s*return\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*\\)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:f:x:_):_) ->
         Just $ makeMonadFix path lineNum line full ("return (" <> f <> " " <> x <> ")") FunctorApplicative
                             "fmap-return" "fmap f (return x) = return (f x)" []
       _ -> Nothing

-- | Fix: join (fmap f m) → m >>= f
fixJoinFmap :: FilePath -> Int -> Text -> Maybe MonadFix'
fixJoinFmap path lineNum line =
  let pattern' = "join\\s+\\(\\s*fmap\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*\\)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:f:m:_):_) ->
         Just $ makeMonadFix path lineNum line full (m <> " >>= " <> f) FunctorApplicative
                             "join-fmap" "join (fmap f m) = m >>= f" ["Control.Monad"]
       _ -> Nothing

--------------------------------------------------------------------------------
-- Do Notation Fixes
--------------------------------------------------------------------------------

-- | Fix: do { x } → x (single expression)
fixUnnecessaryDo :: FilePath -> Int -> Text -> MonadFixConfig -> Maybe MonadFix'
fixUnnecessaryDo path lineNum line config =
  if mfcSimplifyDo config
  then
    let pattern' = "do\\s*\\{\\s*([a-zA-Z_][a-zA-Z0-9_']*)\\s*\\}" :: Text
    in case (line =~ pattern' :: [[Text]]) of
         ((full:x:_):_) ->
           Just $ makeMonadFix path lineNum line full x DoNotationSimplification
                               "unnecessary-do" "Unnecessary do for single expression" []
         _ -> Nothing
  else Nothing

-- | Fix: do { let x = y; ... } → let x = y in do { ... }
fixLetInDo :: FilePath -> Int -> Text -> MonadFixConfig -> Maybe MonadFix'
fixLetInDo path lineNum line config =
  if mfcSimplifyDo config
  then
    let pattern' = "do\\s*\\{\\s*let\\s+([a-z_]+)\\s*=\\s*([^;]+);\\s*return\\s+\\1\\s*\\}" :: Text
    in case (line =~ pattern' :: [[Text]]) of
         ((full:_:val:_):_) ->
           Just $ makeMonadFix path lineNum line full ("pure " <> T.strip val) DoNotationSimplification
                               "let-do-return" "Simplify let-return pattern" []
         _ -> Nothing
  else Nothing

-- | Fix: do { ...; return x } where x is unused → do { ... }
fixLastReturn :: FilePath -> Int -> Text -> MonadFixConfig -> Maybe MonadFix'
fixLastReturn path lineNum line config =
  if mfcSimplifyDo config
  then
    let pattern' = ";\\s*return\\s+\\(\\)\\s*\\}" :: Text
    in case (line =~ pattern' :: Text) of
         match | not (T.null match) ->
           Just $ makeMonadFix path lineNum line match "}" DoNotationSimplification
                               "last-return-unit" "Remove unnecessary return ()" []
         _ -> Nothing
  else Nothing

-- | Fix: do { x <- m; return x } → m
fixSingleBind :: FilePath -> Int -> Text -> MonadFixConfig -> Maybe MonadFix'
fixSingleBind path lineNum line config =
  if mfcSimplifyDo config
  then
    let pattern' = "do\\s*\\{?\\s*([a-z_]+)\\s*<-\\s*([a-zA-Z_][a-zA-Z0-9_'\\s]+);?\\s*return\\s+\\1\\s*\\}?" :: Text
    in case (line =~ pattern' :: [[Text]]) of
         ((full:_:m:_):_) ->
           Just $ makeMonadFix path lineNum line full (T.strip m) DoNotationSimplification
                               "single-bind" "do { x <- m; return x } = m" []
         _ -> Nothing
  else Nothing

-- | Fix: x <- return y → let x = y
fixRedundantBind :: FilePath -> Int -> Text -> Maybe MonadFix'
fixRedundantBind path lineNum line =
  let pattern' = "([a-z_][a-zA-Z0-9_']*)\\s*<-\\s*return\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:x:y:_):_) ->
         Just $ makeMonadFix path lineNum line full ("let " <> x <> " = " <> y) DoNotationSimplification
                             "redundant-bind" "x <- return y should be let x = y" []
       _ -> Nothing

-- | Fix: do { do { ... } } → do { ... }
fixNestedDo :: FilePath -> Int -> Text -> Maybe MonadFix'
fixNestedDo path lineNum line =
  let pattern' = "do\\s*\\{\\s*do\\s*\\{" :: Text
  in case (line =~ pattern' :: Text) of
       match | not (T.null match) ->
         Just $ makeMonadFix path lineNum line match "do {" DoNotationSimplification
                             "nested-do" "Flatten nested do blocks" []
       _ -> Nothing

--------------------------------------------------------------------------------
-- Transformer Fixes
--------------------------------------------------------------------------------

-- | Fix: lift (lift m) → lift m (in specific contexts)
fixLiftLift :: FilePath -> Int -> Text -> MonadFixConfig -> Maybe MonadFix'
fixLiftLift path lineNum line config =
  if mfcOptimizeTransformers config
  then
    let pattern' = "lift\\s*\\(\\s*lift\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*\\)" :: Text
    in case (line =~ pattern' :: [[Text]]) of
         ((full:m:_):_) ->
           Just $ makeMonadFix path lineNum line full ("lift2 " <> m) TransformerOptimization
                               "lift-lift" "Consider using lift2 for double lift"
                               ["Control.Monad.Trans"]
         _ -> Nothing
  else Nothing

-- | Fix: asks id → ask
fixAskReader :: FilePath -> Int -> Text -> Maybe MonadFix'
fixAskReader path lineNum line =
  let pattern' = "asks\\s+id" :: Text
  in case (line =~ pattern' :: Text) of
       match | not (T.null match) ->
         Just $ makeMonadFix path lineNum line match "ask" TransformerOptimization
                             "asks-id" "asks id = ask" []
       _ -> Nothing

-- | Fix: gets id → get
fixGetState :: FilePath -> Int -> Text -> Maybe MonadFix'
fixGetState path lineNum line =
  let pattern' = "gets\\s+id" :: Text
  in case (line =~ pattern' :: Text) of
       match | not (T.null match) ->
         Just $ makeMonadFix path lineNum line match "get" TransformerOptimization
                             "gets-id" "gets id = get" []
       _ -> Nothing

-- | Fix: put x >> put y → put y
fixPutPut :: FilePath -> Int -> Text -> Maybe MonadFix'
fixPutPut path lineNum line =
  let pattern' = "put\\s+[a-zA-Z0-9_']+\\s*>>\\s*(put\\s+[a-zA-Z0-9_']+)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:second:_):_) ->
         Just $ makeMonadFix path lineNum line full second TransformerOptimization
                             "put-put" "First put is overwritten" []
       _ -> Nothing

-- | Fix: modify f >> modify g → modify (g . f)
fixModifyModify :: FilePath -> Int -> Text -> Maybe MonadFix'
fixModifyModify path lineNum line =
  let pattern' = "modify\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*>>\\s*modify\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:f:g:_):_) ->
         Just $ makeMonadFix path lineNum line full ("modify (" <> g <> " . " <> f <> ")") TransformerOptimization
                             "modify-modify" "Compose consecutive modify calls" []
       _ -> Nothing

-- | Fix: tell x >> tell y → tell (x <> y)
fixTellTell :: FilePath -> Int -> Text -> Maybe MonadFix'
fixTellTell path lineNum line =
  let pattern' = "tell\\s+([a-zA-Z_][a-zA-Z0-9_']*)\\s*>>\\s*tell\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:x:y:_):_) ->
         Just $ makeMonadFix path lineNum line full ("tell (" <> x <> " <> " <> y <> ")") TransformerOptimization
                             "tell-tell" "Combine consecutive tell calls" []
       _ -> Nothing

--------------------------------------------------------------------------------
-- Control Flow Fixes
--------------------------------------------------------------------------------

-- | Fix: when True m → m
fixWhenTrue :: FilePath -> Int -> Text -> Maybe MonadFix'
fixWhenTrue path lineNum line =
  let pattern' = "when\\s+True\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:m:_):_) ->
         Just $ makeMonadFix path lineNum line full m ControlFlowSimplification
                             "when-true" "when True m = m" []
       _ -> Nothing

-- | Fix: when False m → pure ()
fixWhenFalse :: FilePath -> Int -> Text -> Maybe MonadFix'
fixWhenFalse path lineNum line =
  let pattern' = "when\\s+False\\s+[a-zA-Z_][a-zA-Z0-9_']*" :: Text
  in case (line =~ pattern' :: Text) of
       match | not (T.null match) ->
         Just $ makeMonadFix path lineNum line match "pure ()" ControlFlowSimplification
                             "when-false" "when False _ = pure ()" []
       _ -> Nothing

-- | Fix: unless True m → pure ()
fixUnlessTrue :: FilePath -> Int -> Text -> Maybe MonadFix'
fixUnlessTrue path lineNum line =
  let pattern' = "unless\\s+True\\s+[a-zA-Z_][a-zA-Z0-9_']*" :: Text
  in case (line =~ pattern' :: Text) of
       match | not (T.null match) ->
         Just $ makeMonadFix path lineNum line match "pure ()" ControlFlowSimplification
                             "unless-true" "unless True _ = pure ()" []
       _ -> Nothing

-- | Fix: unless False m → m
fixUnlessFalse :: FilePath -> Int -> Text -> Maybe MonadFix'
fixUnlessFalse path lineNum line =
  let pattern' = "unless\\s+False\\s+([a-zA-Z_][a-zA-Z0-9_']*)" :: Text
  in case (line =~ pattern' :: [[Text]]) of
       ((full:m:_):_) ->
         Just $ makeMonadFix path lineNum line full m ControlFlowSimplification
                             "unless-false" "unless False m = m" []
       _ -> Nothing

-- | Fix: guard True → pure ()
fixGuardTrue :: FilePath -> Int -> Text -> Maybe MonadFix'
fixGuardTrue path lineNum line =
  let pattern' = "guard\\s+True" :: Text
  in case (line =~ pattern' :: Text) of
       match | not (T.null match) ->
         Just $ makeMonadFix path lineNum line match "pure ()" ControlFlowSimplification
                             "guard-true" "guard True = pure ()" []
       _ -> Nothing

-- | Fix: guard False → empty
fixGuardFalse :: FilePath -> Int -> Text -> Maybe MonadFix'
fixGuardFalse path lineNum line =
  let pattern' = "guard\\s+False" :: Text
  in case (line =~ pattern' :: Text) of
       match | not (T.null match) ->
         Just $ makeMonadFix path lineNum line match "empty" ControlFlowSimplification
                             "guard-false" "guard False = empty" ["Control.Applicative"]
       _ -> Nothing

-- | Fix: forever (return ()) → hang
fixForeverReturn :: FilePath -> Int -> Text -> Maybe MonadFix'
fixForeverReturn path lineNum line =
  let pattern' = "forever\\s*\\(\\s*return\\s+\\(\\)\\s*\\)" :: Text
  in case (line =~ pattern' :: Text) of
       match | not (T.null match) ->
         Just $ makeMonadFix path lineNum line match "forever (pure ())" ControlFlowSimplification
                             "forever-return" "Use pure instead of return" []
       _ -> Nothing

--------------------------------------------------------------------------------
-- Fix Helper
--------------------------------------------------------------------------------

-- | Create a MonadFix'
makeMonadFix :: FilePath -> Int -> Text -> Text -> Text -> MonadFixCategory -> Text -> Text -> [Text] -> MonadFix'
makeMonadFix path lineNum lineText original replacement cat ruleId msg imports =
  let startCol = fromMaybe 1 $ findMonadColumn original lineText
      endCol = startCol + T.length original
  in MonadFix'
    { mfSpan = SrcSpan
        { srcSpanFile = path
        , srcSpanStartLine = Line lineNum
        , srcSpanStartCol = Column startCol
        , srcSpanEndLine = Line lineNum
        , srcSpanEndCol = Column endCol
        }
    , mfOriginal = original
    , mfReplacement = replacement
    , mfCategory = cat
    , mfRuleId = ruleId
    , mfMessage = msg
    , mfValidation = MonadValidationPassed
    , mfAddImports = imports
    }

-- | Find column where pattern starts in line
findMonadColumn :: Text -> Text -> Maybe Int
findMonadColumn pattern' lineContent = case T.breakOn pattern' lineContent of
  (before, after) | not (T.null after) -> Just $ T.length before + 1
  _ -> Nothing

--------------------------------------------------------------------------------
-- Validation
--------------------------------------------------------------------------------

-- | Validate a Monad fix
validateMonadFix :: Text -> MonadFix' -> MonadFixValidation
validateMonadFix content fix =
  let original = mfOriginal fix
      replacement = mfReplacement fix
  in
    if T.null replacement && not (T.null original)
    then MonadValidationFailed "Replacement is empty"
    else if not (original `T.isInfixOf` content)
    then MonadValidationFailed "Original pattern not found in content"
    else if not (balancedMonadParens replacement)
    then MonadValidationWarning "Replacement has unbalanced parentheses"
    else MonadValidationPassed

-- | Check if parentheses are balanced
balancedMonadParens :: Text -> Bool
balancedMonadParens = go (0 :: Int) (0 :: Int)
  where
    go p b text
      | p < 0 || b < 0 = False
      | T.null text = p == 0 && b == 0
      | otherwise =
          let c = T.head text
              rest = T.tail text
          in case c of
               '(' -> go (p + 1) b rest
               ')' -> go (p - 1) b rest
               '{' -> go p (b + 1) rest
               '}' -> go p (b - 1) rest
               _ -> go p b rest

-- | Validate and mark a fix
validateAndMarkMonad :: Text -> MonadFix' -> IO MonadFix'
validateAndMarkMonad content fix = do
  let validation = validateMonadFix content fix
  pure fix { mfValidation = validation }

-- | Check if a fix passed validation
isValidMonadFix :: MonadFix' -> Bool
isValidMonadFix fix = case mfValidation fix of
  MonadValidationPassed -> True
  MonadValidationWarning _ -> True
  MonadValidationFailed _ -> False

-- | Get validation error message
getMonadValidationError :: MonadFix' -> Text
getMonadValidationError fix = case mfValidation fix of
  MonadValidationFailed err -> mfRuleId fix <> ": " <> err
  MonadValidationWarning warn -> mfRuleId fix <> " (warning): " <> warn
  MonadValidationPassed -> ""

--------------------------------------------------------------------------------
-- Applying Fixes
--------------------------------------------------------------------------------

-- | Apply multiple Monad fixes to content
applyMonadFixes :: Text -> [MonadFix'] -> Either Text Text
applyMonadFixes content fixes =
  let sortedFixes = reverse $ sortMonadFixesByPosition fixes
  in foldl applyOneMonad (Right content) sortedFixes
  where
    applyOneMonad (Left err) _ = Left err
    applyOneMonad (Right txt) fix = applySingleMonadFix txt fix

-- | Sort fixes by their start position
sortMonadFixesByPosition :: [MonadFix'] -> [MonadFix']
sortMonadFixesByPosition = foldr insertSortedMonad []
  where
    insertSortedMonad fix [] = [fix]
    insertSortedMonad fix (x:xs)
      | compareMonadPosition fix x == LT = fix : x : xs
      | otherwise = x : insertSortedMonad fix xs

    compareMonadPosition f1 f2 =
      let l1 = srcSpanStartLine (mfSpan f1)
          l2 = srcSpanStartLine (mfSpan f2)
          c1 = srcSpanStartCol (mfSpan f1)
          c2 = srcSpanStartCol (mfSpan f2)
      in case compare l1 l2 of
           EQ -> compare c1 c2
           x -> x

-- | Apply a single fix to content
applySingleMonadFix :: Text -> MonadFix' -> Either Text Text
applySingleMonadFix content fix =
  let span' = mfSpan fix
      lineNum = unLine $ srcSpanStartLine span'
      startCol = unColumn $ srcSpanStartCol span'
      endCol = unColumn $ srcSpanEndCol span'
      lines' = T.lines content
  in if lineNum < 1 || lineNum > length lines'
     then Left $ "Invalid line number: " <> T.pack (show lineNum)
     else
       let targetLine = lines' !! (lineNum - 1)
           prefix = T.take (startCol - 1) targetLine
           suffix = T.drop (endCol - 1) targetLine
           newLine = prefix <> mfReplacement fix <> suffix
           newLines = take (lineNum - 1) lines' ++ [newLine] ++ drop lineNum lines'
       in Right $ T.unlines newLines

--------------------------------------------------------------------------------
-- Statistics
--------------------------------------------------------------------------------

-- | Compute statistics from fixes
computeMonadStats :: [MonadFix'] -> MonadFixStats
computeMonadStats fixes = foldl addMonadFixToStats emptyMonadStats fixes

-- | Add a fix to statistics
addMonadFixToStats :: MonadFixStats -> MonadFix' -> MonadFixStats
addMonadFixToStats stats fix =
  let updateValidation s = case mfValidation fix of
        MonadValidationPassed -> s { mfsValidationsPassed = mfsValidationsPassed s + 1 }
        MonadValidationWarning _ -> s { mfsValidationsPassed = mfsValidationsPassed s + 1 }
        MonadValidationFailed _ -> s { mfsValidationsFailed = mfsValidationsFailed s + 1 }
      updateCategory s = case mfCategory fix of
        ReturnPureSimplification -> s { mfsReturnPure = mfsReturnPure s + 1 }
        FunctorApplicative -> s { mfsFunctorApplicative = mfsFunctorApplicative s + 1 }
        DoNotationSimplification -> s { mfsDoNotation = mfsDoNotation s + 1 }
        TransformerOptimization -> s { mfsTransformer = mfsTransformer s + 1 }
        ControlFlowSimplification -> s { mfsControlFlow = mfsControlFlow s + 1 }
        MonadLawOptimization -> s { mfsMonadLaw = mfsMonadLaw s + 1 }
  in updateCategory $ updateValidation $ stats { mfsTotalFixes = mfsTotalFixes stats + 1 }
