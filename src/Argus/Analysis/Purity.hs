{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Argus.Analysis.Purity
-- Description : Flow-sensitive purity tracking and effect analysis
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides flow-sensitive purity tracking for Haskell code:
--
-- * Detect effectful operations in supposedly pure functions
-- * Track effect propagation through function calls
-- * Identify unsafe IO operations (unsafePerformIO, etc.)
-- * Analyze monad usage and effect systems
--
-- == Purity Levels
--
-- * 'PureComputation' - No effects, referentially transparent
-- * 'IOEffect' - Performs IO operations
-- * 'STEffect' - Uses mutable state in ST monad
-- * 'UnsafeEffect' - Uses unsafe operations like unsafePerformIO
-- * 'ExceptionEffect' - May throw exceptions
-- * 'PartialEffect' - May not terminate or uses partial functions
--
-- == Usage
--
-- @
-- result <- analyzePurity config parsedModule
-- case prPurityIssues result of
--   [] -> putStrLn "Module is pure"
--   issues -> mapM_ printIssue issues
-- @
module Argus.Analysis.Purity
  ( -- * Configuration
    PurityConfig (..)
  , defaultPurityConfig
  , PurityLevel (..)
  , EffectType (..)

    -- * Analysis Results
  , PurityResult (..)
  , PurityIssue (..)
  , FunctionPurity (..)
  , EffectSource (..)

    -- * Analysis Functions
  , analyzePurity
  , analyzeFunctionPurity
  , analyzeExpressionPurity
  , checkPurityViolation
  , inferEffects

    -- * Effect Classification
  , classifyEffect
  , isUnsafeFunction
  , isIOFunction
  , isSTFunction
  , hasPartialFunctions
  , hasExceptions

    -- * Purity Checking
  , isPure
  , isReferentiallyTransparent
  , hasHiddenEffects
  , effectsInScope

    -- * Utilities
  , purityToDiagnostic
  , effectTypeText
  , purityLevelText
  , combinePurityLevels
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON, ToJSONKey, FromJSONKey)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Argus.Types (SrcSpan, Diagnostic(..), Severity(..), DiagnosticKind(..), mkSrcSpanRaw)

--------------------------------------------------------------------------------
-- Purity Levels and Effects
--------------------------------------------------------------------------------

-- | Levels of purity for code analysis
data PurityLevel
  = PureComputation    -- ^ No effects, fully referentially transparent
  | MemoizedEffect     -- ^ Effect that's safe due to memoization (const values)
  | STEffect           -- ^ Uses ST monad for local mutable state
  | IOEffect           -- ^ Performs IO operations
  | UnsafeEffect       -- ^ Uses unsafe operations (unsafePerformIO, etc.)
  | ExceptionEffect    -- ^ May throw runtime exceptions
  | PartialEffect      -- ^ May not terminate or uses partial functions
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Types of effects detected in code
data EffectType
  = NoEffect                  -- ^ No detectable effect
  | IOReadEffect              -- ^ IO read operations (files, network, etc.)
  | IOWriteEffect             -- ^ IO write operations
  | IOBothEffect              -- ^ Both read and write IO
  | MutableStateEffect        -- ^ Mutable state (IORef, MVar, STRef)
  | ConcurrencyEffect         -- ^ Concurrent operations (forkIO, etc.)
  | UnsafeCoerceEffect        -- ^ Type-unsafe coercions
  | UnsafeIOEffect            -- ^ unsafePerformIO and similar
  | UnsafeInterleaveEffect    -- ^ unsafeInterleaveIO
  | FFIEffect                 -- ^ Foreign function interface calls
  | ExceptionThrowEffect      -- ^ Exception throwing
  | ExceptionCatchEffect      -- ^ Exception catching/handling
  | PartialFunctionEffect     -- ^ Partial function usage (head, tail, etc.)
  | NonTerminationEffect      -- ^ Potential non-termination
  | DebugTraceEffect          -- ^ Debug.Trace and similar
  | CustomEffect Text         -- ^ User-defined effect
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, ToJSONKey, FromJSONKey, NFData)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for purity analysis
data PurityConfig = PurityConfig
  { pcStrictMode          :: Bool
    -- ^ Require all functions to declare effects
  , pcAllowUnsafeInTests  :: Bool
    -- ^ Allow unsafe operations in test modules
  , pcAllowDebugTrace     :: Bool
    -- ^ Allow Debug.Trace usage (still reports as effect)
  , pcTreatPartialAsEffect :: Bool
    -- ^ Treat partial functions as effects
  , pcTreatExceptionAsEffect :: Bool
    -- ^ Treat exception throwing as effect
  , pcCustomUnsafeFunctions :: Set Text
    -- ^ Additional unsafe functions to detect
  , pcCustomPureFunctions :: Set Text
    -- ^ Functions known to be pure despite appearances
  , pcMonadEffectMapping  :: Map Text EffectType
    -- ^ Custom monad to effect type mapping
  , pcIgnoreModules       :: Set Text
    -- ^ Modules to ignore in analysis
  , pcMaxDepth            :: Int
    -- ^ Maximum call depth for effect propagation
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Default purity analysis configuration
defaultPurityConfig :: PurityConfig
defaultPurityConfig = PurityConfig
  { pcStrictMode = False
  , pcAllowUnsafeInTests = True
  , pcAllowDebugTrace = False
  , pcTreatPartialAsEffect = True
  , pcTreatExceptionAsEffect = True
  , pcCustomUnsafeFunctions = Set.empty
  , pcCustomPureFunctions = Set.empty
  , pcMonadEffectMapping = defaultMonadEffects
  , pcIgnoreModules = Set.fromList ["Test", "Spec", "Benchmark"]
  , pcMaxDepth = 10
  }

-- | Default mapping of monads to effect types
defaultMonadEffects :: Map Text EffectType
defaultMonadEffects = Map.fromList
  [ ("IO", IOBothEffect)
  , ("ST", MutableStateEffect)
  , ("STM", ConcurrencyEffect)
  , ("ReaderT", NoEffect)
  , ("WriterT", NoEffect)
  , ("StateT", NoEffect)  -- Pure monad transformer
  , ("ExceptT", ExceptionCatchEffect)
  , ("MaybeT", NoEffect)
  , ("EitherT", NoEffect)
  , ("ResourceT", IOBothEffect)
  ]

--------------------------------------------------------------------------------
-- Analysis Results
--------------------------------------------------------------------------------

-- | Source of an effect
data EffectSource = EffectSource
  { esFunction   :: Text
    -- ^ Function name causing the effect
  , esModule     :: Maybe Text
    -- ^ Module where function is defined
  , esEffect     :: EffectType
    -- ^ Type of effect
  , esSpan       :: Maybe SrcSpan
    -- ^ Location of the effect
  , esCallChain  :: [Text]
    -- ^ Call chain leading to the effect
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Purity analysis for a single function
data FunctionPurity = FunctionPurity
  { fpName         :: Text
    -- ^ Function name
  , fpSpan         :: Maybe SrcSpan
    -- ^ Function location
  , fpDeclaredType :: Maybe Text
    -- ^ Declared type signature (if any)
  , fpPurityLevel  :: PurityLevel
    -- ^ Inferred purity level
  , fpEffects      :: [EffectType]
    -- ^ All detected effects
  , fpEffectSources :: [EffectSource]
    -- ^ Where effects originate
  , fpIsPureByType :: Bool
    -- ^ Whether type signature claims purity
  , fpHasHiddenIO  :: Bool
    -- ^ Has IO hidden via unsafePerformIO
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | A purity violation issue
data PurityIssue = PurityIssue
  { piFunction      :: Text
    -- ^ Function with issue
  , piSpan          :: Maybe SrcSpan
    -- ^ Location of issue
  , piExpectedPurity :: PurityLevel
    -- ^ Expected purity from signature
  , piActualPurity  :: PurityLevel
    -- ^ Actual inferred purity
  , piViolationType :: PurityViolationType
    -- ^ Kind of purity violation
  , piEffectSources :: [EffectSource]
    -- ^ Sources of effects
  , piSeverity      :: Severity
    -- ^ Issue severity
  , piSuggestion    :: Maybe Text
    -- ^ Fix suggestion
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)
  -- Note: No NFData due to Severity not having it

-- | Types of purity violations
data PurityViolationType
  = HiddenIOInPureFunction    -- ^ IO hidden in pure function
  | UnsafeCoercionUsed        -- ^ unsafeCoerce used
  | UnsafePerformIOUsed       -- ^ unsafePerformIO used
  | UnsafeInterleaveIOUsed    -- ^ unsafeInterleaveIO used
  | PartialFunctionInTotal    -- ^ Partial function without safety
  | ExceptionInPureCode       -- ^ Exception thrown in pure code
  | FFICallInPureFunction     -- ^ FFI call in pure function
  | MutableStateInPure        -- ^ Mutable state in pure code
  | DebugTraceInProduction    -- ^ Debug.Trace in production code
  | EffectTypeMismatch        -- ^ Effect type doesn't match signature
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Complete purity analysis result
data PurityResult = PurityResult
  { prModuleName      :: Text
    -- ^ Module analyzed
  , prFunctionPurities :: [FunctionPurity]
    -- ^ Per-function purity analysis
  , prPurityIssues    :: [PurityIssue]
    -- ^ Detected purity violations
  , prOverallPurity   :: PurityLevel
    -- ^ Module-level purity
  , prEffectSummary   :: Map EffectType Int
    -- ^ Count of each effect type
  , prUnsafeLocations :: [SrcSpan]
    -- ^ All locations of unsafe code
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Known Unsafe and IO Functions
--------------------------------------------------------------------------------

-- | Functions known to be unsafe
knownUnsafeFunctions :: Set Text
knownUnsafeFunctions = Set.fromList
  [ "unsafePerformIO"
  , "unsafeInterleaveIO"
  , "unsafeDupablePerformIO"
  , "unsafeFixIO"
  , "unsafeCoerce"
  , "unsafeCoerce#"
  , "reallyUnsafePtrEquality#"
  , "inlinePerformIO"
  , "accursedUnutterablePerformIO"
  , "noDuplicate"
  , "touch#"
  -- Internal unsafe functions
  , "unsafeIOToST"
  , "unsafeSTToIO"
  , "unsafePrimToPrim"
  ]

-- | Functions known to perform IO
knownIOFunctions :: Set Text
knownIOFunctions = Set.fromList
  [ "putStr", "putStrLn", "print"
  , "getLine", "getChar", "getContents"
  , "readFile", "writeFile", "appendFile"
  , "readIO", "readLn"
  , "interact"
  , "openFile", "hClose", "hFlush"
  , "hGetLine", "hGetContents", "hPutStr", "hPutStrLn"
  , "getCurrentDirectory", "setCurrentDirectory"
  , "getEnv", "lookupEnv", "setEnv"
  , "getArgs", "getProgName"
  , "exitWith", "exitSuccess", "exitFailure"
  , "forkIO", "forkOS", "killThread"
  , "threadDelay"
  , "newIORef", "readIORef", "writeIORef", "modifyIORef"
  , "newMVar", "takeMVar", "putMVar", "readMVar"
  , "newTVar", "readTVar", "writeTVar"
  , "atomically"
  , "evaluate"
  , "throwIO", "throw"
  , "catch", "handle", "try", "bracket"
  , "finally", "onException"
  ]

-- | Functions that are partial (may throw exceptions)
knownPartialFunctions :: Set Text
knownPartialFunctions = Set.fromList
  [ "head", "tail", "init", "last"
  , "(!)", "!!"
  , "fromJust"
  , "read"
  , "error", "undefined", "errorWithoutStackTrace"
  , "succ", "pred"
  , "minimum", "maximum"
  , "foldr1", "foldl1"
  , "toEnum"
  , "quot", "rem", "div", "mod", "quotRem", "divMod"
  , "cycle"
  ]

-- | Debug functions that should not appear in production
knownDebugFunctions :: Set Text
knownDebugFunctions = Set.fromList
  [ "trace", "traceShow", "traceShowId", "traceId"
  , "traceM", "traceShowM"
  , "traceIO", "traceEventIO"
  , "traceStack"
  , "Debug.Trace.trace"
  ]

-- | FFI-related functions
knownFFIFunctions :: Set Text
knownFFIFunctions = Set.fromList
  [ "foreign", "ccall", "safe", "unsafe"
  , "peek", "poke", "peekByteOff", "pokeByteOff"
  , "malloc", "free", "alloca", "allocaBytes"
  , "castPtr", "plusPtr", "nullPtr"
  , "newForeignPtr", "withForeignPtr"
  ]

--------------------------------------------------------------------------------
-- Analysis Functions
--------------------------------------------------------------------------------

-- | Analyze purity of an entire module
analyzePurity :: PurityConfig -> Text -> Text -> PurityResult
analyzePurity config moduleName sourceCode =
  let functions = extractFunctions sourceCode
      purities = map (analyzeFunctionPurity config sourceCode) functions
      issues = concatMap (checkPurityViolation config) purities
      effectCounts = countEffects purities
      unsafeLocs = mapMaybe fpSpan $ filter fpHasHiddenIO purities
  in PurityResult
       { prModuleName = moduleName
       , prFunctionPurities = purities
       , prPurityIssues = issues
       , prOverallPurity = combinePurityLevels $ map fpPurityLevel purities
       , prEffectSummary = effectCounts
       , prUnsafeLocations = unsafeLocs
       }

-- | Analyze purity of a single function
analyzeFunctionPurity :: PurityConfig -> Text -> (Text, Maybe SrcSpan, Maybe Text) -> FunctionPurity
analyzeFunctionPurity config source (name, spanMay, typeSig) =
  let effects = inferEffects config source name
      purityLevel = effectsToPurityLevel effects
      isPureType = case typeSig of
        Just sig -> not (hasIOInType sig)
        Nothing -> True  -- Assume pure if no signature
      hasHidden = hasHiddenEffects config source name
  in FunctionPurity
       { fpName = name
       , fpSpan = spanMay
       , fpDeclaredType = typeSig
       , fpPurityLevel = purityLevel
       , fpEffects = effects
       , fpEffectSources = mapEffectSources name effects
       , fpIsPureByType = isPureType
       , fpHasHiddenIO = hasHidden
       }

-- | Analyze purity of an expression
analyzeExpressionPurity :: PurityConfig -> Text -> PurityLevel
analyzeExpressionPurity config expr =
  let effects = inferExprEffects config expr
  in effectsToPurityLevel effects

-- | Check for purity violations in a function
checkPurityViolation :: PurityConfig -> FunctionPurity -> [PurityIssue]
checkPurityViolation config fp@FunctionPurity{..}
  | fpIsPureByType && fpPurityLevel > PureComputation =
      [PurityIssue
        { piFunction = fpName
        , piSpan = fpSpan
        , piExpectedPurity = PureComputation
        , piActualPurity = fpPurityLevel
        , piViolationType = classifyViolation fp
        , piEffectSources = fpEffectSources
        , piSeverity = violationSeverity fpPurityLevel
        , piSuggestion = suggestFix fp
        }]
  | fpHasHiddenIO && pcStrictMode config =
      [PurityIssue
        { piFunction = fpName
        , piSpan = fpSpan
        , piExpectedPurity = PureComputation
        , piActualPurity = UnsafeEffect
        , piViolationType = HiddenIOInPureFunction
        , piEffectSources = fpEffectSources
        , piSeverity = Error
        , piSuggestion = Just "Remove unsafePerformIO or change type to IO"
        }]
  | otherwise = []

-- | Infer effects from function body
inferEffects :: PurityConfig -> Text -> Text -> [EffectType]
inferEffects config source funcName =
  let funcBody = extractFunctionBody source funcName
      customUnsafe = pcCustomUnsafeFunctions config
      allUnsafe = Set.union knownUnsafeFunctions customUnsafe
      effects = concat
        [ [UnsafeIOEffect | any (`containsFunction` funcBody) (Set.toList allUnsafe)]
        , [UnsafeCoerceEffect | containsFunction "unsafeCoerce" funcBody]
        , [UnsafeInterleaveEffect | containsFunction "unsafeInterleaveIO" funcBody]
        , [IOBothEffect | any (`containsFunction` funcBody) (Set.toList knownIOFunctions)]
        , [PartialFunctionEffect | pcTreatPartialAsEffect config &&
            any (`containsFunction` funcBody) (Set.toList knownPartialFunctions)]
        , [DebugTraceEffect | any (`containsFunction` funcBody) (Set.toList knownDebugFunctions)]
        , [FFIEffect | any (`containsFunction` funcBody) (Set.toList knownFFIFunctions)]
        , [ExceptionThrowEffect | containsFunction "throw" funcBody ||
            containsFunction "error" funcBody]
        ]
  in if null effects then [NoEffect] else effects

-- | Infer effects from an expression
inferExprEffects :: PurityConfig -> Text -> [EffectType]
inferExprEffects config expr = inferEffects config expr ""

--------------------------------------------------------------------------------
-- Effect Classification
--------------------------------------------------------------------------------

-- | Classify the type of effect
classifyEffect :: Text -> EffectType
classifyEffect funcName
  | funcName `Set.member` knownUnsafeFunctions = UnsafeIOEffect
  | funcName `Set.member` knownIOFunctions = IOBothEffect
  | funcName `Set.member` knownPartialFunctions = PartialFunctionEffect
  | funcName `Set.member` knownDebugFunctions = DebugTraceEffect
  | funcName `Set.member` knownFFIFunctions = FFIEffect
  | otherwise = NoEffect

-- | Check if a function is unsafe
isUnsafeFunction :: Text -> Bool
isUnsafeFunction name = name `Set.member` knownUnsafeFunctions

-- | Check if a function performs IO
isIOFunction :: Text -> Bool
isIOFunction name = name `Set.member` knownIOFunctions

-- | Check if a function uses ST monad
isSTFunction :: Text -> Bool
isSTFunction name = "runST" `T.isInfixOf` name || "ST " `T.isInfixOf` name

-- | Check if code uses partial functions
hasPartialFunctions :: Text -> Bool
hasPartialFunctions code =
  any (`containsFunction` code) (Set.toList knownPartialFunctions)

-- | Check if code can throw exceptions
hasExceptions :: Text -> Bool
hasExceptions code =
  containsFunction "throw" code ||
  containsFunction "error" code ||
  containsFunction "undefined" code

--------------------------------------------------------------------------------
-- Purity Checking
--------------------------------------------------------------------------------

-- | Check if code is pure (no effects)
isPure :: [EffectType] -> Bool
isPure effects = all (== NoEffect) effects

-- | Check if code is referentially transparent
isReferentiallyTransparent :: [EffectType] -> Bool
isReferentiallyTransparent effects =
  isPure effects || all isMemoizableEffect effects
  where
    isMemoizableEffect NoEffect = True
    isMemoizableEffect _ = False

-- | Check if code has hidden IO effects
hasHiddenEffects :: PurityConfig -> Text -> Text -> Bool
hasHiddenEffects config source funcName =
  let funcBody = extractFunctionBody source funcName
      customPure = pcCustomPureFunctions config
  in containsFunction "unsafePerformIO" funcBody &&
     not (funcName `Set.member` customPure)

-- | Get all effects currently in scope
effectsInScope :: PurityConfig -> Text -> Map Text EffectType
effectsInScope config source =
  let functions = extractFunctionNames source
      effectPairs = [(f, case inferEffects config source f of
                           (e:_) -> e
                           [] -> NoEffect) | f <- functions]
  in Map.fromList $ filter ((/= NoEffect) . snd) effectPairs

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Convert purity issue to diagnostic
purityToDiagnostic :: PurityIssue -> Diagnostic
purityToDiagnostic PurityIssue{..} = Diagnostic
  { diagSpan = case piSpan of
      Just sp -> sp
      Nothing -> mkSrcSpanRaw "<unknown>" 1 1 1 1
  , diagSeverity = piSeverity
  , diagKind = SecurityIssue
  , diagMessage = mkPurityMessage piViolationType piFunction
  , diagCode = Just $ "purity/" <> violationCode piViolationType
  , diagFixes = []
  , diagRelated = []
  }

-- | Generate purity violation message
mkPurityMessage :: PurityViolationType -> Text -> Text
mkPurityMessage vt func = case vt of
  HiddenIOInPureFunction ->
    "Function '" <> func <> "' has hidden IO effects via unsafePerformIO"
  UnsafeCoercionUsed ->
    "Function '" <> func <> "' uses unsafeCoerce which is type-unsafe"
  UnsafePerformIOUsed ->
    "Function '" <> func <> "' uses unsafePerformIO in a pure type signature"
  UnsafeInterleaveIOUsed ->
    "Function '" <> func <> "' uses unsafeInterleaveIO which can cause unpredictable behavior"
  PartialFunctionInTotal ->
    "Function '" <> func <> "' uses partial functions without safety guarantees"
  ExceptionInPureCode ->
    "Function '" <> func <> "' may throw exceptions in pure code"
  FFICallInPureFunction ->
    "Function '" <> func <> "' makes FFI calls in pure code"
  MutableStateInPure ->
    "Function '" <> func <> "' uses mutable state in pure code"
  DebugTraceInProduction ->
    "Function '" <> func <> "' contains Debug.Trace calls"
  EffectTypeMismatch ->
    "Function '" <> func <> "' effect type does not match its signature"

-- | Get code for violation type
violationCode :: PurityViolationType -> Text
violationCode = \case
  HiddenIOInPureFunction -> "hidden-io"
  UnsafeCoercionUsed -> "unsafe-coerce"
  UnsafePerformIOUsed -> "unsafe-perform-io"
  UnsafeInterleaveIOUsed -> "unsafe-interleave-io"
  PartialFunctionInTotal -> "partial-in-total"
  ExceptionInPureCode -> "exception-in-pure"
  FFICallInPureFunction -> "ffi-in-pure"
  MutableStateInPure -> "mutable-in-pure"
  DebugTraceInProduction -> "debug-trace"
  EffectTypeMismatch -> "effect-mismatch"

-- | Text representation of effect type
effectTypeText :: EffectType -> Text
effectTypeText = \case
  NoEffect -> "none"
  IOReadEffect -> "io-read"
  IOWriteEffect -> "io-write"
  IOBothEffect -> "io"
  MutableStateEffect -> "mutable-state"
  ConcurrencyEffect -> "concurrency"
  UnsafeCoerceEffect -> "unsafe-coerce"
  UnsafeIOEffect -> "unsafe-io"
  UnsafeInterleaveEffect -> "unsafe-interleave"
  FFIEffect -> "ffi"
  ExceptionThrowEffect -> "exception-throw"
  ExceptionCatchEffect -> "exception-catch"
  PartialFunctionEffect -> "partial"
  NonTerminationEffect -> "non-termination"
  DebugTraceEffect -> "debug-trace"
  CustomEffect t -> "custom:" <> t

-- | Text representation of purity level
purityLevelText :: PurityLevel -> Text
purityLevelText = \case
  PureComputation -> "pure"
  MemoizedEffect -> "memoized"
  STEffect -> "st"
  IOEffect -> "io"
  UnsafeEffect -> "unsafe"
  ExceptionEffect -> "exception"
  PartialEffect -> "partial"

-- | Combine multiple purity levels (takes worst)
combinePurityLevels :: [PurityLevel] -> PurityLevel
combinePurityLevels levels
  | null levels = PureComputation
  | otherwise = maximum levels

--------------------------------------------------------------------------------
-- Internal Helpers
--------------------------------------------------------------------------------

-- | Check if a function appears in code (simple text search)
containsFunction :: Text -> Text -> Bool
containsFunction func code =
  -- Look for function as word boundary
  T.isInfixOf (" " <> func <> " ") (" " <> code <> " ") ||
  T.isInfixOf (" " <> func <> "\n") code ||
  T.isInfixOf ("(" <> func <> " ") code ||
  T.isInfixOf ("(" <> func <> ")") code ||
  T.isInfixOf (" " <> func <> ")") code ||
  T.isInfixOf ("$" <> func) code

-- | Check if type contains IO
hasIOInType :: Text -> Bool
hasIOInType typeSig =
  "IO " `T.isInfixOf` typeSig ||
  "IO)" `T.isInfixOf` typeSig ||
  " IO" `T.isInfixOf` typeSig

-- | Extract function body (simplified - looks for function definition)
extractFunctionBody :: Text -> Text -> Text
extractFunctionBody source funcName =
  let ls = T.lines source
      startIdx = findFunctionStart funcName ls
      endIdx = case startIdx of
        Nothing -> Nothing
        Just start -> Just $ findFunctionEnd start ls
  in case (startIdx, endIdx) of
       (Just start, Just end) -> T.unlines $ take (end - start + 1) $ drop start ls
       _ -> ""
  where
    findFunctionStart :: Text -> [Text] -> Maybe Int
    findFunctionStart name lines' =
      let prefix = name <> " "
          matches = zip [0..] lines'
          found = filter (\(_, l) -> prefix `T.isPrefixOf` T.stripStart l) matches
      in fst <$> listToMaybe found

    findFunctionEnd :: Int -> [Text] -> Int
    findFunctionEnd start lines' =
      let remaining = drop (start + 1) lines'
          indented = takeWhile isIndented remaining
      in start + length indented

    isIndented :: Text -> Bool
    isIndented line = T.null line || T.head line == ' ' || T.head line == '\t'

    listToMaybe :: [a] -> Maybe a
    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x

-- | Extract all function names from source
extractFunctionNames :: Text -> [Text]
extractFunctionNames source =
  let ls = T.lines source
      defs = filter isFunctionDef ls
  in map extractName defs
  where
    isFunctionDef :: Text -> Bool
    isFunctionDef line =
      let stripped = T.stripStart line
      in not (T.null stripped) &&
         not ("--" `T.isPrefixOf` stripped) &&
         " = " `T.isInfixOf` stripped &&
         T.head stripped /= ' '

    extractName :: Text -> Text
    extractName line = T.takeWhile (/= ' ') $ T.stripStart line

-- | Extract functions with their spans and type signatures
extractFunctions :: Text -> [(Text, Maybe SrcSpan, Maybe Text)]
extractFunctions source =
  let names = extractFunctionNames source
  in map (\n -> (n, Nothing, findTypeSignature source n)) names

-- | Find type signature for a function
findTypeSignature :: Text -> Text -> Maybe Text
findTypeSignature source funcName =
  let ls = T.lines source
      sigPrefix = funcName <> " :: "
      found = filter (sigPrefix `T.isPrefixOf`) $ map T.stripStart ls
  in case found of
       (sig:_) -> Just sig
       [] -> Nothing

-- | Convert effects to purity level
effectsToPurityLevel :: [EffectType] -> PurityLevel
effectsToPurityLevel effects
  | any isUnsafeEff effects = UnsafeEffect
  | any isIOEff effects = IOEffect
  | any isSTEff effects = STEffect
  | any isExceptEff effects = ExceptionEffect
  | any isPartialEff effects = PartialEffect
  | otherwise = PureComputation
  where
    isUnsafeEff e = e `elem` [UnsafeIOEffect, UnsafeCoerceEffect, UnsafeInterleaveEffect]
    isIOEff e = e `elem` [IOReadEffect, IOWriteEffect, IOBothEffect, MutableStateEffect, ConcurrencyEffect]
    isSTEff e = e == MutableStateEffect
    isExceptEff e = e `elem` [ExceptionThrowEffect, ExceptionCatchEffect]
    isPartialEff e = e `elem` [PartialFunctionEffect, NonTerminationEffect]

-- | Count effects across functions
countEffects :: [FunctionPurity] -> Map EffectType Int
countEffects purities =
  let allEffects = concatMap fpEffects purities
  in Map.fromListWith (+) [(e, 1) | e <- allEffects, e /= NoEffect]

-- | Map effect sources for a function
mapEffectSources :: Text -> [EffectType] -> [EffectSource]
mapEffectSources funcName effects =
  [EffectSource
    { esFunction = funcName
    , esModule = Nothing
    , esEffect = e
    , esSpan = Nothing
    , esCallChain = []
    } | e <- effects, e /= NoEffect]

-- | Classify violation type from function purity
classifyViolation :: FunctionPurity -> PurityViolationType
classifyViolation FunctionPurity{..}
  | UnsafeIOEffect `elem` fpEffects = UnsafePerformIOUsed
  | UnsafeCoerceEffect `elem` fpEffects = UnsafeCoercionUsed
  | UnsafeInterleaveEffect `elem` fpEffects = UnsafeInterleaveIOUsed
  | PartialFunctionEffect `elem` fpEffects = PartialFunctionInTotal
  | ExceptionThrowEffect `elem` fpEffects = ExceptionInPureCode
  | FFIEffect `elem` fpEffects = FFICallInPureFunction
  | DebugTraceEffect `elem` fpEffects = DebugTraceInProduction
  | MutableStateEffect `elem` fpEffects = MutableStateInPure
  | otherwise = EffectTypeMismatch

-- | Get violation severity
violationSeverity :: PurityLevel -> Severity
violationSeverity = \case
  UnsafeEffect -> Error
  IOEffect -> Error
  ExceptionEffect -> Warning
  PartialEffect -> Warning
  STEffect -> Info
  MemoizedEffect -> Info
  PureComputation -> Info

-- | Suggest fix for purity issue
suggestFix :: FunctionPurity -> Maybe Text
suggestFix FunctionPurity{..}
  | fpHasHiddenIO =
      Just "Change function type to IO or remove unsafePerformIO"
  | PartialFunctionEffect `elem` fpEffects =
      Just "Use safe alternatives (e.g., headMay instead of head)"
  | DebugTraceEffect `elem` fpEffects =
      Just "Remove Debug.Trace calls before production"
  | ExceptionThrowEffect `elem` fpEffects =
      Just "Use Maybe/Either instead of throwing exceptions"
  | otherwise = Nothing
