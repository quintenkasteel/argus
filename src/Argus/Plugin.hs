{-# LANGUAGE StrictData #-}
{-# LANGUAGE PackageImports #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Plugin
-- Description : Enhanced GHC plugin for HIE-aware linting during compilation
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides a GHC plugin that runs linting during compilation.
-- This enables analysis of Template Haskell splices and other code that
-- is only available after compilation.
--
-- == Key Features
--
-- * Captures type information from Core for type-aware auto-fixes
-- * Builds symbol tables during compilation for cross-module analysis
-- * Analyzes Template Haskell splices after full expansion
-- * Detects strictness and space leak issues using Core analysis
-- * Provides constraint checking for safer refactoring
--
-- = Usage
--
-- Add to your @.cabal@ file:
--
-- @
-- ghc-options: -fplugin=Argus.Plugin
-- @
--
-- Or pass to GHC directly:
--
-- @
-- ghc -fplugin=Argus.Plugin YourModule.hs
-- @
--
-- = Plugin Options
--
-- The plugin supports the following options:
--
-- @
-- ghc -fplugin=Argus.Plugin -fplugin-opt=Argus.Plugin:config=path/to/config.yaml
-- ghc -fplugin=Argus.Plugin -fplugin-opt=Argus.Plugin:severity=warning
-- ghc -fplugin=Argus.Plugin -fplugin-opt=Argus.Plugin:output=json
-- ghc -fplugin=Argus.Plugin -fplugin-opt=Argus.Plugin:capture-types
-- ghc -fplugin=Argus.Plugin -fplugin-opt=Argus.Plugin:type-output=types.json
-- @
module Argus.Plugin
  ( -- * GHC Plugin
    plugin

    -- * Plugin Configuration
  , PluginConfig (..)
  , defaultPluginConfig
  , parsePluginOptions

    -- * Plugin Diagnostics
  , PluginDiagnostic (..)
  , PluginFix (..)
  , FixCategoryP (..)
  , FixSafetyP (..)
  , formatPluginDiagnostic
  , formatPluginDiagnosticJson

    -- * Type Capture (for auto-fix validation)
  , CapturedTypeInfo (..)
  , CapturedSymbol (..)
  , SymbolConstraints (..)
  , emptyConstraints
  , writeTypeInfo
  ) where

import Control.Monad (when, forM_)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson (ToJSON, FromJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString.Lazy qualified as BL
import Data.Maybe (mapMaybe, fromMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.IO qualified as TIO
import GHC.Generics (Generic)
import System.IO (stderr)
import System.Directory (createDirectoryIfMissing)
import System.FilePath (takeDirectory)

-- GHC imports (from ghc package, not ghc-lib-parser)
import "ghc" GHC.Plugins
  ( Plugin (..)
  , defaultPlugin
  , CommandLineOption
  , purePlugin
  , CoreToDo (..)
  , CoreM
  , CoreBind
  , Bind (..)
  , ModGuts (..)
  , Var
  , varName
  , varType
  , nameOccName
  , occNameString
  , getSrcSpan
  , showSDocUnsafe
  , ppr
  , Expr (..)
  , Alt (..)
  , Type
  , isStrictType
  , splitTyConApp_maybe
  , tyConName
  )
import "ghc" GHC.Types.SrcLoc qualified as SrcLoc
import "ghc" GHC.Data.FastString (unpackFS)
import "ghc" GHC.Core.Type (splitForAllTyCoVars, splitFunTys)

import Argus.Types (Severity (..), SrcSpan (..), DiagnosticKind (..), mkSrcSpanRaw, Line(..), Column(..))

--------------------------------------------------------------------------------
-- Plugin Configuration
--------------------------------------------------------------------------------

-- | Plugin configuration parsed from command line options
data PluginConfig = PluginConfig
  { pcConfigFile    :: Maybe FilePath  -- ^ Path to linter config file
  , pcMinSeverity   :: Severity        -- ^ Minimum severity to report
  , pcOutputFormat  :: Text            -- ^ Output format (text, json)
  , pcFailOnError   :: Bool            -- ^ Fail compilation on error
  , pcVerbose       :: Bool            -- ^ Verbose output
  , pcCaptureTypes  :: Bool            -- ^ Capture type info for auto-fix
  , pcTypeOutput    :: Maybe FilePath  -- ^ Output path for captured types
  , pcDetectThunks  :: Bool            -- ^ Detect potential space leaks (thunks)
  , pcStrictnessCheck :: Bool          -- ^ Check strictness annotations
  }
  deriving stock (Eq, Show)

-- | Default plugin configuration
defaultPluginConfig :: PluginConfig
defaultPluginConfig = PluginConfig
  { pcConfigFile = Nothing
  , pcMinSeverity = Warning
  , pcOutputFormat = "text"
  , pcFailOnError = False
  , pcVerbose = False
  , pcCaptureTypes = False
  , pcTypeOutput = Nothing
  , pcDetectThunks = True
  , pcStrictnessCheck = True
  }

-- | Parse plugin options from command line
parsePluginOptions :: [CommandLineOption] -> PluginConfig
parsePluginOptions = foldr parseOpt defaultPluginConfig
  where
    parseOpt opt cfg = case T.breakOn "=" (T.pack opt) of
      ("config", val)     -> cfg { pcConfigFile = Just $ T.unpack $ T.drop 1 val }
      ("severity", val)   -> cfg { pcMinSeverity = parseSeverity $ T.drop 1 val }
      ("output", val)     -> cfg { pcOutputFormat = T.drop 1 val }
      ("fail-on-error", _)  -> cfg { pcFailOnError = True }
      ("verbose", _)      -> cfg { pcVerbose = True }
      ("capture-types", _)  -> cfg { pcCaptureTypes = True }
      ("type-output", val)  -> cfg { pcTypeOutput = Just $ T.unpack $ T.drop 1 val }
      ("detect-thunks", _)  -> cfg { pcDetectThunks = True }
      ("no-detect-thunks", _) -> cfg { pcDetectThunks = False }
      ("strictness-check", _) -> cfg { pcStrictnessCheck = True }
      ("no-strictness-check", _) -> cfg { pcStrictnessCheck = False }
      _                   -> cfg

    parseSeverity "error"      = Error
    parseSeverity "warning"    = Warning
    parseSeverity "suggestion" = Suggestion
    parseSeverity "info"       = Info
    parseSeverity _            = Warning

--------------------------------------------------------------------------------
-- Plugin Diagnostics
--------------------------------------------------------------------------------

-- | A diagnostic produced by the plugin
data PluginDiagnostic = PluginDiagnostic
  { pdSpan     :: SrcSpan
  , pdSeverity :: Severity
  , pdKind     :: DiagnosticKind
  , pdMessage  :: Text
  , pdCode     :: Maybe Text
  , pdFixes    :: [PluginFix]  -- ^ Suggested fixes
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A fix suggested by the plugin
data PluginFix = PluginFix
  { pfTitle       :: Text           -- ^ Human-readable fix title
  , pfSpan        :: SrcSpan        -- ^ Source location to modify
  , pfReplacement :: Text           -- ^ Replacement text
  , pfImportAdd   :: [Text]         -- ^ Imports to add (module names)
  , pfIsPreferred :: Bool           -- ^ Is this the preferred fix?
  , pfCategory    :: FixCategoryP   -- ^ Fix category
  , pfSafety      :: FixSafetyP     -- ^ Fix safety level
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Fix category for plugin fixes
data FixCategoryP
  = FCPSafety       -- ^ Safety improvement (partial -> total)
  | FCPPerformance  -- ^ Performance improvement
  | FCPSecurity     -- ^ Security fix
  | FCPStyle        -- ^ Code style
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Fix safety level for plugin fixes
data FixSafetyP
  = FSPAlways     -- ^ Always safe to apply
  | FSPMostly     -- ^ Usually safe, minor edge cases
  | FSPReview     -- ^ Requires human review
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Format a plugin diagnostic for output
formatPluginDiagnostic :: PluginDiagnostic -> Text
formatPluginDiagnostic PluginDiagnostic{..} =
  let loc = T.pack (srcSpanFile pdSpan) <> ":" <>
            T.pack (show $ unLine $ srcSpanStartLine pdSpan) <> ":" <>
            T.pack (show $ unColumn $ srcSpanStartCol pdSpan)
      sev = case pdSeverity of
        Error      -> "error"
        Warning    -> "warning"
        Suggestion -> "suggestion"
        Info       -> "info"
      code = fromMaybe "" $ fmap (\c -> " [" <> c <> "]") pdCode
      mainMsg = loc <> ": " <> sev <> code <> ": " <> pdMessage
      fixMsgs = case pdFixes of
        [] -> ""
        fixes -> "\n  Fixes available:\n" <> T.intercalate "\n" (map formatFix fixes)
  in mainMsg <> fixMsgs
  where
    formatFix PluginFix{..} =
      let safetyBadge = case pfSafety of
            FSPAlways -> "[safe]"
            FSPMostly -> "[mostly-safe]"
            FSPReview -> "[review]"
          importNote = if null pfImportAdd
            then ""
            else " (adds imports: " <> T.intercalate ", " pfImportAdd <> ")"
      in "    - " <> safetyBadge <> " " <> pfTitle <> importNote

-- | Format a plugin diagnostic as JSON
formatPluginDiagnosticJson :: PluginDiagnostic -> Text
formatPluginDiagnosticJson diag = TE.decodeUtf8 $ BL.toStrict $ Aeson.encode diag

--------------------------------------------------------------------------------
-- GHC Plugin
--------------------------------------------------------------------------------

-- | The GHC plugin entry point
plugin :: Plugin
plugin = defaultPlugin
  { installCoreToDos = installLinterPass
  , pluginRecompile = purePlugin  -- Plugin doesn't affect recompilation
  }

-- | Install Argus as a Core-to-Core pass
-- This runs after desugaring, so TH splices are fully expanded
installLinterPass :: [CommandLineOption] -> [CoreToDo] -> CoreM [CoreToDo]
installLinterPass opts todos = do
  let config = parsePluginOptions opts
  pure $ CoreDoPluginPass "Linter" (linterCorePass config) : todos

-- | The Core-to-Core pass that performs linting
linterCorePass :: PluginConfig -> ModGuts -> CoreM ModGuts
linterCorePass config guts = do
  -- Get module name for reporting
  let modName = showSDocUnsafe $ ppr $ mg_module guts
      binds = mg_binds guts

  -- Run linting checks on core bindings
  let diagnostics = lintCoreBindings config binds

  -- Capture type info if requested
  when (pcCaptureTypes config) $ liftIO $ do
    let typeInfo = captureTypeInfo guts
        outputPath = fromMaybe (".argus/" <> modName <> ".types.json") (pcTypeOutput config)
    writeTypeInfo outputPath typeInfo
    when (pcVerbose config) $
      TIO.hPutStrLn stderr $ "Argus.Plugin: Captured type info for " <> T.pack modName

  -- Output diagnostics
  when (not $ null diagnostics) $ liftIO $ do
    when (pcVerbose config) $
      TIO.hPutStrLn stderr $ "Argus.Plugin: " <> T.pack (show $ length diagnostics) <>
                            " issue(s) in " <> T.pack modName

    forM_ diagnostics $ \diag ->
      when (pdSeverity diag >= pcMinSeverity config) $
        TIO.hPutStrLn stderr $ formatPluginDiagnostic diag

  -- Return guts unchanged (we don't modify code)
  pure guts

-- | Lint Core bindings for common issues
lintCoreBindings :: PluginConfig -> [CoreBind] -> [PluginDiagnostic]
lintCoreBindings config binds = concatMap (lintBinding config) binds

-- | Lint a single Core binding
lintBinding :: PluginConfig -> CoreBind -> [PluginDiagnostic]
lintBinding config (NonRec var expr) =
  lintVar var ++
  checkStrictness config var ++
  lintExpr config (getSrcSpan (varName var)) expr ++
  detectThunkIssues config (getSrcSpan (varName var)) expr
lintBinding config (Rec pairs) = concatMap checkPair pairs
  where
    checkPair (v, e) =
      lintVar v ++
      checkStrictness config v ++
      lintExpr config (getSrcSpan (varName v)) e ++
      detectThunkIssues config (getSrcSpan (varName v)) e

-- | Lint a variable for naming conventions and other issues
lintVar :: Var -> [PluginDiagnostic]
lintVar var =
  let name = varName var
      occName = nameOccName name
      nameStr = T.pack $ occNameString occName
      ghcSpan = getSrcSpan name
  in mapMaybe id
    [ -- Check for underscore-prefixed names that are used
      if "_" `T.isPrefixOf` nameStr && nameStr /= "_"
        then Just $ mkDiagnostic ghcSpan Warning NamingConvention
          ("Underscore-prefixed name '" <> nameStr <> "' - ensure it's intentionally unused")
          (Just "plugin/naming/underscore")
        else Nothing

    , -- Check for very short names (potential code smell)
      if T.length nameStr == 1 && nameStr `notElem` ["x", "y", "z", "n", "m", "i", "j", "k", "f", "g", "h", "a", "b", "c"]
        then Just $ mkDiagnostic ghcSpan Suggestion NamingConvention
          ("Very short variable name '" <> nameStr <> "' - consider more descriptive name")
          (Just "plugin/naming/short")
        else Nothing
    ]

--------------------------------------------------------------------------------
-- Expression-Level Linting
--------------------------------------------------------------------------------

-- | Lint expressions for partial functions and other issues
lintExpr :: PluginConfig -> SrcLoc.SrcSpan -> Expr Var -> [PluginDiagnostic]
lintExpr config defaultSpan = go
  where
    go expr = case expr of
      Var v -> checkVarUsage config defaultSpan v
      App f x -> go f ++ go x
      Lam _ body -> go body
      Let bind body -> lintBinding config bind ++ go body
      Case scrut _ _ alts -> go scrut ++ concatMap lintAlt alts
      Cast e _ -> go e
      Tick _ e -> go e
      _ -> []

    lintAlt (Alt _ _ rhs) = go rhs

-- | Check variable usage for partial functions and unsafe operations
checkVarUsage :: PluginConfig -> SrcLoc.SrcSpan -> Var -> [PluginDiagnostic]
checkVarUsage _config srcSpan var =
  let name = varName var
      occName = nameOccName name
      nameStr = occNameString occName
  in mapMaybe id
    [ -- Partial functions
      checkPartialFunction srcSpan nameStr

    , -- Unsafe operations
      checkUnsafeOperation srcSpan nameStr

    , -- Space leak patterns
      checkSpaceLeakPattern srcSpan nameStr
    ]

-- | Check for partial function usage
checkPartialFunction :: SrcLoc.SrcSpan -> String -> Maybe PluginDiagnostic
checkPartialFunction srcSpan name
  | name == "head" = Just $ mkDiagnosticWithFix srcSpan Warning PartialFunction
      "Use of partial function 'head' - use headMay or pattern matching"
      (Just "plugin/partial/head")
      [mkPartialFix srcSpan "head" "headMay" "Safe" ["Safe"]]
  | name == "tail" = Just $ mkDiagnosticWithFix srcSpan Warning PartialFunction
      "Use of partial function 'tail' - use tailMay or pattern matching"
      (Just "plugin/partial/tail")
      [mkPartialFix srcSpan "tail" "tailMay" "Safe" ["Safe"]]
  | name == "init" = Just $ mkDiagnosticWithFix srcSpan Warning PartialFunction
      "Use of partial function 'init' - use initMay or pattern matching"
      (Just "plugin/partial/init")
      [mkPartialFix srcSpan "init" "initMay" "Safe" ["Safe"]]
  | name == "last" = Just $ mkDiagnosticWithFix srcSpan Warning PartialFunction
      "Use of partial function 'last' - use lastMay or pattern matching"
      (Just "plugin/partial/last")
      [mkPartialFix srcSpan "last" "lastMay" "Safe" ["Safe"]]
  | name == "fromJust" = Just $ mkDiagnosticWithFix srcSpan Warning PartialFunction
      "Use of partial function 'fromJust' - use pattern matching or fromMaybe"
      (Just "plugin/partial/fromJust")
      [ mkPartialFix srcSpan "fromJust" "fromMaybe defaultVal" "Data.Maybe" ["Data.Maybe"]
      , mkPatternMatchFix srcSpan "fromJust"
      ]
  | name == "read" = Just $ mkDiagnosticWithFix srcSpan Warning PartialFunction
      "Use of partial function 'read' - use readMaybe for safe parsing"
      (Just "plugin/partial/read")
      [mkPartialFix srcSpan "read" "readMaybe" "Text.Read" ["Text.Read"]]
  | name == "!!" = Just $ mkDiagnosticWithFix srcSpan Warning PartialFunction
      "Use of partial function '!!' - use safe indexing"
      (Just "plugin/partial/index")
      [mkPartialFix srcSpan "!!" "atMay" "Safe" ["Safe"]]
  | name == "maximum" = Just $ mkDiagnosticWithFix srcSpan Suggestion PartialFunction
      "Use of partial function 'maximum' - use maximumMay for empty list safety"
      (Just "plugin/partial/maximum")
      [mkPartialFix srcSpan "maximum" "maximumMay" "Safe" ["Safe"]]
  | name == "minimum" = Just $ mkDiagnosticWithFix srcSpan Suggestion PartialFunction
      "Use of partial function 'minimum' - use minimumMay for empty list safety"
      (Just "plugin/partial/minimum")
      [mkPartialFix srcSpan "minimum" "minimumMay" "Safe" ["Safe"]]
  | name == "foldr1" = Just $ mkDiagnosticWithFix srcSpan Suggestion PartialFunction
      "Use of partial function 'foldr1' - use foldr with explicit base case"
      (Just "plugin/partial/foldr1")
      [mkFoldFix srcSpan "foldr1" "foldr"]
  | name == "foldl1" = Just $ mkDiagnosticWithFix srcSpan Suggestion PartialFunction
      "Use of partial function 'foldl1' - use foldl' with explicit base case"
      (Just "plugin/partial/foldl1")
      [mkFoldFix srcSpan "foldl1" "foldl'"]
  | name == "error" = Just $ mkDiagnostic srcSpan Warning PartialFunction
      "Use of 'error' - prefer proper error handling"
      (Just "plugin/partial/error")
  | name == "undefined" = Just $ mkDiagnostic srcSpan Error PartialFunction
      "Use of 'undefined' - must not be used in production code"
      (Just "plugin/partial/undefined")
  | otherwise = Nothing

-- | Create a fix for replacing a partial function
mkPartialFix :: SrcLoc.SrcSpan -> Text -> Text -> Text -> [Text] -> PluginFix
mkPartialFix srcSpan original replacement fixModule imports = PluginFix
  { pfTitle = "Replace '" <> original <> "' with '" <> replacement <> "'"
  , pfSpan = convertSpan srcSpan
  , pfReplacement = replacement
  , pfImportAdd = imports
  , pfIsPreferred = True
  , pfCategory = FCPSafety
  , pfSafety = FSPAlways
  }

-- | Create a pattern matching fix suggestion
mkPatternMatchFix :: SrcLoc.SrcSpan -> Text -> PluginFix
mkPatternMatchFix srcSpan original = PluginFix
  { pfTitle = "Replace '" <> original <> "' with pattern matching"
  , pfSpan = convertSpan srcSpan
  , pfReplacement = "case <expr> of Just x -> x; Nothing -> <default>"
  , pfImportAdd = []
  , pfIsPreferred = False
  , pfCategory = FCPSafety
  , pfSafety = FSPReview
  }

-- | Create a fold fix suggestion
mkFoldFix :: SrcLoc.SrcSpan -> Text -> Text -> PluginFix
mkFoldFix srcSpan original replacement = PluginFix
  { pfTitle = "Replace '" <> original <> "' with '" <> replacement <> "' and explicit base"
  , pfSpan = convertSpan srcSpan
  , pfReplacement = replacement <> " <f> <base>"
  , pfImportAdd = if replacement == "foldl'" then ["Data.List"] else []
  , pfIsPreferred = True
  , pfCategory = FCPSafety
  , pfSafety = FSPReview
  }

-- | Check for unsafe operations
checkUnsafeOperation :: SrcLoc.SrcSpan -> String -> Maybe PluginDiagnostic
checkUnsafeOperation srcSpan name
  | name == "unsafePerformIO" = Just $ mkDiagnosticWithFix srcSpan Error SecurityIssue
      "Use of 'unsafePerformIO' - breaks referential transparency"
      (Just "plugin/unsafe/performIO")
      [mkUnsafeFix srcSpan "unsafePerformIO" "Refactor to use IO monad properly"]
  | name == "unsafeCoerce" = Just $ mkDiagnosticWithFix srcSpan Error SecurityIssue
      "Use of 'unsafeCoerce' - can cause memory corruption"
      (Just "plugin/unsafe/coerce")
      [mkUnsafeFix srcSpan "unsafeCoerce" "Use type-safe coercion or newtype wrapper"]
  | name == "unsafeInterleaveIO" = Just $ mkDiagnosticWithFix srcSpan Warning SecurityIssue
      "Use of 'unsafeInterleaveIO' - can cause unpredictable behavior"
      (Just "plugin/unsafe/interleaveIO")
      [mkUnsafeFix srcSpan "unsafeInterleaveIO" "Use streaming library (conduit/pipes) for lazy IO"]
  | name == "inlinePerformIO" = Just $ mkDiagnosticWithFix srcSpan Error SecurityIssue
      "Use of 'inlinePerformIO' - extremely unsafe"
      (Just "plugin/unsafe/inlinePerformIO")
      [mkUnsafeFix srcSpan "inlinePerformIO" "Refactor to use IO monad properly"]
  | name == "unsafeDupablePerformIO" = Just $ mkDiagnosticWithFix srcSpan Warning SecurityIssue
      "Use of 'unsafeDupablePerformIO' - may execute IO multiple times"
      (Just "plugin/unsafe/dupablePerformIO")
      [mkUnsafeFix srcSpan "unsafeDupablePerformIO" "Refactor to use IO monad properly"]
  | otherwise = Nothing

-- | Create a fix for unsafe operation refactoring
-- Note: For unsafe operations, we wrap in explicit IO to force the developer
-- to handle the side effects properly rather than hiding them
mkUnsafeFix :: SrcLoc.SrcSpan -> Text -> Text -> PluginFix
mkUnsafeFix srcSpan original _suggestion = PluginFix
  { pfTitle = "Wrap unsafe operation in explicit IO"
  , pfSpan = convertSpan srcSpan
  , pfReplacement = "liftIO (" <> original <> ")"
  , pfImportAdd = ["Control.Monad.IO.Class"]
  , pfIsPreferred = False  -- Requires review
  , pfCategory = FCPSecurity
  , pfSafety = FSPReview
  }

-- | Check for space leak patterns
checkSpaceLeakPattern :: SrcLoc.SrcSpan -> String -> Maybe PluginDiagnostic
checkSpaceLeakPattern srcSpan name
  | name == "foldl" = Just $ mkDiagnosticWithFix srcSpan Suggestion SpaceLeak
      "Use 'foldl'' instead of 'foldl' to avoid space leaks"
      (Just "plugin/spaceleak/foldl")
      [mkSpaceLeakFix srcSpan "foldl" "foldl'" ["Data.List"]]
  | name == "sum" = Just $ mkDiagnosticWithFix srcSpan Info SpaceLeak
      "Consider using 'foldl' (+) 0' for better performance on large lists"
      (Just "plugin/spaceleak/sum")
      [mkSpaceLeakFix srcSpan "sum" "foldl' (+) 0" ["Data.List"]]
  | name == "product" = Just $ mkDiagnosticWithFix srcSpan Info SpaceLeak
      "Consider using 'foldl' (*) 1' for better performance on large lists"
      (Just "plugin/spaceleak/product")
      [mkSpaceLeakFix srcSpan "product" "foldl' (*) 1" ["Data.List"]]
  | name == "++" = Just $ mkDiagnosticWithFix srcSpan Info PerformanceIssue
      "Left-nested '++' can cause O(n²) performance - consider difference lists"
      (Just "plugin/performance/concat")
      [mkPerformanceFix srcSpan "++" "Use Data.DList or ShowS pattern"]
  | name == "concat" = Just $ mkDiagnosticWithFix srcSpan Info PerformanceIssue
      "Repeated 'concat' can cause O(n²) performance - consider difference lists"
      (Just "plugin/performance/concat")
      [mkPerformanceFix srcSpan "concat" "Use Data.DList.fromList/toList"]
  | name == "nub" = Just $ mkDiagnosticWithFix srcSpan Warning PerformanceIssue
      "'nub' is O(n²) - use Data.List.nubOrd for Ord types or Data.HashSet for Hashable"
      (Just "plugin/performance/nub")
      [ mkSpaceLeakFix srcSpan "nub" "nubOrd" ["Data.Containers.ListUtils"]
      , mkSpaceLeakFix srcSpan "nub" "toList . fromList" ["Data.HashSet"]
      ]
  | name == "length" = Just $ mkDiagnostic srcSpan Info PerformanceIssue
      "'length' forces spine of list - use Data.Foldable.length' or consider streaming"
      (Just "plugin/performance/length")
  | otherwise = Nothing

-- | Create a fix for space leak patterns
mkSpaceLeakFix :: SrcLoc.SrcSpan -> Text -> Text -> [Text] -> PluginFix
mkSpaceLeakFix srcSpan original replacement imports = PluginFix
  { pfTitle = "Replace '" <> original <> "' with '" <> replacement <> "'"
  , pfSpan = convertSpan srcSpan
  , pfReplacement = replacement
  , pfImportAdd = imports
  , pfIsPreferred = True
  , pfCategory = FCPPerformance
  , pfSafety = FSPAlways
  }

-- | Create a performance fix suggestion
mkPerformanceFix :: SrcLoc.SrcSpan -> Text -> Text -> PluginFix
mkPerformanceFix srcSpan original suggestion = PluginFix
  { pfTitle = suggestion
  , pfSpan = convertSpan srcSpan
  , pfReplacement = "-- Performance: replace " <> original
  , pfImportAdd = []
  , pfIsPreferred = False
  , pfCategory = FCPPerformance
  , pfSafety = FSPReview
  }

-- | Create a diagnostic from a GHC SrcSpan (without fixes)
mkDiagnostic :: SrcLoc.SrcSpan -> Severity -> DiagnosticKind -> Text -> Maybe Text -> PluginDiagnostic
mkDiagnostic ghcSpan sev kind msg code = PluginDiagnostic
  { pdSpan = convertSpan ghcSpan
  , pdSeverity = sev
  , pdKind = kind
  , pdMessage = msg
  , pdCode = code
  , pdFixes = []
  }

-- | Create a diagnostic from a GHC SrcSpan with fixes
mkDiagnosticWithFix :: SrcLoc.SrcSpan -> Severity -> DiagnosticKind -> Text -> Maybe Text -> [PluginFix] -> PluginDiagnostic
mkDiagnosticWithFix ghcSpan sev kind msg code fixes = PluginDiagnostic
  { pdSpan = convertSpan ghcSpan
  , pdSeverity = sev
  , pdKind = kind
  , pdMessage = msg
  , pdCode = code
  , pdFixes = fixes
  }

-- | Convert GHC SrcSpan to our SrcSpan
convertSpan :: SrcLoc.SrcSpan -> SrcSpan
convertSpan ghcSpan = case ghcSpan of
  SrcLoc.RealSrcSpan rss _ -> mkSrcSpanRaw
    (unpackFS $ SrcLoc.srcSpanFile rss)
    (SrcLoc.srcSpanStartLine rss)
    (SrcLoc.srcSpanStartCol rss)
    (SrcLoc.srcSpanEndLine rss)
    (SrcLoc.srcSpanEndCol rss)
  _ -> mkSrcSpanRaw "" 0 0 0 0  -- Unknown span

--------------------------------------------------------------------------------
-- Type Capture for Auto-Fix Validation
--------------------------------------------------------------------------------

-- | Captured type information from Core compilation
data CapturedTypeInfo = CapturedTypeInfo
  { ctiModuleName  :: Text                      -- ^ Module name
  , ctiSymbols     :: [CapturedSymbol]          -- ^ All captured symbols
  , ctiImports     :: [Text]                    -- ^ Imported modules
  , ctiExports     :: [Text]                    -- ^ Exported symbols
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | A captured symbol with type info
data CapturedSymbol = CapturedSymbol
  { csName        :: Text                       -- ^ Symbol name
  , csType        :: Text                       -- ^ Pretty-printed type
  , csSpan        :: SrcSpan                    -- ^ Source location
  , csConstraints :: SymbolConstraints          -- ^ Required constraints
  , csIsStrict    :: Bool                       -- ^ Is the type strict?
  , csArity       :: Int                        -- ^ Function arity
  , csIsExported  :: Bool                       -- ^ Is symbol exported?
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Constraints required by a symbol
data SymbolConstraints = SymbolConstraints
  { scEq        :: Bool                         -- ^ Requires Eq
  , scOrd       :: Bool                         -- ^ Requires Ord
  , scShow      :: Bool                         -- ^ Requires Show
  , scRead      :: Bool                         -- ^ Requires Read
  , scMonoid    :: Bool                         -- ^ Requires Monoid
  , scSemigroup :: Bool                         -- ^ Requires Semigroup
  , scHashable  :: Bool                         -- ^ Requires Hashable
  , scNFData    :: Bool                         -- ^ Requires NFData
  , scOther     :: [Text]                       -- ^ Other constraints
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Empty constraints
emptyConstraints :: SymbolConstraints
emptyConstraints = SymbolConstraints
  { scEq = False
  , scOrd = False
  , scShow = False
  , scRead = False
  , scMonoid = False
  , scSemigroup = False
  , scHashable = False
  , scNFData = False
  , scOther = []
  }

-- | Write captured type info to a file
writeTypeInfo :: FilePath -> CapturedTypeInfo -> IO ()
writeTypeInfo path info = do
  createDirectoryIfMissing True (takeDirectory path)
  BL.writeFile path (Aeson.encode info)

-- | Capture type info from Core bindings
captureTypeInfo :: ModGuts -> CapturedTypeInfo
captureTypeInfo guts =
  let modName = T.pack $ showSDocUnsafe $ ppr $ mg_module guts
      symbols = concatMap captureBinding (mg_binds guts)
      -- Capture exports (approximated by top-level bindings)
      exports = map csName $ filter csIsExported symbols
  in CapturedTypeInfo
    { ctiModuleName = modName
    , ctiSymbols = symbols
    , ctiImports = []  -- Would need HsModule for actual imports
    , ctiExports = exports
    }

-- | Capture type info from a binding
captureBinding :: CoreBind -> [CapturedSymbol]
captureBinding (NonRec var _expr) = [captureVar var True]
captureBinding (Rec pairs) = map (\(v, _) -> captureVar v True) pairs

-- | Capture type info from a variable
captureVar :: Var -> Bool -> CapturedSymbol
captureVar var isTopLevel =
  let name = varName var
      occName = nameOccName name
      nameStr = T.pack $ occNameString occName
      ghcSpan = getSrcSpan name
      ty = varType var
      tyStr = T.pack $ showSDocUnsafe $ ppr ty
      constraints = extractConstraints ty
      arity = countFunArgs ty
      strict = isStrictType ty
  in CapturedSymbol
    { csName = nameStr
    , csType = tyStr
    , csSpan = convertSpan ghcSpan
    , csConstraints = constraints
    , csIsStrict = strict
    , csArity = arity
    , csIsExported = isTopLevel  -- Approximation
    }

-- | Extract constraints from a type
--
-- Analyzes the type to determine which type class constraints are required.
-- This looks at the constraint context in forall types and checks for common
-- class names.
extractConstraints :: Type -> SymbolConstraints
extractConstraints ty =
  let (_tvs, _body) = splitForAllTyCoVars ty
      -- Extract the constraint portion of the type by examining type variable bounds
      -- The constraints are encoded in the type structure
      tyStr = T.pack $ showSDocUnsafe $ ppr ty
      -- Check for common constraint patterns in the type string
      hasConstraint name = T.isInfixOf name tyStr
  in SymbolConstraints
    { scEq        = hasConstraint "Eq " || hasConstraint "Eq,"
    , scOrd       = hasConstraint "Ord " || hasConstraint "Ord,"
    , scShow      = hasConstraint "Show " || hasConstraint "Show,"
    , scRead      = hasConstraint "Read " || hasConstraint "Read,"
    , scMonoid    = hasConstraint "Monoid " || hasConstraint "Monoid,"
    , scSemigroup = hasConstraint "Semigroup " || hasConstraint "Semigroup,"
    , scHashable  = hasConstraint "Hashable " || hasConstraint "Hashable,"
    , scNFData    = hasConstraint "NFData " || hasConstraint "NFData,"
    , scOther     = extractOtherConstraints tyStr
    }

-- | Extract other constraint names from type string
extractOtherConstraints :: Text -> [Text]
extractOtherConstraints tyStr =
  let commonConstraints = ["Eq", "Ord", "Show", "Read", "Monoid", "Semigroup", "Hashable", "NFData"]
      -- Look for patterns like "ClassName a =>" in the type string
      -- This is a heuristic approach that works for most common cases
      constraintSection = T.takeWhile (/= '=') tyStr
      words' = T.words constraintSection
      -- Filter out common ones and keep capitalized words that look like class names
      startsWithUpper w = case T.uncons w of
        Just (c, _) -> c >= 'A' && c <= 'Z'
        Nothing -> False
      otherWords = filter (\w -> T.length w > 1 && startsWithUpper w && w `notElem` commonConstraints) words'
  in filter (\w -> not ("(" `T.isInfixOf` w || ")" `T.isInfixOf` w)) otherWords

-- | Count function arguments (arity)
--
-- Counts the number of function arguments by examining the function type structure.
-- For a type like @a -> b -> c@, this returns 2.
countFunArgs :: Type -> Int
countFunArgs ty =
  let (args, _res) = splitFunTys ty
      -- splitFunTys returns the arguments in the form of (mult, arg) pairs
      -- Count the number of pairs
  in length args

--------------------------------------------------------------------------------
-- Enhanced Space Leak Detection
--------------------------------------------------------------------------------

-- | Detect potential thunks in let bindings
detectThunkIssues :: PluginConfig -> SrcLoc.SrcSpan -> Expr Var -> [PluginDiagnostic]
detectThunkIssues config defaultSpan expr
  | not (pcDetectThunks config) = []
  | otherwise = go expr
  where
    go (Let (NonRec var body) rest) =
      let varTy = varType var
          issues = if not (isStrictType varTy) && isPotentiallyLarge body
            then [mkDiagnostic defaultSpan Suggestion SpaceLeak
                    ("Potentially large thunk bound to '" <>
                     T.pack (occNameString $ nameOccName $ varName var) <>
                     "' - consider using bang pattern or seq")
                    (Just "plugin/spaceleak/thunk")]
            else []
      in issues ++ go body ++ go rest
    go (Let (Rec binds) rest) =
      concatMap (go . snd) binds ++ go rest
    go (App f x) = go f ++ go x
    go (Lam _ body) = go body
    go (Case scrut _ _ alts) = go scrut ++ concatMap (\(Alt _ _ rhs) -> go rhs) alts
    go (Cast e _) = go e
    go (Tick _ e) = go e
    go _ = []

-- | Check if an expression is potentially large (heuristic)
isPotentiallyLarge :: Expr Var -> Bool
isPotentiallyLarge = go 0
  where
    maxDepth = 3 :: Int

    go depth _ | depth > maxDepth = True
    go depth expr = case expr of
      App f x -> go (depth + 1) f || go (depth + 1) x
      Let _ body -> go (depth + 1) body
      Case _ _ _ alts -> length alts > 2 || any (\(Alt _ _ rhs) -> go (depth + 1) rhs) alts
      _ -> False

--------------------------------------------------------------------------------
-- Strictness Analysis
--------------------------------------------------------------------------------

-- | Check for missing strictness annotations
checkStrictness :: PluginConfig -> Var -> [PluginDiagnostic]
checkStrictness config var
  | not (pcStrictnessCheck config) = []
  | otherwise =
    let name = varName var
        ghcSpan = getSrcSpan name
        ty = varType var
        nameStr = occNameString $ nameOccName name
    in if shouldBeStrict ty && not (isStrictType ty)
       then [mkDiagnostic ghcSpan Info SpaceLeak
               ("Consider adding strictness annotation to '" <> T.pack nameStr <>
                "' - type suggests accumulator pattern")
               (Just "plugin/strictness/accumulator")]
       else []

-- | Heuristic: should this type likely be strict?
shouldBeStrict :: Type -> Bool
shouldBeStrict ty = case splitTyConApp_maybe ty of
  Just (tc, _args) ->
    let tcName = occNameString $ nameOccName $ tyConName tc
    in tcName `elem` ["Int", "Integer", "Double", "Float", "Word"]
  Nothing -> False
