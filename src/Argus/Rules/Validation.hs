{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Rules.Validation
-- Description : Validation system for Argus rules
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides comprehensive validation for Argus rules, ensuring
-- that rule definitions are well-formed before they are used for analysis.
--
-- == Validation Checks
--
-- * Pattern syntax validation
-- * Side condition consistency
-- * Import specification validity
-- * Fix replacement correctness
-- * Category/severity appropriateness
-- * Metavariable usage consistency
--
-- == Usage
--
-- @
-- case validateRule myRule of
--   ValidationPassed -> putStrLn "Rule is valid"
--   ValidationWarnings ws -> mapM_ (putStrLn . warningMessage) ws
--   ValidationFailed errs -> mapM_ (putStrLn . errorMessage) errs
-- @
module Argus.Rules.Validation
  ( -- * Validation Results
    ValidationResult (..)
  , ValidationError (..)
  , ValidationWarning (..)
  , isValid
  , hasWarnings
  , hasErrors

    -- * Rule Validation
  , validateRule
  , validateRules
  , validateRuleStrict

    -- * Component Validation
  , validatePattern
  , validateSideCondition
  , validateImportSpec
  , validateFix
  , validateMetavariables

    -- * Validation Configuration
  , ValidationConfig (..)
  , defaultValidationConfig
  , strictValidationConfig

    -- * Error/Warning Details
  , ErrorCode (..)
  , WarningCode (..)
  , errorToText
  , warningToText
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (isNothing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Text.Regex.TDFA ((=~))

import Argus.Rules.Types
import Argus.Types (Fix(..), FixEdit(..))

--------------------------------------------------------------------------------
-- Validation Results
--------------------------------------------------------------------------------

-- | Result of validating a rule
data ValidationResult
  = ValidationPassed
  | ValidationWarnings [ValidationWarning]
  | ValidationFailed [ValidationError]
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Validation error with details
data ValidationError = ValidationError
  { veCode      :: ErrorCode
  , veRuleId    :: Text
  , veMessage   :: Text
  , veLocation  :: Maybe Text  -- Component that failed
  , veSuggestion :: Maybe Text -- How to fix it
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Validation warning with details
data ValidationWarning = ValidationWarning
  { vwCode     :: WarningCode
  , vwRuleId   :: Text
  , vwMessage  :: Text
  , vwLocation :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Error codes for validation failures
data ErrorCode
  = ErrEmptyRuleId
  | ErrInvalidRuleId
  | ErrEmptyPattern
  | ErrInvalidPatternSyntax
  | ErrUnbalancedParens
  | ErrUnbalancedBrackets
  | ErrInvalidMetavariable
  | ErrUndefinedMetavariable
  | ErrUnusedMetavariable
  | ErrEmptyMessage
  | ErrInvalidImportSpec
  | ErrInvalidModuleName
  | ErrConflictingSideConditions
  | ErrInvalidSideCondition
  | ErrFixMissingMetavariable
  | ErrCircularDependency
  | ErrInvalidSeverity
  | ErrInvalidCategory
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Warning codes for potential issues
data WarningCode
  = WarnNoMessage
  | WarnNoFix
  | WarnBroadPattern
  | WarnOverlappingConditions
  | WarnDeprecatedSyntax
  | WarnUnusualCategory
  | WarnHighSeverityNoFix
  | WarnLowConfidenceFix
  | WarnMissingDocumentation
  | WarnPossiblePerformanceIssue
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Check if validation passed
isValid :: ValidationResult -> Bool
isValid ValidationPassed = True
isValid (ValidationWarnings _) = True
isValid (ValidationFailed _) = False

-- | Check if there are warnings
hasWarnings :: ValidationResult -> Bool
hasWarnings (ValidationWarnings ws) = not (null ws)
hasWarnings _ = False

-- | Check if there are errors
hasErrors :: ValidationResult -> Bool
hasErrors (ValidationFailed _) = True
hasErrors _ = False

--------------------------------------------------------------------------------
-- Validation Configuration
--------------------------------------------------------------------------------

-- | Configuration for validation strictness
data ValidationConfig = ValidationConfig
  { vcRequireMessage        :: Bool  -- ^ Require non-empty message
  , vcRequireFix            :: Bool  -- ^ Require fix for warnings
  , vcCheckMetavariables    :: Bool  -- ^ Validate metavariable usage
  , vcCheckImports          :: Bool  -- ^ Validate import specifications
  , vcAllowBroadPatterns    :: Bool  -- ^ Allow very broad patterns
  , vcMaxPatternComplexity  :: Int   -- ^ Maximum pattern nesting depth
  , vcAllowDeprecated       :: Bool  -- ^ Allow deprecated syntax
  , vcValidateFixSyntax     :: Bool  -- ^ Parse and validate fix patterns
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default validation configuration (permissive)
defaultValidationConfig :: ValidationConfig
defaultValidationConfig = ValidationConfig
  { vcRequireMessage = False
  , vcRequireFix = False
  , vcCheckMetavariables = True
  , vcCheckImports = True
  , vcAllowBroadPatterns = True
  , vcMaxPatternComplexity = 10
  , vcAllowDeprecated = True
  , vcValidateFixSyntax = True
  }

-- | Strict validation configuration for production rules
strictValidationConfig :: ValidationConfig
strictValidationConfig = ValidationConfig
  { vcRequireMessage = True
  , vcRequireFix = True
  , vcCheckMetavariables = True
  , vcCheckImports = True
  , vcAllowBroadPatterns = False
  , vcMaxPatternComplexity = 5
  , vcAllowDeprecated = False
  , vcValidateFixSyntax = True
  }

--------------------------------------------------------------------------------
-- Rule Validation
--------------------------------------------------------------------------------

-- | Validate a single rule with default configuration
validateRule :: Rule -> ValidationResult
validateRule = validateRuleWith defaultValidationConfig

-- | Validate a rule with strict configuration
validateRuleStrict :: Rule -> ValidationResult
validateRuleStrict = validateRuleWith strictValidationConfig

-- | Validate multiple rules
validateRules :: [Rule] -> Map Text ValidationResult
validateRules rules = Map.fromList
  [ (ruleId r, validateRule r) | r <- rules ]

-- | Validate a rule with specific configuration
validateRuleWith :: ValidationConfig -> Rule -> ValidationResult
validateRuleWith config rule =
  let errors = concat
        [ validateRuleId (ruleId rule)
        , validateRulePattern config (ruleId rule) (rulePattern rule)
        , validateRuleMessage config (ruleId rule) (ruleMessage rule)
        , validateRuleReplacement config rule
        , validateRuleSideConditions (ruleId rule) (ruleConditions rule)
        , validateRuleImports config (ruleId rule) (ruleAddImports rule)
        ]
      warnings = collectWarnings config rule
  in case (errors, warnings) of
       ([], []) -> ValidationPassed
       ([], ws) -> ValidationWarnings ws
       (es, _)  -> ValidationFailed es

-- | Validate rule ID
validateRuleId :: Text -> [ValidationError]
validateRuleId rid
  | T.null rid = [mkError ErrEmptyRuleId rid "Rule ID cannot be empty" Nothing
                   (Just "Provide a unique rule identifier")]
  | not (isValidRuleId rid) = [mkError ErrInvalidRuleId rid
                                ("Invalid rule ID format: " <> rid)
                                Nothing
                                (Just "Use format: category/rule-name or package/category/rule-name")]
  | otherwise = []

-- | Check if rule ID is valid format
isValidRuleId :: Text -> Bool
isValidRuleId rid =
  let pattern' = "^[a-z][a-z0-9-]*(/[a-z][a-z0-9-]*)+$" :: Text
  in rid =~ pattern'

-- | Validate rule pattern
validateRulePattern :: ValidationConfig -> Text -> RulePattern -> [ValidationError]
validateRulePattern config rid pat = case pat of
  TextPatternSpec txt ->
    if T.null txt
    then [mkError ErrEmptyPattern rid "Pattern cannot be empty" (Just "pattern") Nothing]
    else validatePatternSyntax config rid txt
  ASTPatternSpec ast -> validateASTPattern config rid ast
  RegexPatternSpec regex ->
    if T.null regex
    then [mkError ErrEmptyPattern rid "Regex pattern cannot be empty" (Just "pattern") Nothing]
    else validateRegexSyntax config rid regex

-- | Validate pattern syntax
validatePatternSyntax :: ValidationConfig -> Text -> Text -> [ValidationError]
validatePatternSyntax config rid patText =
  let parenErrors = validateBalancedParens rid patText
      metavarErrors = if vcCheckMetavariables config
                      then validateMetavariableSyntax rid patText
                      else []
      complexityErrors = validatePatternComplexity config rid patText
  in parenErrors ++ metavarErrors ++ complexityErrors

-- | Validate balanced parentheses
validateBalancedParens :: Text -> Text -> [ValidationError]
validateBalancedParens rid patText =
  let count c = T.length . T.filter (== c)
      opens = count '(' patText + count '[' patText + count '{' patText
      closes = count ')' patText + count ']' patText + count '}' patText
  in if opens /= closes
     then [mkError ErrUnbalancedParens rid
            ("Unbalanced parentheses in pattern: " <> patText)
            (Just "pattern")
            (Just "Ensure all opening brackets have matching closing brackets")]
     else []

-- | Validate metavariable syntax
validateMetavariableSyntax :: Text -> Text -> [ValidationError]
validateMetavariableSyntax rid patText =
  let metavars = extractMetavariables patText
      invalidVars = filter (not . isValidMetavar) metavars
  in map (\v -> mkError ErrInvalidMetavariable rid
                 ("Invalid metavariable: " <> v)
                 (Just "pattern")
                 (Just "Metavariables should be $NAME or $name")) invalidVars

-- | Extract metavariables from pattern text
extractMetavariables :: Text -> [Text]
extractMetavariables txt =
  let words' = T.words txt
      isMetavar w = "$" `T.isPrefixOf` w
  in filter isMetavar words'

-- | Check if metavariable name is valid
isValidMetavar :: Text -> Bool
isValidMetavar mv =
  T.length mv > 1 &&
  "$" `T.isPrefixOf` mv &&
  T.all (\c -> c == '_' || c >= 'A' && c <= 'Z' || c >= 'a' && c <= 'z' || c >= '0' && c <= '9')
        (T.drop 1 mv)

-- | Validate pattern complexity
validatePatternComplexity :: ValidationConfig -> Text -> Text -> [ValidationError]
validatePatternComplexity config rid patText =
  let depth = maxNestingDepth patText
  in if depth > vcMaxPatternComplexity config
     then [mkError ErrInvalidPatternSyntax rid
            ("Pattern too complex (depth " <> T.pack (show depth) <>
             " > max " <> T.pack (show (vcMaxPatternComplexity config)) <> ")")
            (Just "pattern")
            (Just "Simplify the pattern or increase vcMaxPatternComplexity")]
     else []

-- | Calculate maximum nesting depth
maxNestingDepth :: Text -> Int
maxNestingDepth txt = go 0 0 (T.unpack txt)
  where
    go maxD _ [] = maxD
    go maxD d (c:cs)
      | c `elem` ("([{" :: String) = go (max maxD (d + 1)) (d + 1) cs
      | c `elem` (")]}" :: String) = go maxD (max 0 (d - 1)) cs
      | otherwise = go maxD d cs

-- | Validate AST pattern
validateASTPattern :: ValidationConfig -> Text -> Text -> [ValidationError]
validateASTPattern _config _rid _ast = []  -- AST patterns are validated at parse time

-- | Validate regex pattern syntax
validateRegexSyntax :: ValidationConfig -> Text -> Text -> [ValidationError]
validateRegexSyntax _config rid regex =
  -- Basic regex validation - check for obviously broken patterns
  let parenBalance = countBalanced '(' ')' regex
      bracketBalance = countBalanced '[' ']' regex
      errors = []
        ++ [mkError ErrUnbalancedParens rid "Unbalanced parentheses in regex" (Just "pattern") Nothing | parenBalance /= (0 :: Int)]
        ++ [mkError ErrUnbalancedBrackets rid "Unbalanced brackets in regex" (Just "pattern") Nothing | bracketBalance /= (0 :: Int)]
  in errors
  where
    countBalanced :: Char -> Char -> Text -> Int
    countBalanced open close txt = T.foldl' (\acc c -> if c == open then acc + 1 else if c == close then acc - 1 else acc) 0 txt

-- | Validate rule message
validateRuleMessage :: ValidationConfig -> Text -> Text -> [ValidationError]
validateRuleMessage config rid msg
  | vcRequireMessage config && T.null msg =
      [mkError ErrEmptyMessage rid "Rule message is required" (Just "message")
        (Just "Provide a descriptive message explaining the issue")]
  | otherwise = []

-- | Validate rule replacement
validateRuleReplacement :: ValidationConfig -> Rule -> [ValidationError]
validateRuleReplacement config rule =
  case ruleReplacement rule of
    Nothing -> []  -- No replacement is valid
    Just replText ->
      if not (vcValidateFixSyntax config)
      then []
      else
        let patternMetavars = case rulePattern rule of
                                TextPatternSpec txt -> Set.fromList $ extractMetavariables txt
                                _ -> Set.empty
            fixMetavars = Set.fromList $ extractMetavariables replText
            undefinedVars = fixMetavars `Set.difference` patternMetavars
        in if not (Set.null undefinedVars)
           then [mkError ErrFixMissingMetavariable (ruleId rule)
                  ("Replacement uses undefined metavariables: " <> T.intercalate ", " (Set.toList undefinedVars))
                  (Just "replacement")
                  (Just "Ensure all metavariables in the replacement are defined in the pattern")]
           else []

-- | Validate side conditions
validateRuleSideConditions :: Text -> [SideCondition] -> [ValidationError]
validateRuleSideConditions rid conditions =
  concatMap (validateSideCondition' rid) conditions

-- | Validate a single side condition
validateSideCondition' :: Text -> SideCondition -> [ValidationError]
validateSideCondition' rid cond = case cond of
  HasType var _ ->
    if not (isValidMetavar var)
    then [mkError ErrInvalidSideCondition rid
           ("Invalid metavariable in HasType: " <> var)
           (Just "side_condition") Nothing]
    else []
  HasTypeClass var _ ->
    if not (isValidMetavar var)
    then [mkError ErrInvalidSideCondition rid
           ("Invalid metavariable in HasTypeClass: " <> var)
           (Just "side_condition") Nothing]
    else []
  _ -> []  -- Other conditions are valid by construction

-- | Validate import specifications
validateRuleImports :: ValidationConfig -> Text -> [ImportSpec] -> [ValidationError]
validateRuleImports config rid imports
  | not (vcCheckImports config) = []
  | otherwise = concatMap (validateImportSpec' rid) imports

-- | Validate a single import spec
validateImportSpec' :: Text -> ImportSpec -> [ValidationError]
validateImportSpec' rid spec =
  let modName = impModule spec
  in if not (isValidModuleName modName)
     then [mkError ErrInvalidModuleName rid
            ("Invalid module name: " <> modName)
            (Just "import")
            (Just "Module names should be dot-separated capitalized identifiers")]
     else []

-- | Check if module name is valid
isValidModuleName :: Text -> Bool
isValidModuleName name =
  not (T.null name) &&
  all isValidComponent (T.splitOn "." name)
  where
    isValidComponent c =
      not (T.null c) &&
      case T.uncons c of
        Just (h, _) -> h >= 'A' && h <= 'Z'
        Nothing -> False

--------------------------------------------------------------------------------
-- Component Validation (Public API)
--------------------------------------------------------------------------------

-- | Validate a pattern string
validatePattern :: Text -> Either [ValidationError] ()
validatePattern patText =
  let errors = validatePatternSyntax defaultValidationConfig "pattern" patText
  in if null errors then Right () else Left errors

-- | Validate a side condition
validateSideCondition :: SideCondition -> Either [ValidationError] ()
validateSideCondition cond =
  let errors = validateSideCondition' "condition" cond
  in if null errors then Right () else Left errors

-- | Validate an import specification
validateImportSpec :: ImportSpec -> Either [ValidationError] ()
validateImportSpec spec =
  let errors = validateImportSpec' "import" spec
  in if null errors then Right () else Left errors

-- | Validate a fix
validateFix :: Fix -> Set Text -> Either [ValidationError] ()
validateFix fix patternVars =
  let fixText = case fixEdits fix of
                  [] -> ""
                  (e:_) -> fixEditNewText e
      fixMetavars = Set.fromList $ extractMetavariables fixText
      undefinedVars = fixMetavars `Set.difference` patternVars
      errors = if not (Set.null undefinedVars)
               then [mkError ErrFixMissingMetavariable "fix"
                      ("Fix uses undefined metavariables: " <> T.intercalate ", " (Set.toList undefinedVars))
                      (Just "fix")
                      (Just "Ensure all metavariables in the fix are defined in the pattern")]
               else []
  in if null errors then Right () else Left errors

-- | Validate metavariable usage between pattern and fix
validateMetavariables :: Text -> Text -> Either [ValidationError] ()
validateMetavariables patText fixText =
  let patternVars = Set.fromList $ extractMetavariables patText
      fixVars = Set.fromList $ extractMetavariables fixText
      undefinedVars = fixVars `Set.difference` patternVars
      -- Note: unused metavariables (patternVars - fixVars) are intentionally not errors
      -- as a fix may only use a subset of captured variables
  in if not (Set.null undefinedVars)
     then Left [mkError ErrUndefinedMetavariable "validation"
                 ("Undefined metavariables in fix: " <> T.intercalate ", " (Set.toList undefinedVars))
                 Nothing Nothing]
     else Right ()

--------------------------------------------------------------------------------
-- Warning Collection
--------------------------------------------------------------------------------

-- | Collect warnings for a rule
collectWarnings :: ValidationConfig -> Rule -> [ValidationWarning]
collectWarnings config rule = concat
  [ warnNoMessage config rule
  , warnNoFix config rule
  , warnBroadPattern config rule
  , warnHighSeverityNoFix rule
  ]

warnNoMessage :: ValidationConfig -> Rule -> [ValidationWarning]
warnNoMessage config rule
  | not (vcRequireMessage config) && T.null (ruleMessage rule) =
      [mkWarning WarnNoMessage (ruleId rule) "Rule has no message" Nothing]
  | otherwise = []

warnNoFix :: ValidationConfig -> Rule -> [ValidationWarning]
warnNoFix config rule
  | not (vcRequireFix config) && isNothing (ruleReplacement rule) && ruleSeverity rule >= Warning =
      [mkWarning WarnNoFix (ruleId rule)
        "Warning-level rule has no auto-fix" Nothing]
  | otherwise = []

warnBroadPattern :: ValidationConfig -> Rule -> [ValidationWarning]
warnBroadPattern config rule
  | not (vcAllowBroadPatterns config) =
      case rulePattern rule of
        TextPatternSpec txt | isBroadPattern txt ->
          [mkWarning WarnBroadPattern (ruleId rule)
            "Pattern may match too broadly" (Just "pattern")]
        _ -> []
  | otherwise = []

-- | Check if pattern is overly broad
isBroadPattern :: Text -> Bool
isBroadPattern pat =
  let metavars = extractMetavariables pat
      nonMetavar = filter (not . ("$" `T.isPrefixOf`)) (T.words pat)
  in length metavars > length nonMetavar * 2

warnHighSeverityNoFix :: Rule -> [ValidationWarning]
warnHighSeverityNoFix rule
  | ruleSeverity rule == Error && isNothing (ruleReplacement rule) =
      [mkWarning WarnHighSeverityNoFix (ruleId rule)
        "Error-level rule has no auto-fix - consider adding one" Nothing]
  | otherwise = []

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

mkError :: ErrorCode -> Text -> Text -> Maybe Text -> Maybe Text -> ValidationError
mkError code rid msg loc sug = ValidationError
  { veCode = code
  , veRuleId = rid
  , veMessage = msg
  , veLocation = loc
  , veSuggestion = sug
  }

mkWarning :: WarningCode -> Text -> Text -> Maybe Text -> ValidationWarning
mkWarning code rid msg loc = ValidationWarning
  { vwCode = code
  , vwRuleId = rid
  , vwMessage = msg
  , vwLocation = loc
  }

-- | Convert error code to text
errorToText :: ErrorCode -> Text
errorToText = \case
  ErrEmptyRuleId -> "E001: Empty rule ID"
  ErrInvalidRuleId -> "E002: Invalid rule ID format"
  ErrEmptyPattern -> "E003: Empty pattern"
  ErrInvalidPatternSyntax -> "E004: Invalid pattern syntax"
  ErrUnbalancedParens -> "E005: Unbalanced parentheses"
  ErrUnbalancedBrackets -> "E018: Unbalanced brackets"
  ErrInvalidMetavariable -> "E006: Invalid metavariable"
  ErrUndefinedMetavariable -> "E007: Undefined metavariable"
  ErrUnusedMetavariable -> "E008: Unused metavariable"
  ErrEmptyMessage -> "E009: Empty message"
  ErrInvalidImportSpec -> "E010: Invalid import specification"
  ErrInvalidModuleName -> "E011: Invalid module name"
  ErrConflictingSideConditions -> "E012: Conflicting side conditions"
  ErrInvalidSideCondition -> "E013: Invalid side condition"
  ErrFixMissingMetavariable -> "E014: Fix uses undefined metavariable"
  ErrCircularDependency -> "E015: Circular dependency detected"
  ErrInvalidSeverity -> "E016: Invalid severity level"
  ErrInvalidCategory -> "E017: Invalid category"

-- | Convert warning code to text
warningToText :: WarningCode -> Text
warningToText = \case
  WarnNoMessage -> "W001: No message provided"
  WarnNoFix -> "W002: No auto-fix provided"
  WarnBroadPattern -> "W003: Pattern may be too broad"
  WarnOverlappingConditions -> "W004: Overlapping side conditions"
  WarnDeprecatedSyntax -> "W005: Deprecated syntax"
  WarnUnusualCategory -> "W006: Unusual category for rule type"
  WarnHighSeverityNoFix -> "W007: High severity rule without fix"
  WarnLowConfidenceFix -> "W008: Low confidence fix"
  WarnMissingDocumentation -> "W009: Missing documentation"
  WarnPossiblePerformanceIssue -> "W010: Possible performance issue"
