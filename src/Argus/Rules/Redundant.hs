{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Argus.Rules.Redundant
-- Description : Detection and auto-fix of redundant code patterns
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module detects redundant code patterns in Haskell and provides
-- auto-fixes for:
-- - Boolean simplifications (if x then True else False → x)
-- - Redundant constructs (id x → x)
-- - Eta reductions (\x -> f x → f)
-- - Control flow simplifications (if c then action else pure () → when c action)
--
-- Unlike the previous string-based implementation, this uses the ASTMatch
-- infrastructure for accurate, AST-level pattern matching.
module Argus.Rules.Redundant
  ( -- * Detection
    detectRedundant
  , detectRedundantAST
  , RedundantFinding (..)
  , RedundantCategory (..)

    -- * Configuration
  , RedundantConfig (..)
  , defaultRedundantConfig

    -- * AST-Based Rules
  , redundantASTRules
  , loadRedundantRules
  ) where

import Data.Maybe (catMaybes)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

-- GHC imports
import "ghc-lib-parser" GHC.Hs (HsModule, GhcPs)

import Argus.Analysis.TextProcessing (isCodeLine, patternInCode)
import Argus.Types hiding (FixSafety(..))
import Argus.Rules.ASTMatch qualified as AST
import Argus.Rules.Types (Rule(..), SafetyLevel(..), Category(..), RulePattern(..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Category of redundant pattern
data RedundantCategory
  = BooleanSimplification   -- ^ Boolean expression simplifications
  | RedundantConstruct      -- ^ Redundant function applications
  | EtaReduction            -- ^ Lambda eta reductions
  | ControlFlowSimplification -- ^ Control flow simplifications
  | IdentityOperation       -- ^ Operations that are identity
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A detected redundant pattern
data RedundantFinding = RedundantFinding
  { rfCategory    :: RedundantCategory
  , rfSpan        :: SrcSpan
  , rfCode        :: Text           -- ^ Original code
  , rfReplacement :: Text           -- ^ Suggested replacement
  , rfExplanation :: Text           -- ^ Why it's redundant
  , rfSeverity    :: Severity
  , rfAutoFix     :: [Fix]          -- ^ Auto-fix if available
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for redundancy detection
data RedundantConfig = RedundantConfig
  { redEnabled           :: Bool   -- ^ Enable detection
  , redCheckBoolean      :: Bool   -- ^ Check boolean simplifications
  , redCheckRedundant    :: Bool   -- ^ Check redundant constructs
  , redCheckEta          :: Bool   -- ^ Check eta reductions
  , redCheckControlFlow  :: Bool   -- ^ Check control flow patterns
  , redCheckIdentity     :: Bool   -- ^ Check identity operations
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default configuration
defaultRedundantConfig :: RedundantConfig
defaultRedundantConfig = RedundantConfig
  { redEnabled = True
  , redCheckBoolean = True
  , redCheckRedundant = True
  , redCheckEta = False       -- Off by default - may affect type inference
  , redCheckControlFlow = True
  , redCheckIdentity = True
  }

--------------------------------------------------------------------------------
-- AST-Based Rule Definitions
--------------------------------------------------------------------------------

-- | Rule definition with pattern, fix, and metadata
data RedundantRule = RedundantRule
  { rrId          :: Text
  , rrCategory    :: RedundantCategory
  , rrPattern     :: Text
  , rrFix         :: Maybe Text
  , rrMessage     :: Text
  , rrSeverity    :: Severity
  , rrSafety      :: SafetyLevel
  , rrSideCondition :: Maybe Text
  }

-- | All redundant pattern rules organized by category
allRedundantRules :: [RedundantRule]
allRedundantRules = concat
  [ booleanRules
  , redundantConstructRules
  , etaRules
  , controlFlowRules
  , identityRules
  ]

-- | Boolean simplification patterns
booleanRules :: [RedundantRule]
booleanRules =
  [ RedundantRule
      { rrId = "redundant/if-true-false"
      , rrCategory = BooleanSimplification
      , rrPattern = "if x then True else False"
      , rrFix = Just "x"
      , rrMessage = "'if x then True else False' is equivalent to 'x'"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/if-false-true"
      , rrCategory = BooleanSimplification
      , rrPattern = "if x then False else True"
      , rrFix = Just "not x"
      , rrMessage = "'if x then False else True' is equivalent to 'not x'"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/eq-true"
      , rrCategory = BooleanSimplification
      , rrPattern = "x == True"
      , rrFix = Just "x"
      , rrMessage = "'x == True' is equivalent to 'x'"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/eq-false"
      , rrCategory = BooleanSimplification
      , rrPattern = "x == False"
      , rrFix = Just "not x"
      , rrMessage = "'x == False' is equivalent to 'not x'"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/double-not"
      , rrCategory = BooleanSimplification
      , rrPattern = "not (not x)"
      , rrFix = Just "x"
      , rrMessage = "'not (not x)' is equivalent to 'x'"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/double-not-compose"
      , rrCategory = BooleanSimplification
      , rrPattern = "not . not"
      , rrFix = Just "id"
      , rrMessage = "'not . not' is equivalent to 'id'"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/not-eq"
      , rrCategory = BooleanSimplification
      , rrPattern = "not (x == y)"
      , rrFix = Just "x /= y"
      , rrMessage = "'not (x == y)' is equivalent to 'x /= y'"
      , rrSeverity = Suggestion
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/not-neq"
      , rrCategory = BooleanSimplification
      , rrPattern = "not (x /= y)"
      , rrFix = Just "x == y"
      , rrMessage = "'not (x /= y)' is equivalent to 'x == y'"
      , rrSeverity = Suggestion
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/not-gt"
      , rrCategory = BooleanSimplification
      , rrPattern = "not (x > y)"
      , rrFix = Just "x <= y"
      , rrMessage = "'not (x > y)' is equivalent to 'x <= y'"
      , rrSeverity = Suggestion
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/not-lt"
      , rrCategory = BooleanSimplification
      , rrPattern = "not (x < y)"
      , rrFix = Just "x >= y"
      , rrMessage = "'not (x < y)' is equivalent to 'x >= y'"
      , rrSeverity = Suggestion
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  ]

-- | Redundant construct patterns
redundantConstructRules :: [RedundantRule]
redundantConstructRules =
  [ RedundantRule
      { rrId = "redundant/id-apply"
      , rrCategory = RedundantConstruct
      , rrPattern = "id x"
      , rrFix = Just "x"
      , rrMessage = "'id x' is equivalent to 'x'"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/id-dollar"
      , rrCategory = RedundantConstruct
      , rrPattern = "id $ x"
      , rrFix = Just "x"
      , rrMessage = "'id $ x' is equivalent to 'x'"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/maybe-nothing-just"
      , rrCategory = RedundantConstruct
      , rrPattern = "maybe Nothing Just x"
      , rrFix = Just "x"
      , rrMessage = "'maybe Nothing Just' is equivalent to 'id' for Maybe"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/either-left-right"
      , rrCategory = RedundantConstruct
      , rrPattern = "either Left Right x"
      , rrFix = Just "x"
      , rrMessage = "'either Left Right' is equivalent to 'id' for Either"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/maybe-x-id"
      , rrCategory = RedundantConstruct
      , rrPattern = "maybe x id y"
      , rrFix = Just "fromMaybe x y"
      , rrMessage = "'maybe x id' is equivalent to 'fromMaybe x'"
      , rrSeverity = Suggestion
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/fst-tuple"
      , rrCategory = RedundantConstruct
      , rrPattern = "fst (x, y)"
      , rrFix = Just "x"
      , rrMessage = "'fst (x, y)' is equivalent to 'x'"
      , rrSeverity = Suggestion
      , rrSafety = Safe
      , rrSideCondition = Just "isPure y"
      }
  , RedundantRule
      { rrId = "redundant/snd-tuple"
      , rrCategory = RedundantConstruct
      , rrPattern = "snd (x, y)"
      , rrFix = Just "y"
      , rrMessage = "'snd (x, y)' is equivalent to 'y'"
      , rrSeverity = Suggestion
      , rrSafety = Safe
      , rrSideCondition = Just "isPure x"
      }
  ]

-- | Eta reduction patterns
etaRules :: [RedundantRule]
etaRules =
  [ RedundantRule
      { rrId = "redundant/eta-reduce"
      , rrCategory = EtaReduction
      , rrPattern = "\\x -> f x"
      , rrFix = Just "f"
      , rrMessage = "'\\x -> f x' can be eta-reduced to 'f'"
      , rrSeverity = Suggestion
      , rrSafety = Unsafe  -- May affect type inference
      , rrSideCondition = Just "notFreeIn x f"
      }
  ]

-- | Control flow simplification patterns
controlFlowRules :: [RedundantRule]
controlFlowRules =
  [ RedundantRule
      { rrId = "redundant/if-pure-unit"
      , rrCategory = ControlFlowSimplification
      , rrPattern = "if c then action else pure ()"
      , rrFix = Just "when c action"
      , rrMessage = "'if c then action else pure ()' can be 'when c action'"
      , rrSeverity = Suggestion
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/if-return-unit"
      , rrCategory = ControlFlowSimplification
      , rrPattern = "if c then action else return ()"
      , rrFix = Just "when c action"
      , rrMessage = "'if c then action else return ()' can be 'when c action'"
      , rrSeverity = Suggestion
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/if-unless-pure"
      , rrCategory = ControlFlowSimplification
      , rrPattern = "if c then pure () else action"
      , rrFix = Just "unless c action"
      , rrMessage = "'if c then pure () else action' can be 'unless c action'"
      , rrSeverity = Suggestion
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/if-unless-return"
      , rrCategory = ControlFlowSimplification
      , rrPattern = "if c then return () else action"
      , rrFix = Just "unless c action"
      , rrMessage = "'if c then return () else action' can be 'unless c action'"
      , rrSeverity = Suggestion
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  ]

-- | Identity operation patterns
identityRules :: [RedundantRule]
identityRules =
  [ RedundantRule
      { rrId = "redundant/map-id"
      , rrCategory = IdentityOperation
      , rrPattern = "map id x"
      , rrFix = Just "x"
      , rrMessage = "'map id x' is equivalent to 'x'"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/fmap-id"
      , rrCategory = IdentityOperation
      , rrPattern = "fmap id x"
      , rrFix = Just "x"
      , rrMessage = "'fmap id x' is equivalent to 'x' (functor identity law)"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/filter-const-true"
      , rrCategory = IdentityOperation
      , rrPattern = "filter (const True) x"
      , rrFix = Just "x"
      , rrMessage = "'filter (const True)' is equivalent to 'id'"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/double-reverse"
      , rrCategory = IdentityOperation
      , rrPattern = "reverse (reverse x)"
      , rrFix = Just "x"
      , rrMessage = "'reverse . reverse' is equivalent to 'id'"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/double-reverse-compose"
      , rrCategory = IdentityOperation
      , rrPattern = "reverse . reverse"
      , rrFix = Just "id"
      , rrMessage = "'reverse . reverse' is equivalent to 'id'"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/double-reverse-dollar"
      , rrCategory = IdentityOperation
      , rrPattern = "reverse $ reverse x"
      , rrFix = Just "x"
      , rrMessage = "'reverse $ reverse' is equivalent to 'id'"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/id-compose-right"
      , rrCategory = IdentityOperation
      , rrPattern = "f . id"
      , rrFix = Just "f"
      , rrMessage = "'f . id' is equivalent to 'f'"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/id-compose-left"
      , rrCategory = IdentityOperation
      , rrPattern = "id . f"
      , rrFix = Just "f"
      , rrMessage = "'id . f' is equivalent to 'f'"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/catMaybes-map-Just"
      , rrCategory = IdentityOperation
      , rrPattern = "catMaybes (map Just x)"
      , rrFix = Just "x"
      , rrMessage = "'catMaybes . map Just' is equivalent to 'id'"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/catMaybes-map-Just-compose"
      , rrCategory = IdentityOperation
      , rrPattern = "catMaybes . map Just"
      , rrFix = Just "id"
      , rrMessage = "'catMaybes . map Just' is equivalent to 'id'"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/rights-map-Right"
      , rrCategory = IdentityOperation
      , rrPattern = "rights (map Right x)"
      , rrFix = Just "x"
      , rrMessage = "'rights . map Right' is equivalent to 'id'"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/lefts-map-Left"
      , rrCategory = IdentityOperation
      , rrPattern = "lefts (map Left x)"
      , rrFix = Just "x"
      , rrMessage = "'lefts . map Left' is equivalent to 'id'"
      , rrSeverity = Warning
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  , RedundantRule
      { rrId = "redundant/concat-singleton"
      , rrCategory = IdentityOperation
      , rrPattern = "concat [[x]]"
      , rrFix = Just "[x]"
      , rrMessage = "'concat [[x]]' is equivalent to '[x]'"
      , rrSeverity = Suggestion
      , rrSafety = Safe
      , rrSideCondition = Nothing
      }
  ]

--------------------------------------------------------------------------------
-- Convert to Unified Rules
--------------------------------------------------------------------------------

-- | Convert our rule definitions to unified Rules
convertToRule :: RedundantRule -> Rule
convertToRule RedundantRule{..} = Rule
  { ruleId = rrId
  , ruleCategory = Style  -- Redundant patterns are style category
  , ruleSeverity = rrSeverity
  , ruleMessage = rrMessage
  , ruleExplanation = Nothing
  , rulePattern = ASTPatternSpec rrPattern
  , ruleReplacement = rrFix
  , ruleConditions = case rrSideCondition >>= AST.parseSideCondition of
      Nothing -> []
      Just sc -> [sc]
  , ruleSafety = rrSafety
  , ruleAddImports = []
  , ruleRemoveImports = []
  , ruleEnabled = True
  , ruleWithin = []
  , ruleExcept = []
  , ruleDeprecated = Nothing
  , ruleTags = ["redundant"]
  , ruleReferences = []
  , ruleFixDescription = Nothing
  , ruleNote = Nothing
  , ruleSourceModule = Nothing
  , ruleTargetModule = Nothing
  , ruleTarget = Nothing  -- Use category default (Style -> TargetCode)
  }

-- | Load all redundant rules as unified Rules
loadRedundantRules :: IO [Rule]
loadRedundantRules = pure $ map convertToRule allRedundantRules

-- | Get all redundant rules that match the configuration
redundantASTRules :: RedundantConfig -> IO [Rule]
redundantASTRules config
  | not (redEnabled config) = pure []
  | otherwise = do
      allRules <- loadRedundantRules
      pure $ filter (ruleEnabledByConfig config) allRules

-- | Check if a rule is enabled by the configuration
ruleEnabledByConfig :: RedundantConfig -> Rule -> Bool
ruleEnabledByConfig config rule =
  let rid = ruleId rule
      category = inferCategoryFromId rid
  in case category of
       BooleanSimplification -> redCheckBoolean config
       RedundantConstruct -> redCheckRedundant config
       EtaReduction -> redCheckEta config
       ControlFlowSimplification -> redCheckControlFlow config
       IdentityOperation -> redCheckIdentity config

-- | Infer category from rule ID
inferCategoryFromId :: Text -> RedundantCategory
inferCategoryFromId ruleId
  | "if-true-false" `T.isInfixOf` ruleId = BooleanSimplification
  | "if-false-true" `T.isInfixOf` ruleId = BooleanSimplification
  | "eq-true" `T.isInfixOf` ruleId = BooleanSimplification
  | "eq-false" `T.isInfixOf` ruleId = BooleanSimplification
  | "double-not" `T.isInfixOf` ruleId = BooleanSimplification
  | "not-eq" `T.isInfixOf` ruleId = BooleanSimplification
  | "not-neq" `T.isInfixOf` ruleId = BooleanSimplification
  | "not-gt" `T.isInfixOf` ruleId = BooleanSimplification
  | "not-lt" `T.isInfixOf` ruleId = BooleanSimplification
  | "eta" `T.isInfixOf` ruleId = EtaReduction
  | "if-pure" `T.isInfixOf` ruleId = ControlFlowSimplification
  | "if-return" `T.isInfixOf` ruleId = ControlFlowSimplification
  | "if-unless" `T.isInfixOf` ruleId = ControlFlowSimplification
  | "map-id" `T.isInfixOf` ruleId = IdentityOperation
  | "fmap-id" `T.isInfixOf` ruleId = IdentityOperation
  | "filter-const" `T.isInfixOf` ruleId = IdentityOperation
  | "reverse" `T.isInfixOf` ruleId = IdentityOperation
  | "id-compose" `T.isInfixOf` ruleId = IdentityOperation
  | "catMaybes" `T.isInfixOf` ruleId = IdentityOperation
  | "rights" `T.isInfixOf` ruleId = IdentityOperation
  | "lefts" `T.isInfixOf` ruleId = IdentityOperation
  | "concat" `T.isInfixOf` ruleId = IdentityOperation
  | otherwise = RedundantConstruct

--------------------------------------------------------------------------------
-- Detection
--------------------------------------------------------------------------------

-- | Detect redundant patterns using AST-based matching
-- This is the preferred method when you have a parsed AST
detectRedundantAST :: RedundantConfig -> FilePath -> Text -> HsModule GhcPs -> IO [Diagnostic]
detectRedundantAST config filepath _moduleName hsmod
  | not (redEnabled config) = pure []
  | otherwise = do
      rules <- redundantASTRules config
      AST.applyASTRules rules filepath "" hsmod

-- | Detect redundant patterns in source code (text-based fallback)
-- For backward compatibility - uses text-based regex matching when AST not available
detectRedundant :: RedundantConfig -> FilePath -> Text -> [Diagnostic]
detectRedundant config filepath content
  | not (redEnabled config) = []
  | otherwise =
      let lines' = zip [1..] (T.lines content)
      in concatMap (detectLine config filepath) lines'

-- | Detect patterns in a single line (simplified text-based fallback)
-- Skips comment lines and filters string literals to avoid false positives
detectLine :: RedundantConfig -> FilePath -> (Int, Text) -> [Diagnostic]
detectLine config filepath (lineNum, lineText)
  | not (isCodeLine lineText) = []  -- Skip comment lines
  | otherwise = catMaybes $ concat
  [ if redCheckBoolean config then detectBooleanPatterns filepath lineNum lineText else []
  , if redCheckRedundant config then detectRedundantPatterns filepath lineNum lineText else []
  , if redCheckIdentity config then detectIdentityPatterns filepath lineNum lineText else []
  , if redCheckControlFlow config then detectControlFlowPatterns filepath lineNum lineText else []
  ]

-- | Detect boolean simplification patterns
detectBooleanPatterns :: FilePath -> Int -> Text -> [Maybe Diagnostic]
detectBooleanPatterns filepath lineNum lineText =
  [ detectPattern filepath lineNum lineText
      "if " " then True else False"
      "'if x then True else False' is equivalent to 'x'. Replace with: x"
      "redundant/if-true-false"
  , detectPattern filepath lineNum lineText
      "if " " then False else True"
      "'if x then False else True' is equivalent to 'not x'. Replace with: not x"
      "redundant/if-false-true"
  , detectPattern filepath lineNum lineText
      "" " == True"
      "'x == True' is equivalent to 'x'. Replace with: x"
      "redundant/eq-true"
  , detectPattern filepath lineNum lineText
      "" " == False"
      "'x == False' is equivalent to 'not x'. Replace with: not x"
      "redundant/eq-false"
  , detectPattern filepath lineNum lineText
      "not (not " ")"
      "'not (not x)' is equivalent to 'x'. Replace with: x"
      "redundant/double-not"
  ]

-- | Detect redundant construct patterns
detectRedundantPatterns :: FilePath -> Int -> Text -> [Maybe Diagnostic]
detectRedundantPatterns filepath lineNum lineText =
  [ detectPattern filepath lineNum lineText
      "maybe Nothing Just " ""
      "'maybe Nothing Just' is equivalent to 'id' for Maybe. Replace with: id"
      "redundant/maybe-nothing-just"
  , detectPattern filepath lineNum lineText
      "either Left Right " ""
      "'either Left Right' is equivalent to 'id' for Either. Replace with: id"
      "redundant/either-left-right"
  ]

-- | Detect identity operation patterns
detectIdentityPatterns :: FilePath -> Int -> Text -> [Maybe Diagnostic]
detectIdentityPatterns filepath lineNum lineText =
  [ detectPattern filepath lineNum lineText
      "map id " ""
      "'map id x' is equivalent to 'x'. Replace with: x"
      "redundant/map-id"
  , detectPattern filepath lineNum lineText
      "fmap id " ""
      "'fmap id x' is equivalent to 'x' (functor identity law). Replace with: x"
      "redundant/fmap-id"
  , detectPattern filepath lineNum lineText
      "filter (const True) " ""
      "'filter (const True)' is equivalent to 'id'. Replace with: id"
      "redundant/filter-const-true"
  , detectPattern filepath lineNum lineText
      "reverse . reverse" ""
      "'reverse . reverse' is equivalent to 'id'. Replace with: id"
      "redundant/double-reverse-compose"
  , detectPattern filepath lineNum lineText
      "reverse $ reverse " ""
      "'reverse $ reverse' is equivalent to 'id'. Replace with: x"
      "redundant/double-reverse-dollar"
  ]

-- | Detect control flow patterns
detectControlFlowPatterns :: FilePath -> Int -> Text -> [Maybe Diagnostic]
detectControlFlowPatterns filepath lineNum lineText =
  [ detectPattern filepath lineNum lineText
      "if " " else pure ()"
      "'if c then action else pure ()' can be 'when c action'. Replace with: when c action"
      "redundant/if-pure-unit"
  , detectPattern filepath lineNum lineText
      "if " " else return ()"
      "'if c then action else return ()' can be 'when c action'. Replace with: when c action"
      "redundant/if-return-unit"
  ]

-- | Helper to detect a simple pattern
-- Uses patternInCode to filter out matches in comments and string literals
detectPattern :: FilePath -> Int -> Text -> Text -> Text -> Text -> Text -> Maybe Diagnostic
detectPattern filepath lineNum lineText prefix suffix message code
  | (not . T.null $ prefix) && (not . T.null $ suffix) =
      if patternInCode prefix lineText && patternInCode suffix lineText
      then Just $ mkDiagnostic filepath lineNum lineText prefix message code
      else Nothing
  | not . T.null $ prefix =
      if patternInCode prefix lineText
      then Just $ mkDiagnostic filepath lineNum lineText prefix message code
      else Nothing
  | not . T.null $ suffix =
      if patternInCode suffix lineText
      then Just $ mkDiagnostic filepath lineNum lineText suffix message code
      else Nothing
  | otherwise = Nothing

-- | Create a diagnostic from detection
mkDiagnostic :: FilePath -> Int -> Text -> Text -> Text -> Text -> Diagnostic
mkDiagnostic filepath lineNum lineText needle message code =
  let col = case T.breakOn needle lineText of
              (before, _) -> T.length before + 1
  in Diagnostic
    { diagSpan = mkSrcSpanRaw filepath lineNum col lineNum (col + T.length needle)
    , diagSeverity = Warning
    , diagKind = CodePattern
    , diagMessage = message
    , diagCode = Just code
    , diagFixes = []  -- Auto-fix not available in text-based mode
    , diagRelated = []
    }

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Convert finding to diagnostic
_findingToDiagnostic :: RedundantFinding -> Diagnostic
_findingToDiagnostic RedundantFinding{..} = Diagnostic
  { diagSpan = rfSpan
  , diagSeverity = rfSeverity
  , diagKind = CodePattern
  , diagMessage = rfExplanation <> ". Replace with: " <> rfReplacement
  , diagCode = Just $ "redundant/" <> _categoryCode rfCategory
  , diagFixes = rfAutoFix
  , diagRelated = []
  }

_categoryCode :: RedundantCategory -> Text
_categoryCode = \case
  BooleanSimplification -> "boolean"
  RedundantConstruct -> "construct"
  EtaReduction -> "eta"
  ControlFlowSimplification -> "control-flow"
  IdentityOperation -> "identity"
