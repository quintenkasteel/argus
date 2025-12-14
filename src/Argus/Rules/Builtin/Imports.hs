{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Builtin.Imports
-- Description : Import and extension rules
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
--
-- Rules for imports, language extensions, and Prelude alternatives.

module Argus.Rules.Builtin.Imports
  ( -- * Rule Sets
    importRules
  , importOptRules
  , extensionRules
  , preludeAltRules

    -- * Import Optimizations
  , duplicateImport
  , emptyImport
  , redundantQualified
  , hidingUnnecessary
  , importSuggestion
  , qualifiedSuggestion

    -- * Extension Rules
  , lambdaCaseSuggest
  , multiWayIfSuggest
  , typeAppSuggest
  , deriveGenericSuggest
  , recordWildcardsSuggest
  , safeUnsafeConflict
  , unusedExtension
  , overloadedStringsSuggest
  , bangPatternsSuggest

    -- * Prelude Alternatives
  , headAlternative
  , tailAlternative
  , initAlternative
  , lastAlternative
  , indexAlternative
  , readAlternative
  , minimumAlternative
  , maximumAlternative
  , fromJustAlternative
  , errorInPure
  , undefinedWarn

    -- * Rule Count
  , importRuleCount
  ) where

import Argus.Rules.DSL

--------------------------------------------------------------------------------
-- Rule Sets
--------------------------------------------------------------------------------

-- | All import-related rules.
importRules :: [Rule]
importRules = mconcat
  [ importOptRules
  , extensionRules
  , preludeAltRules
  ]

-- | Total count of import rules.
importRuleCount :: Int
importRuleCount = length importRules

--------------------------------------------------------------------------------
-- Import Optimization Rules
--------------------------------------------------------------------------------

-- | Rules for import optimizations.
-- Note: Most qualified import suggestions are now handled by module restrictions
-- in Argus.Rules.ConfigurableRules (defaultModuleRestrictions) with auto-fix support.
importOptRules :: [Rule]
importOptRules =
  [ duplicateImport
  , emptyImport
  , redundantQualified
  , hidingUnnecessary
  , importSuggestion
  , qualifiedSuggestion
  ]

-- | Duplicate import warning.
duplicateImport :: Rule
duplicateImport =
  rule "duplicate-import" $
    matchText "^import\\s+([A-Z][A-Za-z.]+).*\\n.*^import\\s+\\1\\b"
    & category Style
    & severity Warning
    & message "Duplicate import of same module"

-- | import M () warning.
emptyImport :: Rule
emptyImport =
  rule "empty-import" $
    matchText "import\\s+[A-Z][A-Za-z.]+\\s*\\(\\s*\\)"
    & category Style
    & severity Warning
    & message "Empty import list - did you mean to import instances?"

-- | import qualified M as M redundancy.
-- Detects when a module is qualified with an alias identical to its own name.
-- Example: "import qualified Data.Map as Data.Map" is redundant
-- Note: The regex uses a backreference (\1) to match when alias equals module name
redundantQualified :: Rule
redundantQualified =
  rule "redundant-qualified" $
    matchText "import\\s+qualified\\s+([A-Z][A-Za-z0-9.']*)\\s+as\\s+\\1\\b"
    & category Style
    & severity Suggestion
    & message "Redundant qualification - module aliased to itself, remove 'as' clause"
    & safetyLevel ManualReview

-- | import M hiding (...) check.
hidingUnnecessary :: Rule
hidingUnnecessary =
  rule "hiding-unnecessary" $
    matchText "import\\s+[A-Z][A-Za-z.]+\\s+hiding"
    & category Style
    & severity Info
    & message "Using hiding - check if explicit import list is cleaner"
    & safetyLevel ManualReview

-- Note: Qualified import suggestions are now handled by module restrictions
-- in Argus.Rules.ConfigurableRules with auto-fix support.
-- These rules remain for backwards compatibility and as fallback detection.

-- | Generic import suggestion for common container/text modules
-- This catches any unqualified import of these modules
importSuggestion :: Rule
importSuggestion =
  rule "import-suggestion" $
    matchText "^import\\s+Data\\.(Map|Set|Text|ByteString|HashMap|IntMap|Vector|Sequence)(\\.\\w+)?\\s*$"
    & category Style
    & severity Suggestion
    & message "Consider qualified import for containers/text modules"
    & safetyLevel ManualReview

-- | Qualified import suggestion.
qualifiedSuggestion :: Rule
qualifiedSuggestion =
  rule "qualified-suggestion" $
    matchText "import\\s+qualified\\s+[A-Z][A-Za-z.]+\\s*$"
    & category Style
    & severity Info
    & message "Qualified import without alias - consider adding 'as X'"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Extension Rules
--------------------------------------------------------------------------------

-- | Rules for language extensions.
extensionRules :: [Rule]
extensionRules =
  [ lambdaCaseSuggest
  , multiWayIfSuggest
  , typeAppSuggest
  , deriveGenericSuggest
  , recordWildcardsSuggest
  , safeUnsafeConflict
  , unusedExtension
  , overloadedStringsSuggest
  , bangPatternsSuggest
  ]

-- | LambdaCase suggestion.
lambdaCaseSuggest :: Rule
lambdaCaseSuggest =
  rule "lambda-case-suggest" $
    matchText "\\\\\\s*[a-z_]+\\s*->\\s*case\\s+[a-z_]+\\s+of"
    & category Style
    & severity Suggestion
    & message "Consider LambdaCase: \\x -> case x of  ==>  \\case"

-- | MultiWayIf suggestion.
multiWayIfSuggest :: Rule
multiWayIfSuggest =
  rule "multi-way-if-suggest" $
    matchText "if\\s+[^t]+then\\s+if\\s+[^t]+then"
    & category Style
    & severity Info
    & message "Nested if-then-else - consider MultiWayIf"
    & safetyLevel ManualReview

-- | TypeApplications suggestion.
typeAppSuggest :: Rule
typeAppSuggest =
  rule "type-app-suggest" $
    matchText "@[A-Z][a-zA-Z]*\\b"
    & category Style
    & severity Info
    & message "Type application requires TypeApplications extension"
    & safetyLevel ManualReview

-- | DeriveGeneric suggestion.
deriveGenericSuggest :: Rule
deriveGenericSuggest =
  rule "derive-generic-suggest" $
    matchText "deriving.*Generic"
    & category Style
    & severity Info
    & message "Deriving Generic requires DeriveGeneric extension"
    & safetyLevel ManualReview

-- | RecordWildCards suggestion.
recordWildcardsSuggest :: Rule
recordWildcardsSuggest =
  rule "record-wildcards-suggest" $
    matchText "\\{\\s*\\.\\.\\s*\\}"
    & category Style
    & severity Info
    & message "Record wildcards require RecordWildCards extension"
    & safetyLevel ManualReview

-- | Safe + unsafe operation conflict.
safeUnsafeConflict :: Rule
safeUnsafeConflict =
  rule "safe-unsafe-conflict" $
    matchText "\\{-#\\s*LANGUAGE\\s+Safe\\s*#-\\}"
    & category Safety
    & severity Info
    & message "Safe Haskell enabled - ensure no unsafe operations"
    & safetyLevel ManualReview

-- | Unused extension warning.
unusedExtension :: Rule
unusedExtension =
  rule "unused-extension" $
    matchText "\\{-#\\s*LANGUAGE\\s+[A-Z][a-zA-Z]+\\s*#-\\}"
    & category Style
    & severity Info
    & message "Language extension - verify it is used"
    & safetyLevel ManualReview

-- | OverloadedStrings suggestion.
overloadedStringsSuggest :: Rule
overloadedStringsSuggest =
  rule "overloaded-strings-suggest" $
    matchText "::\\s*Text|::\\s*ByteString"
    & category Style
    & severity Info
    & message "Text/ByteString literals may need OverloadedStrings"
    & safetyLevel ManualReview

-- | BangPatterns suggestion.
bangPatternsSuggest :: Rule
bangPatternsSuggest =
  rule "bang-patterns-suggest" $
    matchText "!\\s*[a-z_]+"
    & category Style
    & severity Info
    & message "Bang pattern requires BangPatterns extension"
    & safetyLevel ManualReview

--------------------------------------------------------------------------------
-- Prelude Alternative Rules
--------------------------------------------------------------------------------

-- | Rules for Prelude alternatives.
preludeAltRules :: [Rule]
preludeAltRules =
  [ headAlternative
  , tailAlternative
  , initAlternative
  , lastAlternative
  , indexAlternative
  , readAlternative
  , minimumAlternative
  , maximumAlternative
  , fromJustAlternative
  , errorInPure
  , undefinedWarn
  ]

-- | head alternative suggestion.
headAlternative :: Rule
headAlternative =
  rule "head-alternative" $
    matchText "\\bhead\\b"
    & category Safety
    & severity Suggestion
    & message "head is partial"
    & note "Consider listToMaybe, headMay, or pattern matching"

-- | tail alternative suggestion.
tailAlternative :: Rule
tailAlternative =
  rule "tail-alternative" $
    matchText "\\btail\\b"
    & category Safety
    & severity Suggestion
    & message "tail is partial"
    & note "Consider drop 1, tailMay, or pattern matching"

-- | init alternative suggestion.
initAlternative :: Rule
initAlternative =
  rule "init-alternative" $
    matchText "\\binit\\b"
    & category Safety
    & severity Suggestion
    & message "init is partial"
    & note "Consider initMay or explicit handling"

-- | last alternative suggestion.
lastAlternative :: Rule
lastAlternative =
  rule "last-alternative" $
    matchText "\\blast\\b"
    & category Safety
    & severity Suggestion
    & message "last is partial"
    & note "Consider lastMay or explicit handling"

-- | !! alternative suggestion.
indexAlternative :: Rule
indexAlternative =
  rule "index-alternative" $
    matchText "!!\\s*[0-9]+"
    & category Safety
    & severity Suggestion
    & message "!! is partial"
    & note "Consider atMay, lookup, or bounds checking"

-- | read alternative suggestion.
readAlternative :: Rule
readAlternative =
  rule "read-alternative" $
    matchText "\\bread\\b"
    & category Safety
    & severity Suggestion
    & message "read is partial"
    & note "Consider readMaybe or readEither"

-- | minimum/maximum on potentially empty.
minimumAlternative :: Rule
minimumAlternative =
  rule "minimum-alternative" $
    matchText "\\bminimum\\b"
    & category Safety
    & severity Suggestion
    & message "minimum is partial on empty"
    & note "Consider minimumMay or NonEmpty"

-- | maximum alternative.
maximumAlternative :: Rule
maximumAlternative =
  rule "maximum-alternative" $
    matchText "\\bmaximum\\b"
    & category Safety
    & severity Suggestion
    & message "maximum is partial on empty"
    & note "Consider maximumMay or NonEmpty"

-- | fromJust alternative.
fromJustAlternative :: Rule
fromJustAlternative =
  rule "fromJust-alternative" $
    matchText "\\bfromJust\\b"
    & category Safety
    & severity Warning
    & message "fromJust is partial"
    & note "Consider fromMaybe, pattern matching, or (>>=)"

-- | error in pure code.
errorInPure :: Rule
errorInPure =
  rule "error-in-pure" $
    matchText "\\berror\\s+\""
    & category Safety
    & severity Warning
    & message "error in pure code"
    & note "Consider Either or Maybe for error handling"

-- | undefined warning.
undefinedWarn :: Rule
undefinedWarn =
  rule "undefined-warn" $
    matchText "\\bundefined\\b"
    & category Safety
    & severity Warning
    & message "undefined is a placeholder"
    & note "Replace with proper implementation"
