{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.LSP.RuleInfo
-- Description : Rule explanation database for LSP hover tooltips
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides detailed explanations, examples, and documentation
-- for Argus lint rules, displayed as hover tooltips in LSP clients.
--
-- Each rule explanation includes:
--
-- * Detailed description of what the rule detects
-- * Why it matters (rationale)
-- * Examples of problematic code
-- * Examples of fixed/improved code
-- * Related rules that might also be relevant
-- * Links to external documentation
--
-- == Usage
--
-- @
-- -- Get explanation for a rule code
-- case getRuleExplanation "avoid-head" of
--   Just info -> displayTooltip (formatRuleInfo info)
--   Nothing -> displayDefault
-- @
module Argus.LSP.RuleInfo
  ( -- * Rule Information
    RuleInfo (..)
  , RuleExample (..)
  , ExampleType (..)

    -- * Rule Database
  , getRuleExplanation
  , getRuleExplanationByCode
  , getAllRuleInfos
  , getRulesByCategory

    -- * Formatting
  , formatRuleInfo
  , formatRuleInfoMarkdown
  , formatShortSummary
  , formatExample

    -- * Re-exports
  , Category (..)
  , SafetyLevel (..)
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe ()
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Data.Aeson (ToJSON(..), FromJSON(..))

import Argus.Rules.Types (Category(..), SafetyLevel(..))

--------------------------------------------------------------------------------
-- Rule Information Types
--------------------------------------------------------------------------------

-- | Comprehensive information about a lint rule
data RuleInfo = RuleInfo
  { riRuleId       :: Text             -- ^ Rule identifier (e.g., "avoid-head")
  , riRuleCode     :: Maybe Text       -- ^ Alternative code (e.g., "ARGUS001")
  , riCategory     :: Category         -- ^ Rule category
  , riShortDesc    :: Text             -- ^ One-line description
  , riLongDesc     :: Text             -- ^ Detailed explanation
  , riRationale    :: Text             -- ^ Why this rule matters
  , riExamples     :: [RuleExample]    -- ^ Code examples
  , riRelatedRules :: [Text]           -- ^ Related rule IDs
  , riDocLinks     :: [Text]           -- ^ External documentation URLs
  , riSafetyLevel  :: SafetyLevel      -- ^ Fix safety level
  , riFixAvailable :: Bool             -- ^ Whether auto-fix is available
  , riSince        :: Maybe Text       -- ^ Version when rule was added
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Type of code example
data ExampleType
  = BadExample      -- ^ Problematic code that triggers the rule
  | GoodExample     -- ^ Improved code that follows the rule
  | ContextExample  -- ^ Additional context or edge cases
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A code example with explanation
data RuleExample = RuleExample
  { reType        :: ExampleType
  , reCode        :: Text            -- ^ The code snippet
  , reExplanation :: Maybe Text      -- ^ Optional explanation
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Rule Database
--------------------------------------------------------------------------------

-- | Rule information database
ruleDatabase :: Map Text RuleInfo
ruleDatabase = Map.fromList $ map (\ri -> (riRuleId ri, ri))
  [ -- Safety rules
    avoidHeadInfo
  , avoidTailInfo
  , avoidFromJustInfo
  , avoidUndefinedInfo
  , avoidErrorInfo
  , avoidReadInfo
  , avoidBangBangInfo
  , avoidMaximumInfo
  , avoidMinimumInfo
  , avoidInitInfo
  , avoidLastInfo
  , avoidFoldr1Info
  , avoidFoldl1Info
  , avoidSuccInfo
  , avoidPredInfo
  , avoidToEnumInfo
  , avoidCycleInfo
    -- Performance rules
  , avoidNubInfo
  , preferFoldl'Info
  , avoidLengthEq0Info
  , avoidConcatMapInfo
  , preferIntMapInfo
  , avoidHeadSortInfo
  , avoidReverseAppendInfo
  , useDListInfo
  , avoidElemInfo
  , useTextNotStringInfo
    -- Space leak rules
  , strictStateInfo
  , strictFieldsInfo
  , avoidLazyIOInfo
    -- Modernization rules
  , useFmapInfo
  , useTraverseInfo
  , useVoidInfo
  , useWhenInfo
  , useUnlessInfo
  , useLambdaCaseInfo
  , useTupleSectionInfo
    -- List rules
  , useNullInfo
  , useConcatInfo
  , useIntercalateInfo
    -- Maybe rules
  , useMaybeInfo
  , useFromMaybeInfo
  , useMapMaybeInfo
  , useIsJustInfo
  , useIsNothingInfo
    -- Either rules
  , useEitherInfo
  , useLeftsInfo
  , useRightsInfo
    -- Boolean rules
  , useBoolInfo
  , simplifyNotInfo
  , redundantIfInfo
    -- Monadic rules
  , useApplicativeInfo
  , useForMInfo
  , useTraverseUnderscoreInfo
  , avoidSequenceInfo
    -- String/Text rules
  , useTextPackInfo
  , useTextUnpackInfo
    -- Numeric rules
  , useDivModInfo
  , useQuotRemInfo
    -- Import rules
  , unusedImportInfo
  , redundantImportInfo
  , qualifyPostInfo
    -- Lens rules
  , useLensViewInfo
  , useLensSetInfo
  , useLensOverInfo
  ]

-- | Get rule explanation by rule ID
getRuleExplanation :: Text -> Maybe RuleInfo
getRuleExplanation ruleId = Map.lookup ruleId ruleDatabase

-- | Get rule explanation by diagnostic code
getRuleExplanationByCode :: Text -> Maybe RuleInfo
getRuleExplanationByCode code =
  Map.elems ruleDatabase
    |> filter (\ri -> riRuleCode ri == Just code)
    |> listToMaybe
  where
    (|>) = flip ($)
    listToMaybe [] = Nothing
    listToMaybe (x:_) = Just x

-- | Get all rule information entries
getAllRuleInfos :: [RuleInfo]
getAllRuleInfos = Map.elems ruleDatabase

-- | Get rules by category
getRulesByCategory :: Category -> [RuleInfo]
getRulesByCategory cat = filter (\ri -> riCategory ri == cat) getAllRuleInfos

--------------------------------------------------------------------------------
-- Formatting
--------------------------------------------------------------------------------

-- | Format rule information as markdown for hover tooltip
formatRuleInfo :: RuleInfo -> Text
formatRuleInfo = formatRuleInfoMarkdown

-- | Format rule information as detailed markdown
formatRuleInfoMarkdown :: RuleInfo -> Text
formatRuleInfoMarkdown RuleInfo{..} =
  let header = "## " <> riRuleId <> maybeCode <> "\n\n"
      maybeCode = maybe "" (\c -> " (`" <> c <> "`)") riRuleCode
      category = "**Category:** " <> categoryToText riCategory <> "\n\n"
      shortDesc = riShortDesc <> "\n\n"
      longDesc = if T.null riLongDesc then "" else riLongDesc <> "\n\n"
      rationale = if T.null riRationale then "" else "### Why This Matters\n\n" <> riRationale <> "\n\n"
      examples = if null riExamples then "" else formatExamples riExamples
      related = if null riRelatedRules
                then ""
                else "### Related Rules\n\n" <> T.intercalate ", " (map (\r -> "`" <> r <> "`") riRelatedRules) <> "\n\n"
      docs = if null riDocLinks
             then ""
             else "### Documentation\n\n" <> T.unlines (map formatDocLink riDocLinks) <> "\n"
      fixInfo = formatFixInfo riSafetyLevel riFixAvailable
  in header <> category <> shortDesc <> longDesc <> rationale <> examples <> related <> docs <> fixInfo

-- | Format a short summary (for quick display)
formatShortSummary :: RuleInfo -> Text
formatShortSummary RuleInfo{..} =
  "**" <> riRuleId <> "**: " <> riShortDesc

-- | Format examples section
formatExamples :: [RuleExample] -> Text
formatExamples examples =
  let bads = filter (\e -> reType e == BadExample) examples
      goods = filter (\e -> reType e == GoodExample) examples
      badSection = if null bads
                   then ""
                   else "### Problematic Code\n\n" <> T.intercalate "\n\n" (map formatExample bads) <> "\n\n"
      goodSection = if null goods
                    then ""
                    else "### Recommended Alternative\n\n" <> T.intercalate "\n\n" (map formatExample goods) <> "\n\n"
  in badSection <> goodSection

-- | Format a single example
formatExample :: RuleExample -> Text
formatExample RuleExample{..} =
  let code = "```haskell\n" <> reCode <> "\n```"
      expl = maybe "" (\e -> "\n\n" <> e) reExplanation
  in code <> expl

-- | Format fix availability information
formatFixInfo :: SafetyLevel -> Bool -> Text
formatFixInfo safety hasfix
  | not hasfix = "**Fix:** Manual fix required\n"
  | otherwise = "**Fix:** Auto-fix available (" <> safetyLevelToText safety <> ")\n"

-- | Format documentation link
formatDocLink :: Text -> Text
formatDocLink url = "- " <> url

-- | Convert category to text
categoryToText :: Category -> Text
categoryToText = \case
  Performance -> "Performance"
  SpaceLeaks -> "Space Leaks"
  Security -> "Security"
  Safety -> "Safety"
  Style -> "Style"
  Correctness -> "Correctness"
  Modernization -> "Modernization"
  Imports -> "Imports"
  Naming -> "Naming"
  Extensions -> "Extensions"
  Complexity -> "Complexity"
  Concurrency -> "Concurrency"
  ErrorHandling -> "Error Handling"
  Documentation -> "Documentation"
  Redundant -> "Redundant"
  Custom t -> t

-- | Convert safety level to text
safetyLevelToText :: SafetyLevel -> Text
safetyLevelToText = \case
  Safe -> "always safe"
  MostlySafe -> "mostly safe"
  NeedsReview -> "needs review"
  Unsafe -> "unsafe"

--------------------------------------------------------------------------------
-- Rule Information Database
--------------------------------------------------------------------------------

-- | Information for avoid-head rule
avoidHeadInfo :: RuleInfo
avoidHeadInfo = RuleInfo
  { riRuleId = "avoid-head"
  , riRuleCode = Just "SAFETY-001"
  , riCategory = Safety
  , riShortDesc = "Avoid using 'head' - it fails on empty lists"
  , riLongDesc = "'head' is a partial function that throws an exception when called on an empty list. This can cause runtime crashes that are difficult to debug."
  , riRationale = "Partial functions violate type safety by hiding failure cases. Using total alternatives like \"headMay\" from the \"safe\" package makes errors explicit in the type system, catching bugs at compile time instead of runtime."
  , riExamples =
      [ RuleExample BadExample "firstElement xs = head xs  -- Crashes on empty list!" Nothing
      , RuleExample GoodExample "firstElement xs = headMay xs  -- Returns Nothing on empty list" (Just "Import from Data.Maybe.Safe or use pattern matching")
      , RuleExample GoodExample "firstElement (x:_) = Just x\nfirstElement [] = Nothing" (Just "Pattern matching makes the empty case explicit")
      ]
  , riRelatedRules = ["avoid-tail", "avoid-last", "avoid-init", "avoid-!!"]
  , riDocLinks =
      [ "https://wiki.haskell.org/Avoiding_partial_functions"
      , "https://hackage.haskell.org/package/safe"
      ]
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

-- | Information for avoid-tail rule
avoidTailInfo :: RuleInfo
avoidTailInfo = RuleInfo
  { riRuleId = "avoid-tail"
  , riRuleCode = Just "SAFETY-002"
  , riCategory = Safety
  , riShortDesc = "Avoid using 'tail' - it fails on empty lists"
  , riLongDesc = "'tail' is a partial function that throws an exception when called on an empty list. Like 'head', this can cause unexpected runtime failures."
  , riRationale = "Using 'tailMay' or pattern matching makes the possibility of an empty list explicit, preventing runtime crashes and making code more robust."
  , riExamples =
      [ RuleExample BadExample "restOfList xs = tail xs" Nothing
      , RuleExample GoodExample "restOfList xs = tailMay xs" (Just "Returns Nothing for empty lists")
      , RuleExample GoodExample "restOfList (_:xs) = Just xs\nrestOfList [] = Nothing" (Just "Explicit pattern matching")
      ]
  , riRelatedRules = ["avoid-head", "avoid-last", "avoid-init"]
  , riDocLinks = ["https://hackage.haskell.org/package/safe"]
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

-- | Information for avoid-fromJust rule
avoidFromJustInfo :: RuleInfo
avoidFromJustInfo = RuleInfo
  { riRuleId = "avoid-fromJust"
  , riRuleCode = Just "SAFETY-010"
  , riCategory = Safety
  , riShortDesc = "Avoid using 'fromJust' - it defeats the purpose of Maybe"
  , riLongDesc = "'fromJust' extracts a value from 'Maybe' by throwing an exception if it's 'Nothing'. This defeats the entire purpose of using 'Maybe' for safe error handling."
  , riRationale = "The 'Maybe' type exists to handle potential absence of values safely. Using 'fromJust' reintroduces the possibility of runtime crashes. Instead, handle 'Nothing' explicitly with pattern matching, 'fromMaybe', or monadic operations."
  , riExamples =
      [ RuleExample BadExample "value = fromJust maybeThing  -- Crashes if Nothing!" Nothing
      , RuleExample GoodExample "value = fromMaybe defaultVal maybeThing" (Just "Provide a default value")
      , RuleExample GoodExample "case maybeThing of\n  Just x -> useValue x\n  Nothing -> handleError" (Just "Handle both cases explicitly")
      ]
  , riRelatedRules = ["avoid-head", "avoid-error"]
  , riDocLinks = ["https://wiki.haskell.org/Avoiding_partial_functions"]
  , riSafetyLevel = Safe
  , riFixAvailable = False
  , riSince = Just "1.0.0"
  }

-- | Information for avoid-undefined rule
avoidUndefinedInfo :: RuleInfo
avoidUndefinedInfo = RuleInfo
  { riRuleId = "avoid-undefined"
  , riRuleCode = Just "SAFETY-020"
  , riCategory = Safety
  , riShortDesc = "Avoid 'undefined' - use typed holes or implement the function"
  , riLongDesc = "'undefined' allows you to compile code with incomplete implementations, but crashes at runtime when evaluated. While useful during development, it should never appear in production code."
  , riRationale = "Production code should handle all cases. During development, use typed holes (e.g., _todo) which provide better compiler error messages with type information. For impossible cases, consider using 'error' with a descriptive message or refactoring to make the case truly impossible through types."
  , riExamples =
      [ RuleExample BadExample "complexFunction x = undefined  -- TODO: implement later" Nothing
      , RuleExample GoodExample "complexFunction x = _  -- Typed hole shows what type is needed" (Just "Use typed holes during development")
      , RuleExample GoodExample "complexFunction x = error \"complexFunction: not yet implemented\"" (Just "At least provide context in the error message")
      ]
  , riRelatedRules = ["avoid-error"]
  , riDocLinks = ["https://wiki.haskell.org/Typed_hole"]
  , riSafetyLevel = Safe
  , riFixAvailable = False
  , riSince = Just "1.0.0"
  }

-- | Information for avoid-error rule
avoidErrorInfo :: RuleInfo
avoidErrorInfo = RuleInfo
  { riRuleId = "avoid-error"
  , riRuleCode = Just "SAFETY-021"
  , riCategory = Safety
  , riShortDesc = "Avoid 'error' - use proper error handling with Either or exceptions"
  , riLongDesc = "'error' terminates the program with a String message. While better than 'undefined', it still doesn't integrate with Haskell's type-based error handling."
  , riRationale = "Use 'Either', 'Maybe', or exception types to represent errors. This allows callers to handle errors appropriately and makes error cases visible in the type signature."
  , riExamples =
      [ RuleExample BadExample "divide :: Int -> Int -> Int\ndivide x 0 = error \"division by zero\"\ndivide x y = x `div` y" Nothing
      , RuleExample GoodExample "divide :: Int -> Int -> Either String Int\ndivide x 0 = Left \"division by zero\"\ndivide x y = Right (x `div` y)" (Just "Return Either to make errors explicit")
      ]
  , riRelatedRules = ["avoid-undefined", "avoid-fromJust"]
  , riDocLinks = ["https://wiki.haskell.org/Error_vs._Exception"]
  , riSafetyLevel = MostlySafe
  , riFixAvailable = False
  , riSince = Just "1.0.0"
  }

-- | Information for avoid-nub rule
avoidNubInfo :: RuleInfo
avoidNubInfo = RuleInfo
  { riRuleId = "avoid-nub"
  , riRuleCode = Just "PERF-001"
  , riCategory = Performance
  , riShortDesc = "Avoid 'nub' - it's O(n²), use 'ordNub' or 'nubOrd' instead"
  , riLongDesc = "The 'nub' function from Data.List has O(n²) complexity because it uses (==) for comparison. For larger lists, this can cause significant performance degradation."
  , riRationale = "If your type has an Ord instance, use 'ordNub' from Data.Containers.ListUtils or 'nubOrd' from the 'extra' package. These variants use Set internally and run in O(n log n) time. For even better performance with hashable types, consider 'nubOrd' from 'hashable'."
  , riExamples =
      [ RuleExample BadExample "unique = nub longList  -- O(n²) performance" Nothing
      , RuleExample GoodExample "import Data.Containers.ListUtils (nubOrd)\nunique = nubOrd longList  -- O(n log n)" (Just "Use nubOrd for Ord types")
      , RuleExample GoodExample "import Data.List.Extra (nubOrd)\nunique = nubOrd longList" (Just "Alternative from 'extra' package")
      ]
  , riRelatedRules = ["prefer-intset", "prefer-hashset"]
  , riDocLinks =
      [ "https://hackage.haskell.org/package/containers/docs/Data-Containers-ListUtils.html"
      , "https://wiki.haskell.org/Performance"
      ]
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

-- | Information for prefer-foldl' rule
preferFoldl'Info :: RuleInfo
preferFoldl'Info = RuleInfo
  { riRuleId = "prefer-foldl'"
  , riRuleCode = Just "SPACE-001"
  , riCategory = SpaceLeaks
  , riShortDesc = "Use 'foldl'' instead of 'foldl' to avoid space leaks"
  , riLongDesc = "'foldl' builds up thunks when accumulating results, potentially causing stack overflows or excessive memory usage. 'foldl'' (with a prime) is the strict version that evaluates the accumulator at each step."
  , riRationale = "Space leaks occur when lazy evaluation creates chains of unevaluated computations. Using strict variants forces evaluation at each step, preventing thunk buildup. This is especially important for numeric computations and large data processing."
  , riExamples =
      [ RuleExample BadExample "sum = foldl (+) 0  -- Builds up thunks: ((((0 + 1) + 2) + 3) + ...)" Nothing
      , RuleExample GoodExample "import Data.List (foldl')\nsum = foldl' (+) 0  -- Evaluates immediately: 6" (Just "Strict evaluation prevents space leaks")
      ]
  , riRelatedRules = ["strict-state", "strict-writer"]
  , riDocLinks =
      [ "https://wiki.haskell.org/Foldr_Foldl_Foldl'"
      , "https://www.well-typed.com/blog/2014/09/understanding-the-stack/"
      ]
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

-- | Information for avoid-length-eq-0 rule
avoidLengthEq0Info :: RuleInfo
avoidLengthEq0Info = RuleInfo
  { riRuleId = "avoid-length-eq-0"
  , riRuleCode = Just "PERF-010"
  , riCategory = Performance
  , riShortDesc = "Use 'null' instead of 'length == 0' - it's O(1) instead of O(n)"
  , riLongDesc = "Checking if 'length xs == 0' traverses the entire list to count elements, even though we only need to check if it's empty. The 'null' function is O(1) and more efficient."
  , riRationale = "This is a common performance anti-pattern. 'null' only needs to check if the list is empty (pattern match on []), while 'length' must traverse the entire list. For long lists, this difference is significant."
  , riExamples =
      [ RuleExample BadExample "if length xs == 0 then ... else ..." Nothing
      , RuleExample GoodExample "if null xs then ... else ..." (Just "O(1) instead of O(n)")
      , RuleExample BadExample "isEmpty = (== 0) . length" Nothing
      , RuleExample GoodExample "isEmpty = null" (Just "Much more efficient")
      ]
  , riRelatedRules = ["avoid-length-gt-0"]
  , riDocLinks = ["https://wiki.haskell.org/Performance/Accumulating_parameter"]
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

-- | Information for avoid-read rule
avoidReadInfo :: RuleInfo
avoidReadInfo = RuleInfo
  { riRuleId = "avoid-read"
  , riRuleCode = Just "SAFETY-030"
  , riCategory = Safety
  , riShortDesc = "Avoid 'read' - use 'readMaybe' or parser combinators"
  , riLongDesc = "'read' is a partial function that throws an exception when the string cannot be parsed. This makes it unsuitable for parsing untrusted input."
  , riRationale = "Use 'readMaybe' from Text.Read to get a 'Maybe' result that explicitly handles parse failure. For more complex parsing, use parser combinator libraries like megaparsec or attoparsec."
  , riExamples =
      [ RuleExample BadExample "number = read userInput :: Int  -- Crashes on invalid input" Nothing
      , RuleExample GoodExample "import Text.Read (readMaybe)\nnumber = readMaybe userInput :: Maybe Int" (Just "Returns Nothing on parse failure")
      , RuleExample GoodExample "case readMaybe userInput of\n  Just n -> processNumber n\n  Nothing -> showError \"Invalid number\"" (Just "Handle parse failure explicitly")
      ]
  , riRelatedRules = ["avoid-fromJust", "avoid-head"]
  , riDocLinks = ["https://hackage.haskell.org/package/base/docs/Text-Read.html#v:readMaybe"]
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

-- | Information for avoid-concat-map rule
avoidConcatMapInfo :: RuleInfo
avoidConcatMapInfo = RuleInfo
  { riRuleId = "avoid-concat-map"
  , riRuleCode = Just "PERF-020"
  , riCategory = Performance
  , riShortDesc = "Use 'concatMap' instead of 'concat . map' for better fusion"
  , riLongDesc = "The pattern 'concat . map' creates an intermediate list structure that is immediately deconstructed. 'concatMap' fuses these operations into a single pass."
  , riRationale = "GHC's rewrite rules can optimize 'concatMap' but may not optimize the composed version. Using 'concatMap' directly is more efficient and clearer in intent."
  , riExamples =
      [ RuleExample BadExample "result = concat (map processItem items)  -- Two passes over data" Nothing
      , RuleExample GoodExample "result = concatMap processItem items  -- Single pass, better fusion" Nothing
      ]
  , riRelatedRules = ["use-foldmap"]
  , riDocLinks = ["https://wiki.haskell.org/GHC_optimisations#Fusion"]
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

-- | Information for avoid-!! rule
avoidBangBangInfo :: RuleInfo
avoidBangBangInfo = RuleInfo
  { riRuleId = "avoid-!!"
  , riRuleCode = Just "SAFETY-003"
  , riCategory = Safety
  , riShortDesc = "Avoid using (!!) - it fails on out-of-bounds indices"
  , riLongDesc = "The (!!) operator is a partial function that throws an exception when the index is out of bounds. This is particularly dangerous because list indices are not checked at compile time."
  , riRationale = "Use 'atMay' from the 'safe' package, or use Vector/Array types that have better bounds checking. Lists are not designed for random access - consider using a Vector if you need efficient indexing."
  , riExamples =
      [ RuleExample BadExample "thirdItem = items !! 2  -- Crashes if list has < 3 elements" Nothing
      , RuleExample GoodExample "import Safe (atMay)\nthirdItem = items `atMay` 2" (Just "Returns Nothing for out-of-bounds")
      , RuleExample GoodExample "import qualified Data.Vector as V\nthirdItem = V.unsafeIndex vec 2" (Just "Use Vector for efficient indexing")
      ]
  , riRelatedRules = ["avoid-head", "prefer-vector"]
  , riDocLinks = ["https://hackage.haskell.org/package/safe"]
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

-- | Information for avoid-maximum rule
avoidMaximumInfo :: RuleInfo
avoidMaximumInfo = RuleInfo
  { riRuleId = "avoid-maximum"
  , riRuleCode = Just "SAFETY-004"
  , riCategory = Safety
  , riShortDesc = "Avoid 'maximum' - it fails on empty lists"
  , riLongDesc = "'maximum' throws an exception when called on an empty list. Like other partial list functions, this can cause unexpected runtime failures."
  , riRationale = "Use 'maximumMay' from the 'safe' package or use 'foldl1' with explicit empty case handling. Better yet, consider if your algorithm can guarantee non-empty lists at the type level using NonEmpty from Data.List.NonEmpty."
  , riExamples =
      [ RuleExample BadExample "max = maximum values  -- Crashes on empty list" Nothing
      , RuleExample GoodExample "import Safe (maximumMay)\nmax = maximumMay values" (Just "Returns Nothing for empty list")
      , RuleExample GoodExample "import Data.List.NonEmpty (NonEmpty, maximum)\nmax = maximum nonEmptyValues" (Just "Use NonEmpty to prove non-emptiness at compile time")
      ]
  , riRelatedRules = ["avoid-minimum", "avoid-head"]
  , riDocLinks = ["https://hackage.haskell.org/package/base/docs/Data-List-NonEmpty.html"]
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

-- | Information for prefer-intmap rule
preferIntMapInfo :: RuleInfo
preferIntMapInfo = RuleInfo
  { riRuleId = "prefer-intmap"
  , riRuleCode = Just "PERF-030"
  , riCategory = Performance
  , riShortDesc = "Use IntMap instead of Map Int for better performance"
  , riLongDesc = "When using Int keys, IntMap is significantly faster than Map Int because it uses a specialized Patricia tree structure optimized for integer keys."
  , riRationale = "IntMap operations are typically 2-5x faster than Map Int for lookups and insertions. The performance difference increases with map size. Only use Map Int if you need its specific features like arbitrary Ord keys."
  , riExamples =
      [ RuleExample BadExample "import qualified Data.Map as Map\nindexMap :: Map.Map Int String" Nothing
      , RuleExample GoodExample "import qualified Data.IntMap as IntMap\nindexMap :: IntMap.IntMap String" (Just "Specialized for Int keys")
      ]
  , riRelatedRules = ["prefer-intset", "prefer-hashmap"]
  , riDocLinks = ["https://hackage.haskell.org/package/containers/docs/Data-IntMap.html"]
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

-- | Information for avoid-head-sort rule
avoidHeadSortInfo :: RuleInfo
avoidHeadSortInfo = RuleInfo
  { riRuleId = "avoid-head-sort"
  , riRuleCode = Just "PERF-040"
  , riCategory = Performance
  , riShortDesc = "Don't use 'head . sort' to find minimum - use 'minimum' instead"
  , riLongDesc = "Using 'head . sort' to find the smallest element sorts the entire list (O(n log n)) when you only need to find the minimum (O(n))."
  , riRationale = "This is a common algorithmic mistake. Finding the minimum element requires a single pass through the data, while sorting requires much more work. Use 'minimum' for O(n) complexity."
  , riExamples =
      [ RuleExample BadExample "smallest = head (sort items)  -- O(n log n) + partial!" Nothing
      , RuleExample GoodExample "smallest = minimum items  -- O(n) but still partial" (Just "Better performance but still partial")
      , RuleExample GoodExample "import Safe (minimumMay)\nsmallest = minimumMay items" (Just "Best: O(n) and total")
      ]
  , riRelatedRules = ["avoid-last-sort", "avoid-maximum"]
  , riDocLinks = ["https://wiki.haskell.org/Performance"]
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

-- | Information for strict-state rule
strictStateInfo :: RuleInfo
strictStateInfo = RuleInfo
  { riRuleId = "strict-state"
  , riRuleCode = Just "SPACE-010"
  , riCategory = SpaceLeaks
  , riShortDesc = "Use strict State monad to avoid space leaks"
  , riLongDesc = "The lazy State monad can accumulate thunks in the state value, leading to space leaks. Use Control.Monad.State.Strict instead of Control.Monad.State.Lazy."
  , riRationale = "In most cases, you want strict state updates. Lazy state is rarely needed and can cause surprising memory behavior. The strict version evaluates the state at each step, preventing thunk buildup."
  , riExamples =
      [ RuleExample BadExample "import Control.Monad.State\n-- Uses lazy State by default" Nothing
      , RuleExample GoodExample "import Control.Monad.State.Strict\n-- Strict State prevents space leaks" (Just "Explicitly use the strict version")
      ]
  , riRelatedRules = ["prefer-foldl'", "strict-writer"]
  , riDocLinks =
      [ "https://www.well-typed.com/blog/2013/09/diagnosing-a-space-leak/"
      , "https://hackage.haskell.org/package/mtl/docs/Control-Monad-State-Strict.html"
      ]
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

--------------------------------------------------------------------------------
-- Additional Safety Rules
--------------------------------------------------------------------------------

avoidMinimumInfo :: RuleInfo
avoidMinimumInfo = RuleInfo
  { riRuleId = "avoid-minimum"
  , riRuleCode = Just "SAFETY-005"
  , riCategory = Safety
  , riShortDesc = "Avoid 'minimum' - it fails on empty lists"
  , riLongDesc = "'minimum' is a partial function that throws an exception when called on an empty list, similar to 'maximum'."
  , riRationale = "Use 'minimumMay' from the 'safe' package or use NonEmpty from Data.List.NonEmpty to enforce non-emptiness at the type level."
  , riExamples =
      [ RuleExample BadExample "smallest = minimum values" Nothing
      , RuleExample GoodExample "import Safe (minimumMay)\nsmallest = minimumMay values" (Just "Safe alternative")
      ]
  , riRelatedRules = ["avoid-maximum", "avoid-head"]
  , riDocLinks = ["https://hackage.haskell.org/package/safe"]
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

avoidInitInfo :: RuleInfo
avoidInitInfo = RuleInfo
  { riRuleId = "avoid-init"
  , riRuleCode = Just "SAFETY-006"
  , riCategory = Safety
  , riShortDesc = "Avoid 'init' - it fails on empty lists"
  , riLongDesc = "'init' returns all elements except the last, but throws an exception on empty lists."
  , riRationale = "Use 'initMay' from the 'safe' package or handle the empty case explicitly with pattern matching."
  , riExamples =
      [ RuleExample BadExample "allButLast = init values" Nothing
      , RuleExample GoodExample "import Safe (initMay)\nallButLast = initMay values" Nothing
      ]
  , riRelatedRules = ["avoid-tail", "avoid-last"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

avoidLastInfo :: RuleInfo
avoidLastInfo = RuleInfo
  { riRuleId = "avoid-last"
  , riRuleCode = Just "SAFETY-007"
  , riCategory = Safety
  , riShortDesc = "Avoid 'last' - it fails on empty lists"
  , riLongDesc = "'last' returns the last element of a list but throws an exception on empty lists."
  , riRationale = "Use 'lastMay' from the 'safe' package for safe extraction of the last element."
  , riExamples =
      [ RuleExample BadExample "finalElement = last items" Nothing
      , RuleExample GoodExample "import Safe (lastMay)\nfinalElement = lastMay items" Nothing
      ]
  , riRelatedRules = ["avoid-head", "avoid-init"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

avoidFoldr1Info :: RuleInfo
avoidFoldr1Info = RuleInfo
  { riRuleId = "avoid-foldr1"
  , riRuleCode = Just "SAFETY-008"
  , riCategory = Safety
  , riShortDesc = "Avoid 'foldr1' - it fails on empty lists"
  , riLongDesc = "'foldr1' is like 'foldr' but uses the last element as the initial value, making it partial."
  , riRationale = "Use 'foldr' with an explicit initial value to handle all cases safely."
  , riExamples =
      [ RuleExample BadExample "result = foldr1 combine items" Nothing
      , RuleExample GoodExample "result = foldr combine initialValue items" Nothing
      ]
  , riRelatedRules = ["avoid-foldl1"]
  , riDocLinks = []
  , riSafetyLevel = MostlySafe
  , riFixAvailable = False
  , riSince = Just "1.0.0"
  }

avoidFoldl1Info :: RuleInfo
avoidFoldl1Info = RuleInfo
  { riRuleId = "avoid-foldl1"
  , riRuleCode = Just "SAFETY-009"
  , riCategory = Safety
  , riShortDesc = "Avoid 'foldl1' - it fails on empty lists"
  , riLongDesc = "'foldl1' is the left-associative version of 'foldr1' and is also partial."
  , riRationale = "Use 'foldl'' with an explicit initial value for safety and better performance."
  , riExamples =
      [ RuleExample BadExample "total = foldl1 (+) numbers" Nothing
      , RuleExample GoodExample "import Data.List (foldl')\ntotal = foldl' (+) 0 numbers" Nothing
      ]
  , riRelatedRules = ["avoid-foldr1", "prefer-foldl'"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = False
  , riSince = Just "1.0.0"
  }

avoidSuccInfo :: RuleInfo
avoidSuccInfo = RuleInfo
  { riRuleId = "avoid-succ"
  , riRuleCode = Just "SAFETY-011"
  , riCategory = Safety
  , riShortDesc = "Avoid 'succ' on maxBound - it throws an exception"
  , riLongDesc = "'succ' increments an Enum value but throws an exception at maxBound."
  , riRationale = "Add explicit bounds checking or use safe alternatives for bounded types."
  , riExamples =
      [ RuleExample BadExample "next = succ maxBound  -- Exception!" Nothing
      , RuleExample GoodExample "next = if x == maxBound then Nothing else Just (succ x)" Nothing
      ]
  , riRelatedRules = ["avoid-pred", "avoid-toEnum"]
  , riDocLinks = []
  , riSafetyLevel = MostlySafe
  , riFixAvailable = False
  , riSince = Just "1.0.0"
  }

avoidPredInfo :: RuleInfo
avoidPredInfo = RuleInfo
  { riRuleId = "avoid-pred"
  , riRuleCode = Just "SAFETY-012"
  , riCategory = Safety
  , riShortDesc = "Avoid 'pred' on minBound - it throws an exception"
  , riLongDesc = "'pred' decrements an Enum value but throws an exception at minBound."
  , riRationale = "Add explicit bounds checking for safe decrement operations."
  , riExamples =
      [ RuleExample BadExample "prev = pred minBound  -- Exception!" Nothing
      , RuleExample GoodExample "prev = if x == minBound then Nothing else Just (pred x)" Nothing
      ]
  , riRelatedRules = ["avoid-succ"]
  , riDocLinks = []
  , riSafetyLevel = MostlySafe
  , riFixAvailable = False
  , riSince = Just "1.0.0"
  }

avoidToEnumInfo :: RuleInfo
avoidToEnumInfo = RuleInfo
  { riRuleId = "avoid-toEnum"
  , riRuleCode = Just "SAFETY-013"
  , riCategory = Safety
  , riShortDesc = "Avoid 'toEnum' - it can throw exceptions for out-of-range values"
  , riLongDesc = "'toEnum' converts an Int to an Enum value but throws an exception for invalid values."
  , riRationale = "Use bounds checking before calling toEnum or use safer conversion methods."
  , riExamples =
      [ RuleExample BadExample "value = toEnum n  -- May throw!" Nothing
      , RuleExample GoodExample "value = if n >= fromEnum (minBound :: MyEnum) && n <= fromEnum (maxBound :: MyEnum)\n        then Just (toEnum n)\n        else Nothing" Nothing
      ]
  , riRelatedRules = ["avoid-succ", "avoid-pred"]
  , riDocLinks = []
  , riSafetyLevel = MostlySafe
  , riFixAvailable = False
  , riSince = Just "1.0.0"
  }

avoidCycleInfo :: RuleInfo
avoidCycleInfo = RuleInfo
  { riRuleId = "avoid-cycle"
  , riRuleCode = Just "SAFETY-014"
  , riCategory = Safety
  , riShortDesc = "Avoid 'cycle' on empty lists - it creates an infinite loop"
  , riLongDesc = "'cycle' creates an infinite list by repeating its input, but fails on empty lists."
  , riRationale = "Ensure the input to 'cycle' is non-empty or use NonEmpty from Data.List.NonEmpty."
  , riExamples =
      [ RuleExample BadExample "repeated = cycle []  -- Infinite loop!" Nothing
      , RuleExample GoodExample "import Data.List.NonEmpty (NonEmpty, cycle)\nrepeated = cycle nonEmptyList" Nothing
      ]
  , riRelatedRules = ["avoid-repeat"]
  , riDocLinks = []
  , riSafetyLevel = MostlySafe
  , riFixAvailable = False
  , riSince = Just "1.0.0"
  }

--------------------------------------------------------------------------------
-- Additional Performance Rules
--------------------------------------------------------------------------------

avoidReverseAppendInfo :: RuleInfo
avoidReverseAppendInfo = RuleInfo
  { riRuleId = "avoid-reverse-append"
  , riRuleCode = Just "PERF-011"
  , riCategory = Performance
  , riShortDesc = "Avoid 'reverse' followed by '++'  - use difference lists"
  , riLongDesc = "Reversing a list and then appending is inefficient when done repeatedly in a loop."
  , riRationale = "Use difference lists (DList) or accumulate in the correct order to avoid O(n²) complexity."
  , riExamples =
      [ RuleExample BadExample "buildList acc x = reverse (x : reverse acc)" Nothing
      , RuleExample GoodExample "import Data.DList qualified as DL\nbuildList acc x = DL.snoc acc x" Nothing
      ]
  , riRelatedRules = ["use-dlist"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = False
  , riSince = Just "1.0.0"
  }

useDListInfo :: RuleInfo
useDListInfo = RuleInfo
  { riRuleId = "use-dlist"
  , riRuleCode = Just "PERF-012"
  , riCategory = Performance
  , riShortDesc = "Use DList for O(1) append operations"
  , riLongDesc = "Repeated list concatenation with '++' is O(n) per operation. DList provides O(1) append."
  , riRationale = "When building lists through repeated concatenation, DList can dramatically improve performance."
  , riExamples =
      [ RuleExample BadExample "result = foldl (++) [] lists  -- O(n²)" Nothing
      , RuleExample GoodExample "import Data.DList qualified as DL\nresult = DL.toList $ foldl (<>) DL.empty (map DL.fromList lists)" Nothing
      ]
  , riRelatedRules = ["avoid-reverse-append"]
  , riDocLinks = ["https://hackage.haskell.org/package/dlist"]
  , riSafetyLevel = Safe
  , riFixAvailable = False
  , riSince = Just "1.0.0"
  }

avoidElemInfo :: RuleInfo
avoidElemInfo = RuleInfo
  { riRuleId = "avoid-elem"
  , riRuleCode = Just "PERF-013"
  , riCategory = Performance
  , riShortDesc = "Use Set for repeated membership tests instead of 'elem'"
  , riLongDesc = "'elem' has O(n) complexity. For repeated membership tests, convert to Set for O(log n) lookup."
  , riRationale = "If you're checking membership multiple times, the cost of converting to a Set is amortized."
  , riExamples =
      [ RuleExample BadExample "filtered = filter (`elem` largeList) items" Nothing
      , RuleExample GoodExample "import Data.Set qualified as Set\nlet largeSet = Set.fromList largeList\nfiltered = filter (`Set.member` largeSet) items" Nothing
      ]
  , riRelatedRules = ["prefer-intset"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = False
  , riSince = Just "1.0.0"
  }

useTextNotStringInfo :: RuleInfo
useTextNotStringInfo = RuleInfo
  { riRuleId = "use-text-not-string"
  , riRuleCode = Just "PERF-014"
  , riCategory = Performance
  , riShortDesc = "Use Text instead of String for better performance"
  , riLongDesc = "String is just [Char], which is inefficient for text processing. Text is optimized for Unicode text."
  , riRationale = "Text provides better memory usage and faster operations for text data."
  , riExamples =
      [ RuleExample BadExample "processText :: String -> String" Nothing
      , RuleExample GoodExample "import Data.Text (Text)\nprocessText :: Text -> Text" Nothing
      ]
  , riRelatedRules = []
  , riDocLinks = ["https://hackage.haskell.org/package/text"]
  , riSafetyLevel = MostlySafe
  , riFixAvailable = False
  , riSince = Just "1.0.0"
  }

--------------------------------------------------------------------------------
-- Space Leak Rules
--------------------------------------------------------------------------------

strictFieldsInfo :: RuleInfo
strictFieldsInfo = RuleInfo
  { riRuleId = "strict-fields"
  , riRuleCode = Just "SPACE-011"
  , riCategory = SpaceLeaks
  , riShortDesc = "Use strict fields in data types to avoid space leaks"
  , riLongDesc = "Lazy fields can accumulate thunks, causing space leaks in long-running programs."
  , riRationale = "Mark fields as strict with '!' or use the StrictData extension to evaluate fields immediately."
  , riExamples =
      [ RuleExample BadExample "data Counter = Counter { count :: Int }" Nothing
      , RuleExample GoodExample "data Counter = Counter { count :: !Int }" (Just "Strict field")
      ]
  , riRelatedRules = ["prefer-foldl'", "strict-state"]
  , riDocLinks = []
  , riSafetyLevel = MostlySafe
  , riFixAvailable = False
  , riSince = Just "1.0.0"
  }

avoidLazyIOInfo :: RuleInfo
avoidLazyIOInfo = RuleInfo
  { riRuleId = "avoid-lazy-io"
  , riRuleCode = Just "SPACE-012"
  , riCategory = SpaceLeaks
  , riShortDesc = "Avoid lazy IO - use strict IO or streaming libraries"
  , riLongDesc = "Lazy IO can cause file handle leaks and unpredictable resource usage."
  , riRationale = "Use strict IO operations or streaming libraries like conduit or streaming for predictable resource management."
  , riExamples =
      [ RuleExample BadExample "contents <- readFile path  -- Lazy!" Nothing
      , RuleExample GoodExample "import Data.Text.IO qualified as T\ncontents <- T.readFile path  -- Strict" Nothing
      ]
  , riRelatedRules = []
  , riDocLinks = ["https://wiki.haskell.org/Lazy_I/O"]
  , riSafetyLevel = Safe
  , riFixAvailable = False
  , riSince = Just "1.0.0"
  }

--------------------------------------------------------------------------------
-- Modernization Rules
--------------------------------------------------------------------------------

useFmapInfo :: RuleInfo
useFmapInfo = RuleInfo
  { riRuleId = "use-fmap"
  , riRuleCode = Just "MODERN-001"
  , riCategory = Modernization
  , riShortDesc = "Use 'fmap' or '<$>' instead of 'liftM'"
  , riLongDesc = "'liftM' is a legacy function from when Monad didn't have a Functor superclass."
  , riRationale = "'fmap' is more general and works for any Functor, not just Monads."
  , riExamples =
      [ RuleExample BadExample "result <- liftM f action" Nothing
      , RuleExample GoodExample "result <- fmap f action\n-- or:\nresult <- f <$> action" Nothing
      ]
  , riRelatedRules = ["use-traverse"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

useTraverseInfo :: RuleInfo
useTraverseInfo = RuleInfo
  { riRuleId = "use-traverse"
  , riRuleCode = Just "MODERN-002"
  , riCategory = Modernization
  , riShortDesc = "Use 'traverse' instead of 'mapM'"
  , riLongDesc = "'mapM' is a legacy function; 'traverse' is more general and works for any Traversable."
  , riRationale = "'traverse' is the modern, more general version that works with Applicative, not just Monad."
  , riExamples =
      [ RuleExample BadExample "results <- mapM process items" Nothing
      , RuleExample GoodExample "results <- traverse process items" Nothing
      ]
  , riRelatedRules = ["use-fmap", "use-traverse-underscore"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

useVoidInfo :: RuleInfo
useVoidInfo = RuleInfo
  { riRuleId = "use-void"
  , riRuleCode = Just "MODERN-003"
  , riCategory = Modernization
  , riShortDesc = "Use 'void' instead of '>> return ()'"
  , riLongDesc = "The 'void' function clearly expresses the intent to discard a result."
  , riRationale = "'void' is more explicit and shorter than '>> return ()'."
  , riExamples =
      [ RuleExample BadExample "action >> return ()" Nothing
      , RuleExample GoodExample "import Control.Monad (void)\nvoid action" Nothing
      ]
  , riRelatedRules = []
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

useWhenInfo :: RuleInfo
useWhenInfo = RuleInfo
  { riRuleId = "use-when"
  , riRuleCode = Just "MODERN-004"
  , riCategory = Modernization
  , riShortDesc = "Use 'when' instead of 'if ... then ... else return ()'"
  , riLongDesc = "The 'when' combinator clearly expresses conditional actions in monadic code."
  , riRationale = "'when' is more concise and clearer in intent than an if expression."
  , riExamples =
      [ RuleExample BadExample "if condition then action else return ()" Nothing
      , RuleExample GoodExample "import Control.Monad (when)\nwhen condition action" Nothing
      ]
  , riRelatedRules = ["use-unless"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

useUnlessInfo :: RuleInfo
useUnlessInfo = RuleInfo
  { riRuleId = "use-unless"
  , riRuleCode = Just "MODERN-005"
  , riCategory = Modernization
  , riShortDesc = "Use 'unless' instead of 'if not ... then ... else return ()'"
  , riLongDesc = "The 'unless' combinator is clearer than 'if not' for conditional actions."
  , riRationale = "'unless' avoids the double negative of 'if not', making code easier to read."
  , riExamples =
      [ RuleExample BadExample "if not condition then action else return ()" Nothing
      , RuleExample GoodExample "import Control.Monad (unless)\nunless condition action" Nothing
      ]
  , riRelatedRules = ["use-when"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

useLambdaCaseInfo :: RuleInfo
useLambdaCaseInfo = RuleInfo
  { riRuleId = "use-lambda-case"
  , riRuleCode = Just "MODERN-006"
  , riCategory = Modernization
  , riShortDesc = "Use '\\case' instead of '\\x -> case x of'"
  , riLongDesc = "The LambdaCase extension provides cleaner syntax for lambda expressions that immediately case on their argument."
  , riRationale = "\\case avoids naming the intermediate variable, making the code more concise."
  , riExamples =
      [ RuleExample BadExample "map (\\x -> case x of\n  Just y -> y\n  Nothing -> 0) items" Nothing
      , RuleExample GoodExample "{-# LANGUAGE LambdaCase #-}\nmap (\\case\n  Just y -> y\n  Nothing -> 0) items" Nothing
      ]
  , riRelatedRules = ["use-tuple-section"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

useTupleSectionInfo :: RuleInfo
useTupleSectionInfo = RuleInfo
  { riRuleId = "use-tuple-section"
  , riRuleCode = Just "MODERN-007"
  , riCategory = Modernization
  , riShortDesc = "Use tuple sections instead of lambdas"
  , riLongDesc = "The TupleSections extension allows partial application of tuple constructors."
  , riRationale = "Tuple sections are more concise than lambdas for creating tuples."
  , riExamples =
      [ RuleExample BadExample "map (\\x -> (x, 0)) items" Nothing
      , RuleExample GoodExample "{-# LANGUAGE TupleSections #-}\nmap (, 0) items" Nothing
      ]
  , riRelatedRules = []
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

--------------------------------------------------------------------------------
-- List Rules
--------------------------------------------------------------------------------

useNullInfo :: RuleInfo
useNullInfo = RuleInfo
  { riRuleId = "use-null"
  , riRuleCode = Just "STYLE-001"
  , riCategory = Style
  , riShortDesc = "Use 'null' to check for empty lists"
  , riLongDesc = "Comparing to [] or using length is less clear and less efficient than 'null'."
  , riRationale = "'null' is O(1), clearer in intent, and works for all Foldable types."
  , riExamples =
      [ RuleExample BadExample "if xs == [] then ..." Nothing
      , RuleExample GoodExample "if null xs then ..." Nothing
      ]
  , riRelatedRules = ["avoid-length-eq-0"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

useConcatInfo :: RuleInfo
useConcatInfo = RuleInfo
  { riRuleId = "use-concat"
  , riRuleCode = Just "STYLE-002"
  , riCategory = Style
  , riShortDesc = "Use 'concat' instead of 'foldr (++) []'"
  , riLongDesc = "'concat' is the standard function for flattening a list of lists."
  , riRationale = "'concat' is clearer and may be optimized by GHC."
  , riExamples =
      [ RuleExample BadExample "combined = foldr (++) [] lists" Nothing
      , RuleExample GoodExample "combined = concat lists" Nothing
      ]
  , riRelatedRules = ["use-intercalate"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

useIntercalateInfo :: RuleInfo
useIntercalateInfo = RuleInfo
  { riRuleId = "use-intercalate"
  , riRuleCode = Just "STYLE-003"
  , riCategory = Style
  , riShortDesc = "Use 'intercalate' to join lists with a separator"
  , riLongDesc = "'intercalate' is the standard way to join lists with a separator."
  , riRationale = "'intercalate' is clearer than manually interspersing and concatenating."
  , riExamples =
      [ RuleExample BadExample "concat $ intersperse \",\" items" Nothing
      , RuleExample GoodExample "import Data.List (intercalate)\nintercalate \",\" items" Nothing
      ]
  , riRelatedRules = ["use-concat"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

--------------------------------------------------------------------------------
-- Maybe Rules
--------------------------------------------------------------------------------

useMaybeInfo :: RuleInfo
useMaybeInfo = RuleInfo
  { riRuleId = "use-maybe"
  , riRuleCode = Just "STYLE-010"
  , riCategory = Style
  , riShortDesc = "Use 'maybe' function for case analysis on Maybe"
  , riLongDesc = "The 'maybe' function provides a cleaner way to handle Maybe values."
  , riRationale = "'maybe' is more concise and point-free friendly than explicit case expressions."
  , riExamples =
      [ RuleExample BadExample "case mx of\n  Nothing -> defaultValue\n  Just x -> f x" Nothing
      , RuleExample GoodExample "maybe defaultValue f mx" Nothing
      ]
  , riRelatedRules = ["use-from-maybe"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

useFromMaybeInfo :: RuleInfo
useFromMaybeInfo = RuleInfo
  { riRuleId = "use-from-maybe"
  , riRuleCode = Just "STYLE-011"
  , riCategory = Style
  , riShortDesc = "Use 'fromMaybe' to extract with a default value"
  , riLongDesc = "'fromMaybe' is the standard way to provide a default for Maybe."
  , riRationale = "'fromMaybe' is clearer than 'maybe defaultValue id'."
  , riExamples =
      [ RuleExample BadExample "maybe defaultValue id mx" Nothing
      , RuleExample GoodExample "import Data.Maybe (fromMaybe)\nfromMaybe defaultValue mx" Nothing
      ]
  , riRelatedRules = ["use-maybe"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

useMapMaybeInfo :: RuleInfo
useMapMaybeInfo = RuleInfo
  { riRuleId = "use-map-maybe"
  , riRuleCode = Just "STYLE-012"
  , riCategory = Style
  , riShortDesc = "Use 'mapMaybe' instead of 'catMaybes . map'"
  , riLongDesc = "'mapMaybe' fuses mapping and filtering into a single operation."
  , riRationale = "'mapMaybe' is more efficient and clearer in intent."
  , riExamples =
      [ RuleExample BadExample "results = catMaybes $ map f items" Nothing
      , RuleExample GoodExample "import Data.Maybe (mapMaybe)\nresults = mapMaybe f items" Nothing
      ]
  , riRelatedRules = []
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

useIsJustInfo :: RuleInfo
useIsJustInfo = RuleInfo
  { riRuleId = "use-is-just"
  , riRuleCode = Just "STYLE-013"
  , riCategory = Style
  , riShortDesc = "Use 'isJust' to check if Maybe is Just"
  , riLongDesc = "'isJust' is the standard function for checking if a Maybe is Just."
  , riRationale = "'isJust' is clearer than pattern matching just to get a boolean."
  , riExamples =
      [ RuleExample BadExample "case mx of { Just _ -> True; Nothing -> False }" Nothing
      , RuleExample GoodExample "import Data.Maybe (isJust)\nisJust mx" Nothing
      ]
  , riRelatedRules = ["use-is-nothing"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

useIsNothingInfo :: RuleInfo
useIsNothingInfo = RuleInfo
  { riRuleId = "use-is-nothing"
  , riRuleCode = Just "STYLE-014"
  , riCategory = Style
  , riShortDesc = "Use 'isNothing' to check if Maybe is Nothing"
  , riLongDesc = "'isNothing' is the standard function for checking if a Maybe is Nothing."
  , riRationale = "'isNothing' is clearer than pattern matching or 'not . isJust'."
  , riExamples =
      [ RuleExample BadExample "case mx of { Nothing -> True; Just _ -> False }" Nothing
      , RuleExample GoodExample "import Data.Maybe (isNothing)\nisNothing mx" Nothing
      ]
  , riRelatedRules = ["use-is-just"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

--------------------------------------------------------------------------------
-- Either Rules
--------------------------------------------------------------------------------

useEitherInfo :: RuleInfo
useEitherInfo = RuleInfo
  { riRuleId = "use-either"
  , riRuleCode = Just "STYLE-020"
  , riCategory = Style
  , riShortDesc = "Use 'either' function for case analysis on Either"
  , riLongDesc = "The 'either' function provides a cleaner way to handle Either values."
  , riRationale = "'either' is more concise than explicit case expressions."
  , riExamples =
      [ RuleExample BadExample "case result of\n  Left e -> handleError e\n  Right v -> handleSuccess v" Nothing
      , RuleExample GoodExample "either handleError handleSuccess result" Nothing
      ]
  , riRelatedRules = []
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

useLeftsInfo :: RuleInfo
useLeftsInfo = RuleInfo
  { riRuleId = "use-lefts"
  , riRuleCode = Just "STYLE-021"
  , riCategory = Style
  , riShortDesc = "Use 'lefts' to extract Left values"
  , riLongDesc = "'lefts' is the standard function for extracting all Left values from a list."
  , riRationale = "'lefts' is clearer than list comprehension."
  , riExamples =
      [ RuleExample BadExample "[e | Left e <- eithers]" Nothing
      , RuleExample GoodExample "import Data.Either (lefts)\nlefts eithers" Nothing
      ]
  , riRelatedRules = ["use-rights"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

useRightsInfo :: RuleInfo
useRightsInfo = RuleInfo
  { riRuleId = "use-rights"
  , riRuleCode = Just "STYLE-022"
  , riCategory = Style
  , riShortDesc = "Use 'rights' to extract Right values"
  , riLongDesc = "'rights' is the standard function for extracting all Right values from a list."
  , riRationale = "'rights' is clearer than list comprehension."
  , riExamples =
      [ RuleExample BadExample "[v | Right v <- eithers]" Nothing
      , RuleExample GoodExample "import Data.Either (rights)\nrights eithers" Nothing
      ]
  , riRelatedRules = ["use-lefts"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

--------------------------------------------------------------------------------
-- Boolean Rules
--------------------------------------------------------------------------------

useBoolInfo :: RuleInfo
useBoolInfo = RuleInfo
  { riRuleId = "use-bool"
  , riRuleCode = Just "STYLE-030"
  , riCategory = Style
  , riShortDesc = "Simplify boolean expressions"
  , riLongDesc = "Avoid using 'if' just to return boolean literals."
  , riRationale = "Direct boolean expressions are clearer and more concise."
  , riExamples =
      [ RuleExample BadExample "if condition then True else False" Nothing
      , RuleExample GoodExample "condition" Nothing
      ]
  , riRelatedRules = ["simplify-not", "redundant-if"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

simplifyNotInfo :: RuleInfo
simplifyNotInfo = RuleInfo
  { riRuleId = "simplify-not"
  , riRuleCode = Just "STYLE-031"
  , riCategory = Style
  , riShortDesc = "Remove double negation"
  , riLongDesc = "Double negation is confusing and unnecessary."
  , riRationale = "Simplify 'not (not x)' to just 'x'."
  , riExamples =
      [ RuleExample BadExample "not (not x)" Nothing
      , RuleExample GoodExample "x" Nothing
      ]
  , riRelatedRules = ["use-bool"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

redundantIfInfo :: RuleInfo
redundantIfInfo = RuleInfo
  { riRuleId = "redundant-if"
  , riRuleCode = Just "STYLE-032"
  , riCategory = Style
  , riShortDesc = "Remove redundant if-then-else"
  , riLongDesc = "If expressions that return the condition itself are redundant."
  , riRationale = "Return the condition directly for clarity."
  , riExamples =
      [ RuleExample BadExample "if x == 0 then True else False" Nothing
      , RuleExample GoodExample "x == 0" Nothing
      ]
  , riRelatedRules = ["use-bool"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

--------------------------------------------------------------------------------
-- Monadic Rules
--------------------------------------------------------------------------------

useApplicativeInfo :: RuleInfo
useApplicativeInfo = RuleInfo
  { riRuleId = "use-applicative"
  , riRuleCode = Just "STYLE-040"
  , riCategory = Modernization
  , riShortDesc = "Use applicative style instead of monadic do-notation"
  , riLongDesc = "When computations don't depend on each other, applicative style is clearer."
  , riRationale = "Applicative style shows independence and may enable more optimization."
  , riExamples =
      [ RuleExample BadExample "do\n  x <- action1\n  y <- action2\n  return (f x y)" Nothing
      , RuleExample GoodExample "f <$> action1 <*> action2" Nothing
      ]
  , riRelatedRules = []
  , riDocLinks = ["https://wiki.haskell.org/Applicative_functor"]
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

useForMInfo :: RuleInfo
useForMInfo = RuleInfo
  { riRuleId = "use-for-m"
  , riRuleCode = Just "STYLE-041"
  , riCategory = Style
  , riShortDesc = "Use 'forM' for clearer iteration"
  , riLongDesc = "'forM' is 'mapM' with flipped arguments, making the data come first."
  , riRationale = "'forM' is often clearer when the computation is the focus."
  , riExamples =
      [ RuleExample BadExample "mapM (\\x -> ...) items" Nothing
      , RuleExample GoodExample "import Control.Monad (forM)\nforM items $ \\x -> ..." Nothing
      ]
  , riRelatedRules = ["use-traverse"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

useTraverseUnderscoreInfo :: RuleInfo
useTraverseUnderscoreInfo = RuleInfo
  { riRuleId = "use-traverse-underscore"
  , riRuleCode = Just "STYLE-042"
  , riCategory = Modernization
  , riShortDesc = "Use @traverse_@ instead of @mapM_@"
  , riLongDesc = "@traverse_@ is the modern, more general version of @mapM_@."
  , riRationale = "@traverse_@ works for any Traversable, not just lists."
  , riExamples =
      [ RuleExample BadExample "mapM_ print items" Nothing
      , RuleExample GoodExample "traverse_ print items" Nothing
      ]
  , riRelatedRules = ["use-traverse"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

avoidSequenceInfo :: RuleInfo
avoidSequenceInfo = RuleInfo
  { riRuleId = "avoid-sequence"
  , riRuleCode = Just "STYLE-043"
  , riCategory = Style
  , riShortDesc = "Use 'traverse' instead of 'sequence . map'"
  , riLongDesc = "'traverse' fuses the map and sequence operations."
  , riRationale = "'traverse' is more efficient and clearer."
  , riExamples =
      [ RuleExample BadExample "results <- sequence (map f items)" Nothing
      , RuleExample GoodExample "results <- traverse f items" Nothing
      ]
  , riRelatedRules = ["use-traverse"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

--------------------------------------------------------------------------------
-- String/Text Rules
--------------------------------------------------------------------------------

useTextPackInfo :: RuleInfo
useTextPackInfo = RuleInfo
  { riRuleId = "use-text-pack"
  , riRuleCode = Just "PERF-050"
  , riCategory = Performance
  , riShortDesc = "Use OverloadedStrings extension instead of explicit T.pack"
  , riLongDesc = "The OverloadedStrings extension allows string literals to be Text automatically."
  , riRationale = "OverloadedStrings is cleaner and less error-prone than manual packing."
  , riExamples =
      [ RuleExample BadExample "msg = T.pack \"hello\"" Nothing
      , RuleExample GoodExample "{-# LANGUAGE OverloadedStrings #-}\nmsg = \"hello\" :: Text" Nothing
      ]
  , riRelatedRules = []
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

useTextUnpackInfo :: RuleInfo
useTextUnpackInfo = RuleInfo
  { riRuleId = "use-text-unpack"
  , riRuleCode = Just "PERF-051"
  , riCategory = Performance
  , riShortDesc = "Avoid unpacking Text to String"
  , riLongDesc = "Converting Text to String defeats the performance benefits of Text."
  , riRationale = "Keep data as Text and use Text-based functions."
  , riExamples =
      [ RuleExample BadExample "length $ T.unpack text" Nothing
      , RuleExample GoodExample "T.length text" Nothing
      ]
  , riRelatedRules = ["use-text-not-string"]
  , riDocLinks = []
  , riSafetyLevel = MostlySafe
  , riFixAvailable = False
  , riSince = Just "1.0.0"
  }

--------------------------------------------------------------------------------
-- Numeric Rules
--------------------------------------------------------------------------------

useDivModInfo :: RuleInfo
useDivModInfo = RuleInfo
  { riRuleId = "use-div-mod"
  , riRuleCode = Just "PERF-060"
  , riCategory = Performance
  , riShortDesc = "Use 'divMod' to compute both division and modulus"
  , riLongDesc = "Computing 'div' and 'mod' separately is less efficient than 'divMod'."
  , riRationale = "'divMod' computes both values in a single operation."
  , riExamples =
      [ RuleExample BadExample "let d = x `div` y\n    m = x `mod` y" Nothing
      , RuleExample GoodExample "let (d, m) = x `divMod` y" Nothing
      ]
  , riRelatedRules = ["use-quot-rem"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

useQuotRemInfo :: RuleInfo
useQuotRemInfo = RuleInfo
  { riRuleId = "use-quot-rem"
  , riRuleCode = Just "PERF-061"
  , riCategory = Performance
  , riShortDesc = "Use 'quotRem' to compute both quotient and remainder"
  , riLongDesc = "Computing 'quot' and 'rem' separately is less efficient than 'quotRem'."
  , riRationale = "'quotRem' computes both values in a single operation."
  , riExamples =
      [ RuleExample BadExample "let q = x `quot` y\n    r = x `rem` y" Nothing
      , RuleExample GoodExample "let (q, r) = x `quotRem` y" Nothing
      ]
  , riRelatedRules = ["use-div-mod"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

--------------------------------------------------------------------------------
-- Import Rules
--------------------------------------------------------------------------------

unusedImportInfo :: RuleInfo
unusedImportInfo = RuleInfo
  { riRuleId = "unused-import"
  , riRuleCode = Just "IMPORT-001"
  , riCategory = Imports
  , riShortDesc = "Remove unused import"
  , riLongDesc = "This import statement brings in symbols that are never used."
  , riRationale = "Remove unused imports to keep the code clean and compilation faster."
  , riExamples =
      [ RuleExample BadExample "import Data.Map (Map)  -- Map not used" Nothing
      , RuleExample GoodExample "-- Import removed" Nothing
      ]
  , riRelatedRules = ["redundant-import"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

redundantImportInfo :: RuleInfo
redundantImportInfo = RuleInfo
  { riRuleId = "redundant-import"
  , riRuleCode = Just "IMPORT-002"
  , riCategory = Imports
  , riShortDesc = "Combine redundant imports"
  , riLongDesc = "Multiple imports from the same module can be combined into one."
  , riRationale = "Combining imports makes the code cleaner and easier to maintain."
  , riExamples =
      [ RuleExample BadExample "import Data.List (sort)\nimport Data.List (nub)" Nothing
      , RuleExample GoodExample "import Data.List (sort, nub)" Nothing
      ]
  , riRelatedRules = ["unused-import"]
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

qualifyPostInfo :: RuleInfo
qualifyPostInfo = RuleInfo
  { riRuleId = "qualify-post"
  , riRuleCode = Just "IMPORT-003"
  , riCategory = Modernization
  , riShortDesc = "Use ImportQualifiedPost extension"
  , riLongDesc = "Post-qualified imports put 'qualified' after the module name, improving readability."
  , riRationale = "Post-qualified imports align better and are easier to scan."
  , riExamples =
      [ RuleExample BadExample "import qualified Data.Map as Map" Nothing
      , RuleExample GoodExample "{-# LANGUAGE ImportQualifiedPost #-}\nimport Data.Map qualified as Map" Nothing
      ]
  , riRelatedRules = []
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

--------------------------------------------------------------------------------
-- Lens Rules
--------------------------------------------------------------------------------

useLensViewInfo :: RuleInfo
useLensViewInfo = RuleInfo
  { riRuleId = "use-lens-view"
  , riRuleCode = Just "LENS-001"
  , riCategory = Style
  , riShortDesc = "Use 'view' for clarity"
  , riLongDesc = "In complex lens expressions, 'view' is often clearer than (^.)."
  , riRationale = "'view' makes the intent more explicit in long expressions."
  , riExamples =
      [ RuleExample BadExample "let x = obj ^. field1 ^. field2" Nothing
      , RuleExample GoodExample "let x = view (field1 . field2) obj" Nothing
      ]
  , riRelatedRules = []
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

useLensSetInfo :: RuleInfo
useLensSetInfo = RuleInfo
  { riRuleId = "use-lens-set"
  , riRuleCode = Just "LENS-002"
  , riCategory = Style
  , riShortDesc = "Use 'set' for clarity"
  , riLongDesc = "The 'set' function is often clearer than (.~) in complex expressions."
  , riRationale = "'set' makes the operation more explicit."
  , riExamples =
      [ RuleExample BadExample "let obj' = obj & field .~ value" Nothing
      , RuleExample GoodExample "let obj' = set field value obj" Nothing
      ]
  , riRelatedRules = []
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }

useLensOverInfo :: RuleInfo
useLensOverInfo = RuleInfo
  { riRuleId = "use-lens-over"
  , riRuleCode = Just "LENS-003"
  , riCategory = Style
  , riShortDesc = "Use 'over' for clarity"
  , riLongDesc = "The 'over' function is often clearer than (%~) in complex expressions."
  , riRationale = "'over' makes the modification more explicit."
  , riExamples =
      [ RuleExample BadExample "let obj' = obj & field %~ f" Nothing
      , RuleExample GoodExample "let obj' = over field f obj" Nothing
      ]
  , riRelatedRules = []
  , riDocLinks = []
  , riSafetyLevel = Safe
  , riFixAvailable = True
  , riSince = Just "1.0.0"
  }
