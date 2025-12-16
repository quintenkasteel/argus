{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Argus.Rules.Modernize
-- Description : Modernization suggestions for Haskell code
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module detects opportunities to modernize Haskell code by:
-- - Preferring Applicative over Monad where applicable
-- - Using Semigroup operators
-- - Using Bifunctor combinators
-- - Preferring modern idioms
module Argus.Rules.Modernize
  ( -- * Detection
    detectModernize
  , ModernizeFinding (..)
  , ModernizeCategory (..)

    -- * Configuration
  , ModernizeConfig (..)
  , defaultModernizeConfig
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe (catMaybes)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

import Argus.Analysis.TextProcessing (isCodeLine, extractCode, patternInCode)
import Argus.Types

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Category of modernization
data ModernizeCategory
  = ApplicativeStyle      -- ^ Prefer Applicative over Monad
  | SemigroupStyle        -- ^ Use Semigroup operators
  | BifunctorStyle        -- ^ Use Bifunctor combinators
  | FunctorStyle          -- ^ Functor improvements
  | TraversableStyle      -- ^ Traversable improvements
  | MonoidStyle           -- ^ Monoid improvements
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A detected modernization opportunity
data ModernizeFinding = ModernizeFinding
  { mfCategory    :: ModernizeCategory
  , mfSpan        :: SrcSpan
  , mfCode        :: Text           -- ^ Original code
  , mfReplacement :: Text           -- ^ Suggested replacement
  , mfExplanation :: Text           -- ^ Why modernize
  , mfSeverity    :: Severity
  , mfAutoFix     :: [Fix]          -- ^ Auto-fix if available
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for modernization detection
data ModernizeConfig = ModernizeConfig
  { modEnabled           :: Bool   -- ^ Enable detection
  , modCheckApplicative  :: Bool   -- ^ Check Applicative patterns
  , modCheckSemigroup    :: Bool   -- ^ Check Semigroup patterns
  , modCheckBifunctor    :: Bool   -- ^ Check Bifunctor patterns
  , modCheckFunctor      :: Bool   -- ^ Check Functor patterns
  , modCheckTraversable  :: Bool   -- ^ Check Traversable patterns
  , modCheckMonoid       :: Bool   -- ^ Check Monoid patterns
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default configuration
defaultModernizeConfig :: ModernizeConfig
defaultModernizeConfig = ModernizeConfig
  { modEnabled = True
  , modCheckApplicative = True
  , modCheckSemigroup = True
  , modCheckBifunctor = True
  , modCheckFunctor = True
  , modCheckTraversable = True
  , modCheckMonoid = True
  }

--------------------------------------------------------------------------------
-- Detection
--------------------------------------------------------------------------------

-- | Detect modernization opportunities in source code
detectModernize :: ModernizeConfig -> FilePath -> Text -> [Diagnostic]
detectModernize config path content
  | not (modEnabled config) = []
  | otherwise =
    let findings = concat
          [ if modCheckApplicative config then detectApplicative path content else []
          , if modCheckSemigroup config then detectSemigroup path content else []
          , if modCheckBifunctor config then detectBifunctor path content else []
          , if modCheckFunctor config then detectFunctor path content else []
          , if modCheckTraversable config then detectTraversable path content else []
          , if modCheckMonoid config then detectMonoid path content else []
          ]
    in map findingToDiagnostic findings

--------------------------------------------------------------------------------
-- Applicative Style Detection
--------------------------------------------------------------------------------

-- | Detect opportunities to use Applicative instead of Monad
detectApplicative :: FilePath -> Text -> [ModernizeFinding]
detectApplicative path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkApplicativeLine path) linesWithNums

checkApplicativeLine :: FilePath -> (Int, Text) -> [ModernizeFinding]
checkApplicativeLine path (lineNum, line)
  | not (isCodeLine line) = []
  | otherwise = catMaybes
  [ -- return → pure
    if patternInCode " return " line && not (patternInCode "do" line)
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace " return " " pure " (T.strip line)
      in Just ModernizeFinding
        { mfCategory = ApplicativeStyle
        , mfSpan = mkSpan path lineNum "return" line
        , mfCode = T.strip line
        , mfReplacement = fixed
        , mfExplanation = "'pure' is more general than 'return' (Applicative vs Monad)"
        , mfSeverity = Suggestion
        , mfAutoFix = [mkModernizeFix "Use pure" [FixEdit lineSpan fixed] True]
        }
    else Nothing

  , -- liftM → fmap
    if patternInCode "liftM " line && not (patternInCode "liftM2" line)
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "liftM " "fmap " (T.strip line)
      in Just ModernizeFinding
        { mfCategory = ApplicativeStyle
        , mfSpan = mkSpan path lineNum "liftM" line
        , mfCode = T.strip line
        , mfReplacement = fixed
        , mfExplanation = "'fmap' is the Functor equivalent of 'liftM'"
        , mfSeverity = Suggestion
        , mfAutoFix = [mkModernizeFix "Use fmap" [FixEdit lineSpan fixed] True]
        }
    else Nothing

  , -- liftM2 → liftA2
    if patternInCode "liftM2 " line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "liftM2 " "liftA2 " (T.strip line)
      in Just ModernizeFinding
        { mfCategory = ApplicativeStyle
        , mfSpan = mkSpan path lineNum "liftM2" line
        , mfCode = T.strip line
        , mfReplacement = fixed
        , mfExplanation = "'liftA2' is the Applicative equivalent of 'liftM2'"
        , mfSeverity = Suggestion
        , mfAutoFix = [mkModernizeFix "Use liftA2" [FixEdit lineSpan fixed] True]
        }
    else Nothing

  , -- liftM3 → liftA3
    if patternInCode "liftM3 " line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "liftM3 " "liftA3 " (T.strip line)
      in Just ModernizeFinding
        { mfCategory = ApplicativeStyle
        , mfSpan = mkSpan path lineNum "liftM3" line
        , mfCode = T.strip line
        , mfReplacement = fixed
        , mfExplanation = "'liftA3' is the Applicative equivalent of 'liftM3'"
        , mfSeverity = Suggestion
        , mfAutoFix = [mkModernizeFix "Use liftA3" [FixEdit lineSpan fixed] True]
        }
    else Nothing

  , -- ap → (<*>)
    if patternInCode " ap " line || patternInCode "`ap`" line
    then Just ModernizeFinding
        { mfCategory = ApplicativeStyle
        , mfSpan = mkSpan path lineNum "ap" line
        , mfCode = T.strip line
        , mfReplacement = "(<*>)"
        , mfExplanation = "'(<*>)' is the Applicative operator equivalent of 'ap'"
        , mfSeverity = Suggestion
        , mfAutoFix = []  -- Hard to auto-fix due to operator syntax
        }
    else Nothing

  , -- sequence → sequenceA
    if patternInCode "sequence " line && not (patternInCode "sequenceA" line)
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "sequence " "sequenceA " (T.strip line)
      in Just ModernizeFinding
        { mfCategory = ApplicativeStyle
        , mfSpan = mkSpan path lineNum "sequence" line
        , mfCode = T.strip line
        , mfReplacement = fixed
        , mfExplanation = "'sequenceA' is more general (works with any Applicative)"
        , mfSeverity = Info
        , mfAutoFix = [mkModernizeFix "Use sequenceA" [FixEdit lineSpan fixed] True]
        }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Semigroup Style Detection
--------------------------------------------------------------------------------

-- | Detect opportunities to use Semigroup patterns
detectSemigroup :: FilePath -> Text -> [ModernizeFinding]
detectSemigroup path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkSemigroupLine path) linesWithNums

checkSemigroupLine :: FilePath -> (Int, Text) -> [ModernizeFinding]
checkSemigroupLine path (lineNum, line)
  | not (isCodeLine line) = []
  | otherwise = catMaybes
  [ -- mappend → (<>)
    if patternInCode "mappend " line || patternInCode "`mappend`" line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "`mappend`" "<>" $ T.replace "mappend " "(<>) " (T.strip line)
      in Just ModernizeFinding
        { mfCategory = SemigroupStyle
        , mfSpan = mkSpan path lineNum "mappend" line
        , mfCode = T.strip line
        , mfReplacement = fixed
        , mfExplanation = "'(<>)' from Semigroup is preferred over 'mappend'"
        , mfSeverity = Suggestion
        , mfAutoFix = [mkModernizeFix "Use (<>)" [FixEdit lineSpan fixed] True]
        }
    else Nothing

  , -- mempty `mappend` x → x
    if patternInCode "mempty `mappend`" line || patternInCode "mappend mempty " line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "mempty `mappend` " "" $
                  T.replace "mappend mempty " "" (T.strip line)
      in Just ModernizeFinding
        { mfCategory = SemigroupStyle
        , mfSpan = mkSpan path lineNum "mempty" line
        , mfCode = T.strip line
        , mfReplacement = fixed
        , mfExplanation = "'mempty <> x' is just 'x' (identity law)"
        , mfSeverity = Warning
        , mfAutoFix = [mkModernizeFix "Remove mempty identity" [FixEdit lineSpan fixed] True]
        }
    else Nothing

  , -- x `mappend` mempty → x
    if patternInCode "`mappend` mempty" line || patternInCode " <> mempty" line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace " `mappend` mempty" "" $
                  T.replace " <> mempty" "" (T.strip line)
      in Just ModernizeFinding
        { mfCategory = SemigroupStyle
        , mfSpan = mkSpan path lineNum "mempty" line
        , mfCode = T.strip line
        , mfReplacement = fixed
        , mfExplanation = "'x <> mempty' is just 'x' (identity law)"
        , mfSeverity = Warning
        , mfAutoFix = [mkModernizeFix "Remove mempty identity" [FixEdit lineSpan fixed] True]
        }
    else Nothing

  , -- mconcat [x] → x
    if patternInCode "mconcat [" line && patternInCode "]" line
    then Just ModernizeFinding
        { mfCategory = SemigroupStyle
        , mfSpan = mkSpan path lineNum "mconcat" line
        , mfCode = T.strip line
        , mfReplacement = "x"
        , mfExplanation = "'mconcat [x]' is just 'x'"
        , mfSeverity = Suggestion
        , mfAutoFix = []  -- Hard to auto-fix without knowing the structure
        }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Bifunctor Style Detection
--------------------------------------------------------------------------------

-- | Detect opportunities to use Bifunctor
detectBifunctor :: FilePath -> Text -> [ModernizeFinding]
detectBifunctor path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkBifunctorLine path) linesWithNums

checkBifunctorLine :: FilePath -> (Int, Text) -> [ModernizeFinding]
checkBifunctorLine path (lineNum, line)
  | not (isCodeLine line) = []
  | otherwise = catMaybes
  [ -- first f . second g → bimap f g
    if patternInCode "first " line && patternInCode " second " line
    then Just ModernizeFinding
        { mfCategory = BifunctorStyle
        , mfSpan = mkSpan path lineNum "first" line
        , mfCode = T.strip line
        , mfReplacement = "bimap f g"
        , mfExplanation = "'first f . second g' is 'bimap f g'"
        , mfSeverity = Suggestion
        , mfAutoFix = []  -- Complex to auto-fix
        }
    else Nothing

  , -- second g . first f → bimap f g
    if patternInCode "second " line && patternInCode " first " line
    then Just ModernizeFinding
        { mfCategory = BifunctorStyle
        , mfSpan = mkSpan path lineNum "second" line
        , mfCode = T.strip line
        , mfReplacement = "bimap f g"
        , mfExplanation = "'second g . first f' is 'bimap f g'"
        , mfSeverity = Suggestion
        , mfAutoFix = []  -- Complex to auto-fix
        }
    else Nothing

  , -- (f x, g y) pattern in arrow context
    if patternInCode "***" line && patternInCode "arr" line
    then Just ModernizeFinding
        { mfCategory = BifunctorStyle
        , mfSpan = mkSpan path lineNum "***" line
        , mfCode = T.strip line
        , mfReplacement = "bimap f g"
        , mfExplanation = "Consider using bimap instead of Arrow (***) for tuples"
        , mfSeverity = Info
        , mfAutoFix = []
        }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Functor Style Detection
--------------------------------------------------------------------------------

-- | Detect Functor improvement opportunities
detectFunctor :: FilePath -> Text -> [ModernizeFinding]
detectFunctor path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkFunctorLine path) linesWithNums

checkFunctorLine :: FilePath -> (Int, Text) -> [ModernizeFinding]
checkFunctorLine path (lineNum, line)
  | not (isCodeLine line) = []
  | otherwise = catMaybes
  [ -- fmap f . fmap g → fmap (f . g)
    if T.count "fmap " (extractCode line) >= 2 && patternInCode " . " line
    then Just ModernizeFinding
        { mfCategory = FunctorStyle
        , mfSpan = mkSpan path lineNum "fmap" line
        , mfCode = T.strip line
        , mfReplacement = "fmap (f . g)"
        , mfExplanation = "'fmap f . fmap g' is 'fmap (f . g)' (functor composition law)"
        , mfSeverity = Suggestion
        , mfAutoFix = []  -- Complex to auto-fix
        }
    else Nothing

  , -- (<$) x → fmap (const x) or x <$
    if patternInCode " <$ " line
    then Just ModernizeFinding
        { mfCategory = FunctorStyle
        , mfSpan = mkSpan path lineNum "<$" line
        , mfCode = T.strip line
        , mfReplacement = "Using <$ is good, consider void if discarding"
        , mfExplanation = "(<$) replaces all values in a Functor with a constant"
        , mfSeverity = Info
        , mfAutoFix = []
        }
    else Nothing

  , -- void (fmap f x) → void x
    if patternInCode "void (fmap" line || patternInCode "void $ fmap" line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "void (fmap " "void (" $
                  T.replace "void $ fmap " "void $ " (T.strip line)
      in Just ModernizeFinding
        { mfCategory = FunctorStyle
        , mfSpan = mkSpan path lineNum "void" line
        , mfCode = T.strip line
        , mfReplacement = fixed
        , mfExplanation = "'void (fmap f x)' is just 'void x'"
        , mfSeverity = Warning
        , mfAutoFix = [mkModernizeFix "Simplify void" [FixEdit lineSpan fixed] True]
        }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Traversable Style Detection
--------------------------------------------------------------------------------

-- | Detect Traversable improvement opportunities
detectTraversable :: FilePath -> Text -> [ModernizeFinding]
detectTraversable path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkTraversableLine path) linesWithNums

checkTraversableLine :: FilePath -> (Int, Text) -> [ModernizeFinding]
checkTraversableLine path (lineNum, line)
  | not (isCodeLine line) = []
  | otherwise = catMaybes
  [ -- mapM → traverse
    if patternInCode "mapM " line && not (patternInCode "mapM_" line)
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "mapM " "traverse " (T.strip line)
      in Just ModernizeFinding
        { mfCategory = TraversableStyle
        , mfSpan = mkSpan path lineNum "mapM" line
        , mfCode = T.strip line
        , mfReplacement = fixed
        , mfExplanation = "'traverse' is more general than 'mapM'"
        , mfSeverity = Info
        , mfAutoFix = [mkModernizeFix "Use traverse" [FixEdit lineSpan fixed] True]
        }
    else Nothing

  , -- mapM_ → traverse_
    if patternInCode "mapM_ " line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "mapM_ " "traverse_ " (T.strip line)
      in Just ModernizeFinding
        { mfCategory = TraversableStyle
        , mfSpan = mkSpan path lineNum "mapM_" line
        , mfCode = T.strip line
        , mfReplacement = fixed
        , mfExplanation = "'traverse_' is more general than 'mapM_'"
        , mfSeverity = Info
        , mfAutoFix = [mkModernizeFix "Use traverse_" [FixEdit lineSpan fixed] True]
        }
    else Nothing

  , -- forM → for
    if patternInCode "forM " line && not (patternInCode "forM_" line)
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "forM " "for " (T.strip line)
      in Just ModernizeFinding
        { mfCategory = TraversableStyle
        , mfSpan = mkSpan path lineNum "forM" line
        , mfCode = T.strip line
        , mfReplacement = fixed
        , mfExplanation = "'for' is more general than 'forM'"
        , mfSeverity = Info
        , mfAutoFix = [mkModernizeFix "Use for" [FixEdit lineSpan fixed] True]
        }
    else Nothing

  , -- forM_ → for_
    if patternInCode "forM_ " line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "forM_ " "for_ " (T.strip line)
      in Just ModernizeFinding
        { mfCategory = TraversableStyle
        , mfSpan = mkSpan path lineNum "forM_" line
        , mfCode = T.strip line
        , mfReplacement = fixed
        , mfExplanation = "'for_' is more general than 'forM_'"
        , mfSeverity = Info
        , mfAutoFix = [mkModernizeFix "Use for_" [FixEdit lineSpan fixed] True]
        }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Monoid Style Detection
--------------------------------------------------------------------------------

-- | Detect Monoid improvement opportunities
detectMonoid :: FilePath -> Text -> [ModernizeFinding]
detectMonoid path content =
  let linesWithNums = zip [1..] (T.lines content)
  in concatMap (checkMonoidLine path) linesWithNums

checkMonoidLine :: FilePath -> (Int, Text) -> [ModernizeFinding]
checkMonoidLine path (lineNum, line)
  | not (isCodeLine line) = []
  | otherwise = catMaybes
  [ -- fold → mconcat (for lists)
    if patternInCode "fold " line && not (patternInCode "foldl" line) &&
       not (patternInCode "foldr" line) && not (patternInCode "foldMap" line)
    then Just ModernizeFinding
        { mfCategory = MonoidStyle
        , mfSpan = mkSpan path lineNum "fold" line
        , mfCode = T.strip line
        , mfReplacement = "Consider if mconcat or foldMap is clearer"
        , mfExplanation = "'fold' is 'mconcat' for Foldable"
        , mfSeverity = Info
        , mfAutoFix = []
        }
    else Nothing

  , -- foldr (<>) mempty → fold
    if patternInCode "foldr (<>) mempty" line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "foldr (<>) mempty" "fold" (T.strip line)
      in Just ModernizeFinding
        { mfCategory = MonoidStyle
        , mfSpan = mkSpan path lineNum "foldr" line
        , mfCode = T.strip line
        , mfReplacement = fixed
        , mfExplanation = "'foldr (<>) mempty' is the definition of 'fold'"
        , mfSeverity = Suggestion
        , mfAutoFix = [mkModernizeFix "Use fold" [FixEdit lineSpan fixed] True]
        }
    else Nothing

  , -- foldMap id → fold
    if patternInCode "foldMap id" line
    then
      let lineSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length line + 1)
          fixed = T.replace "foldMap id" "fold" (T.strip line)
      in Just ModernizeFinding
        { mfCategory = MonoidStyle
        , mfSpan = mkSpan path lineNum "foldMap id" line
        , mfCode = T.strip line
        , mfReplacement = fixed
        , mfExplanation = "'foldMap id' is equivalent to 'fold'"
        , mfSeverity = Suggestion
        , mfAutoFix = [mkModernizeFix "Use fold" [FixEdit lineSpan fixed] True]
        }
    else Nothing
  ]

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Create a span for a pattern match
mkSpan :: FilePath -> Int -> Text -> Text -> SrcSpan
mkSpan path lineNum needle line =
  let col = case T.breakOn needle line of
              (before, _) -> T.length before + 1
  in mkSrcSpanRaw path lineNum col lineNum (col + T.length needle)

-- | Convert finding to diagnostic
findingToDiagnostic :: ModernizeFinding -> Diagnostic
findingToDiagnostic ModernizeFinding{..} = Diagnostic
  { diagSpan = mfSpan
  , diagSeverity = mfSeverity
  , diagKind = CodePattern
  , diagMessage = mfExplanation <> ". Replace with: " <> mfReplacement
  , diagCode = Just $ "modernize/" <> categoryCode mfCategory
  , diagFixes = mfAutoFix
  , diagRelated = []
  }

categoryCode :: ModernizeCategory -> Text
categoryCode = \case
  ApplicativeStyle -> "applicative"
  SemigroupStyle -> "semigroup"
  BifunctorStyle -> "bifunctor"
  FunctorStyle -> "functor"
  TraversableStyle -> "traversable"
  MonoidStyle -> "monoid"

-- | Helper to create a modernize fix with proper category and safety
mkModernizeFix :: Text -> [FixEdit] -> Bool -> Fix
mkModernizeFix title edits preferred = Fix
  { fixTitle = title
  , fixEdits = edits
  , fixIsPreferred = preferred
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCModernize
  , fixSafety = FSMostly
  }
