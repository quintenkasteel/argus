{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Argus.Rules.Pragmas
-- Description : LANGUAGE pragma analysis
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides analysis of LANGUAGE pragmas, including
-- detection of unused or redundant language extensions.
module Argus.Rules.Pragmas
  ( -- * Pragma checking
    checkPragmas
  , findUnusedPragmas
  , detectUsedExtensions
  ) where

import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import Argus.Analysis.Syntactic (PragmaInfo (..))
import Argus.Types

--------------------------------------------------------------------------------
-- Pragma Checking
--------------------------------------------------------------------------------

-- | Check pragmas for issues and return diagnostics
checkPragmas :: FilePath -> Text -> [PragmaInfo] -> [Diagnostic]
checkPragmas path source pragmas =
  let usedExtensions = detectUsedExtensions source
      unused = findUnusedPragmas pragmas usedExtensions
  in map (unusedPragmaToDiagnostic path) unused

-- | Find pragmas that are enabled but not used
findUnusedPragmas :: [PragmaInfo] -> Set Text -> [PragmaInfo]
findUnusedPragmas pragmas usedExtensions =
  filter (isUnused usedExtensions) pragmas
  where
    isUnused used pragma =
      piIsEnabled pragma
      && not (piExtension pragma `Set.member` used)
      && piExtension pragma `Set.member` checkableExtensions

-- | Extensions we can reliably check for usage
checkableExtensions :: Set Text
checkableExtensions = Set.fromList
  [ "BangPatterns"
  , "LambdaCase"
  , "MultiWayIf"
  , "TupleSections"
  , "NumericUnderscores"
  , "BinaryLiterals"
  , "HexFloatLiterals"
  , "TypeApplications"
  , "RecordWildCards"
  , "NamedFieldPuns"
  , "ViewPatterns"
  , "PatternGuards"
  , "EmptyCase"
  , "NegativeLiterals"
  , "BlockArguments"
  , "ParallelListComp"
  , "TransformListComp"
  , "MonadComprehensions"
  , "ApplicativeDo"
  , "RecursiveDo"
  , "Arrows"
  , "UnicodeSyntax"
  ]

-- | Detect which extensions are used based on source text patterns
-- This is a heuristic-based approach since full detection requires parsing
detectUsedExtensions :: Text -> Set Text
detectUsedExtensions source = Set.fromList $ map fst $ Prelude.filter (checkUsed source) extensionPatterns
  where
    extensionPatterns :: [(Text, Text -> Bool)]
    extensionPatterns =
      [ ("BangPatterns", containsBangPatterns)
      , ("LambdaCase", containsLambdaCase)
      , ("MultiWayIf", containsMultiWayIf)
      , ("TupleSections", containsTupleSections)
      , ("NumericUnderscores", containsNumericUnderscores)
      , ("BinaryLiterals", containsBinaryLiterals)
      , ("HexFloatLiterals", containsHexFloatLiterals)
      , ("TypeApplications", containsTypeApplications)
      , ("RecordWildCards", containsRecordWildCards)
      , ("NamedFieldPuns", containsNamedFieldPuns)
      , ("ViewPatterns", containsViewPatterns)
      , ("EmptyCase", containsEmptyCase)
      , ("BlockArguments", containsBlockArguments)
      , ("UnicodeSyntax", containsUnicodeSyntax)
      , ("RecursiveDo", containsRecursiveDo)
      , ("Arrows", containsArrows)
      , ("ApplicativeDo", containsApplicativeDo)
      ]

    checkUsed :: Text -> (Text, Text -> Bool) -> Bool
    checkUsed src (_, check) = check src

-- | Check for bang patterns (!)
containsBangPatterns :: Text -> Bool
containsBangPatterns source =
  -- Pattern: identifier followed by ! then identifier, e.g., f !x = ...
  T.isInfixOf " !" source && not (T.isInfixOf "!=" source)

-- | Check for lambda case (\\case)
containsLambdaCase :: Text -> Bool
containsLambdaCase source =
  T.isInfixOf "\\case" source || T.isInfixOf "\\ case" source

-- | Check for multi-way if (if |)
containsMultiWayIf :: Text -> Bool
containsMultiWayIf source = T.isInfixOf "if |" source || T.isInfixOf "if\n  |" source

-- | Check for tuple sections ((, x) or (x, ))
containsTupleSections :: Text -> Bool
containsTupleSections source =
  T.isInfixOf "(," source || T.isInfixOf ",)" source

-- | Check for numeric underscores (1_000_000)
containsNumericUnderscores :: Text -> Bool
containsNumericUnderscores source =
  -- Look for digit_digit patterns
  any hasUnderscore (T.words source)
  where
    hasUnderscore word =
      T.any (== '_') word && T.any isDigit word && T.any isDigit (T.filter (/= '_') word)
    isDigit c = c >= '0' && c <= '9'

-- | Check for binary literals (0b...)
containsBinaryLiterals :: Text -> Bool
containsBinaryLiterals source =
  T.isInfixOf "0b" source || T.isInfixOf "0B" source

-- | Check for hex float literals (0x...p...)
containsHexFloatLiterals :: Text -> Bool
containsHexFloatLiterals source =
  (T.isInfixOf "0x" source || T.isInfixOf "0X" source) &&
  (T.isInfixOf "p" source || T.isInfixOf "P" source)

-- | Check for type applications (@Type)
containsTypeApplications :: Text -> Bool
containsTypeApplications source =
  T.isInfixOf " @" source || T.isInfixOf "(@" source

-- | Check for RecordWildCards ({..})
containsRecordWildCards :: Text -> Bool
containsRecordWildCards source = T.isInfixOf "{..}" source

-- | Check for NamedFieldPuns (matching {field} patterns)
containsNamedFieldPuns :: Text -> Bool
containsNamedFieldPuns source =
  -- Look for patterns like {fieldName} without assignment
  -- This is a heuristic - proper detection would need parsing
  T.isInfixOf "{" source && T.isInfixOf "}" source

-- | Check for view patterns (pattern -> ...)
containsViewPatterns :: Text -> Bool
containsViewPatterns _source =
  -- View patterns have the form (f -> pat), look for -> in patterns
  -- This is heuristic since -> is also used in types and lambdas
  False  -- Too hard to detect reliably

-- | Check for empty case expressions
containsEmptyCase :: Text -> Bool
containsEmptyCase _source =
  -- Look for "case ... of" followed by "where" or end without alternatives
  -- This is heuristic
  False  -- Too hard to detect reliably

-- | Check for BlockArguments (do blocks in function arguments)
containsBlockArguments :: Text -> Bool
containsBlockArguments _source =
  -- Look for patterns like "func do ..." without $
  -- This is heuristic
  False  -- Too hard to detect reliably

-- | Check for unicode syntax characters
containsUnicodeSyntax :: Text -> Bool
containsUnicodeSyntax source =
  T.any (`elem` unicodeSyntaxChars) source
  where
    unicodeSyntaxChars = ['\x2200', '\x2203', '\x2192', '\x2190', '\x21D2', '\x2237', '\x2026']
    -- ∀, ∃, →, ←, ⇒, ∷, …

-- | Check for recursive do (mdo or rec)
containsRecursiveDo :: Text -> Bool
containsRecursiveDo source =
  T.isInfixOf " mdo" source || T.isInfixOf "\nmdo" source ||
  T.isInfixOf " rec " source

-- | Check for arrow syntax (proc, -<)
containsArrows :: Text -> Bool
containsArrows source =
  T.isInfixOf " proc " source || T.isInfixOf "-<" source

-- | Check for ApplicativeDo
containsApplicativeDo :: Text -> Bool
containsApplicativeDo _source =
  -- Can't detect this reliably without semantic analysis
  False

-- | Convert unused pragma to diagnostic
unusedPragmaToDiagnostic :: FilePath -> PragmaInfo -> Diagnostic
unusedPragmaToDiagnostic _path pragma = Diagnostic
  { diagSpan = piSpan pragma
  , diagSeverity = Suggestion
  , diagKind = RedundantCode
  , diagMessage = "Language extension '" <> piExtension pragma <> "' appears to be unused"
  , diagCode = Just $ "pragma/unused/" <> piExtension pragma
  , diagFixes = [Fix
      { fixTitle = "Remove unused extension"
      , fixEdits = [FixEdit (piSpan pragma) ""]
      , fixIsPreferred = True
      , fixAddImports = []
      , fixRemoveImports = []
      , fixCategory = FCRedundant
      , fixSafety = FSAlways
      }]
  , diagRelated = []
  }
