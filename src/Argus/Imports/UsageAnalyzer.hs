{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Argus.Imports.UsageAnalyzer
-- Description : Analyze symbol usage in source code
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides functionality for analyzing which symbols are used
-- in source code before and after a fix is applied. It enables:
--
-- * Detecting what new symbols are introduced by a fix
-- * Detecting what symbols are removed by a fix
-- * Mapping symbols to their required imports
-- * Determining optimal import modifications
--
-- == Usage
--
-- @
-- -- Analyze a fix to determine import changes
-- let analysis = analyzeFixImports sourceText fix importDB
--
-- -- Get required import additions and removals
-- let addImports = analysisAddImports analysis
-- let removeImports = analysisRemoveImports analysis
-- @
module Argus.Imports.UsageAnalyzer
  ( -- * Types
    UsageAnalysis (..)
  , SymbolUsage (..)
  , SymbolType (..)
  , UsageChange (..)

    -- * Analysis Functions
  , analyzeSourceSymbols
  , analyzeFixSymbols
  , analyzeUsageChange
  , computeImportChanges

    -- * Symbol Extraction
  , extractUsedSymbols
  , extractQualifiedUsages
  , extractTypeSymbols
  , extractValueSymbols

    -- * Import Resolution
  , resolveSymbolImports
  , findBestImport
  , isSymbolInScope
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.List (nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, catMaybes)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Text.Regex.TDFA ((=~))

import Argus.Types (FixImport(..), ImportSymbol(..), ImportSymbolType(..), Fix(..), FixEdit(..))
import Argus.Imports.ImportDB
  ( ImportDB(..)
  , SymbolInfo(..)
  , lookupSymbol
  )

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Information about a symbol's usage
data SymbolUsage = SymbolUsage
  { suName          :: Text           -- ^ Symbol name
  , suQualifier     :: Maybe Text     -- ^ Optional module qualifier
  , suType          :: SymbolType     -- ^ Type of symbol (type vs value)
  , suOccurrences   :: Int            -- ^ Number of occurrences
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Type of symbol
data SymbolType
  = STType        -- ^ Type, data type, or class
  | STValue       -- ^ Function, value, or constructor
  | STOperator    -- ^ Operator (infix function)
  | STUnknown     -- ^ Unknown/could be either
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Changes in symbol usage between old and new code
data UsageChange = UsageChange
  { ucAdded       :: Set Text         -- ^ Symbols added by the change
  , ucRemoved     :: Set Text         -- ^ Symbols removed by the change
  , ucUnchanged   :: Set Text         -- ^ Symbols still present
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Complete analysis of a fix's impact on imports
data UsageAnalysis = UsageAnalysis
  { uaSourceFile     :: FilePath       -- ^ Source file being analyzed
  , uaSymbolsBefore  :: Set Text       -- ^ Symbols used before fix
  , uaSymbolsAfter   :: Set Text       -- ^ Symbols used after fix
  , uaChange         :: UsageChange    -- ^ Summary of changes
  , uaRequiredImports :: [FixImport]   -- ^ Imports needed for new symbols
  , uaRemovableImports :: [Text]       -- ^ Imports that can be removed
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Analysis Functions
--------------------------------------------------------------------------------

-- | Analyze source code for used symbols
analyzeSourceSymbols :: Text -> [SymbolUsage]
analyzeSourceSymbols source =
  let valueSyms = extractValueSymbols source
      typeSyms = extractTypeSymbols source
      qualSyms = extractQualifiedUsages source
  in nub $ valueSyms ++ typeSyms ++ qualSyms

-- | Analyze a fix to determine what symbols it introduces/removes
analyzeFixSymbols :: Fix -> UsageChange
analyzeFixSymbols fix =
  let edits = fixEdits fix
      -- Extract symbols from what's being added
      -- Note: FixEdit only tracks newText, not old text
      addedCode = T.unlines $ map fixEditNewText edits

      addedSymbols = Set.fromList $ map suName $ analyzeSourceSymbols addedCode

      -- For removed symbols, we rely on the fix's explicit list
      -- since FixEdit doesn't store old text
  in UsageChange
    { ucAdded = addedSymbols
    , ucRemoved = Set.empty  -- Would need full file analysis
    , ucUnchanged = Set.empty
    }

-- | Analyze usage change between old and new code
analyzeUsageChange :: Text -> Text -> UsageChange
analyzeUsageChange oldCode newCode =
  let oldSymbols = Set.fromList $ map suName $ analyzeSourceSymbols oldCode
      newSymbols = Set.fromList $ map suName $ analyzeSourceSymbols newCode
  in UsageChange
    { ucAdded = newSymbols `Set.difference` oldSymbols
    , ucRemoved = oldSymbols `Set.difference` newSymbols
    , ucUnchanged = oldSymbols `Set.intersection` newSymbols
    }

-- | Compute the import changes needed for a fix
computeImportChanges :: ImportDB -> Text -> Fix -> UsageAnalysis
computeImportChanges db sourceText fix =
  let -- Get the full change
      change = analyzeFixSymbols fix

      -- Look up imports for added symbols
      addedSymbolsList = Set.toList (ucAdded change)
      requiredImports = resolveSymbolImports db addedSymbolsList

      -- For removed symbols, we'd need to analyze the whole file to see
      -- if they're still used elsewhere. For now, we use the fix's
      -- explicit removeImports field.
      removable = fixRemoveImports fix

      -- Get current symbols
      currentSymbols = Set.fromList $ map suName $ analyzeSourceSymbols sourceText
      afterSymbols = (currentSymbols `Set.difference` ucRemoved change)
                       `Set.union` ucAdded change

  in UsageAnalysis
    { uaSourceFile = ""  -- Will be filled in by caller
    , uaSymbolsBefore = currentSymbols
    , uaSymbolsAfter = afterSymbols
    , uaChange = change
    , uaRequiredImports = requiredImports
    , uaRemovableImports = removable
    }

--------------------------------------------------------------------------------
-- Symbol Extraction
--------------------------------------------------------------------------------

-- | Extract all used symbols from source code
extractUsedSymbols :: Text -> [Text]
extractUsedSymbols = map suName . analyzeSourceSymbols

-- | Extract qualified usages (e.g., M.foo, Data.Map.lookup)
extractQualifiedUsages :: Text -> [SymbolUsage]
extractQualifiedUsages source =
  let -- Match qualified names: Module.name or M.name
      qualPat = "([A-Z][a-zA-Z0-9]*\\.)+[a-z_][a-zA-Z0-9_']*" :: String
      matches = findAllMatches source qualPat
  in mapMaybe parseQualifiedName matches
  where
    parseQualifiedName :: Text -> Maybe SymbolUsage
    parseQualifiedName qname =
      case T.splitOn "." qname of
        [] -> Nothing
        parts ->
          let name = last parts
              qualifier = T.intercalate "." (init parts)
          in Just $ SymbolUsage
            { suName = name
            , suQualifier = Just qualifier
            , suType = if isTypeish name then STType else STValue
            , suOccurrences = 1
            }

-- | Extract type-level symbols
extractTypeSymbols :: Text -> [SymbolUsage]
extractTypeSymbols source =
  let -- Match type names: start with uppercase
      typePat = "\\b([A-Z][a-zA-Z0-9_']*)\\b" :: String
      matches = findAllMatches source typePat
      -- Filter out common keywords and very short matches
      filtered = filter isLikelyType matches
  in map mkTypeUsage filtered
  where
    mkTypeUsage name = SymbolUsage
      { suName = name
      , suQualifier = Nothing
      , suType = STType
      , suOccurrences = 1
      }

    isLikelyType t = T.length t > 1 && t `notElem` keywords

    keywords =
      [ "True", "False", "Nothing", "Just", "Left", "Right"
      , "LT", "EQ", "GT", "IO", "If", "Then", "Else", "Case", "Of"
      , "Let", "In", "Where", "Do", "Type", "Data", "Newtype"
      , "Class", "Instance", "Deriving", "Module", "Import"
      ]

-- | Extract value-level symbols
extractValueSymbols :: Text -> [SymbolUsage]
extractValueSymbols source =
  let -- Match function/value names: start with lowercase or underscore
      -- Note: We omit trailing \b because primes aren't word characters
      valuePat = "\\b[a-z_][a-zA-Z0-9_']*" :: String
      matches = findAllMatches source valuePat
      -- Filter out keywords
      filtered = filter (not . isKeyword) matches

      -- Also extract operators
      opPat = "\\([!#$%&*+./<=>?@\\\\^|~:-]+\\)" :: String
      opMatches = findAllMatches source opPat
      operators = map (T.filter (`notElem` ("()" :: String))) opMatches

  in map mkValueUsage filtered ++ map mkOpUsage operators
  where
    mkValueUsage name = SymbolUsage
      { suName = name
      , suQualifier = Nothing
      , suType = STValue
      , suOccurrences = 1
      }

    mkOpUsage name = SymbolUsage
      { suName = name
      , suQualifier = Nothing
      , suType = STOperator
      , suOccurrences = 1
      }

    isKeyword t = t `elem`
      [ "if", "then", "else", "case", "of", "let", "in", "where"
      , "do", "module", "import", "type", "data", "newtype", "class"
      , "instance", "deriving", "qualified", "as", "hiding", "forall"
      , "infix", "infixl", "infixr", "foreign", "export", "safe"
      , "interruptible", "unsafe", "default", "family"
      ]

--------------------------------------------------------------------------------
-- Import Resolution
--------------------------------------------------------------------------------

-- | Resolve a list of symbols to their required imports
resolveSymbolImports :: ImportDB -> [Text] -> [FixImport]
resolveSymbolImports db symbols =
  let -- Look up each symbol
      symbolInfos = catMaybes $ map (findBestImport db) symbols

      -- Group by module
      grouped = groupByModule symbolInfos

      -- Convert to FixImports
  in map toFixImport (Map.toList grouped)
  where
    groupByModule :: [(Text, SymbolInfo)] -> Map Text [(Text, SymbolInfo)]
    groupByModule = foldr addToGroup Map.empty

    addToGroup (name, info) m =
      let modName = siModule info
      in Map.insertWith (++) modName [(name, info)] m

    toFixImport :: (Text, [(Text, SymbolInfo)]) -> FixImport
    toFixImport (modName, syms) = FixImport
      { fimpModule = modName
      , fimpSymbols = map (mkImportSymbol . fst) syms
      , fimpQualified = Nothing
      , fimpHiding = False
      , fimpPackage = Nothing
      }

    mkImportSymbol name = ImportSymbol
      { isymName = name
      , isymType = ISTFunction  -- Default, could be refined
      , isymChildren = []
      }

-- | Find the best import for a symbol
findBestImport :: ImportDB -> Text -> Maybe (Text, SymbolInfo)
findBestImport db name =
  case lookupSymbol db name of
    [] -> Nothing
    (info:_) -> Just (name, info)

-- | Check if a symbol is in scope given current imports
isSymbolInScope :: Text -> [Text] -> ImportDB -> Bool
isSymbolInScope symbol importedModules db =
  case lookupSymbol db symbol of
    [] -> False  -- Unknown symbol
    infos -> any (isImportedFrom importedModules) infos
  where
    isImportedFrom mods info =
      siModule info `elem` mods

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Check if a name looks like a type (starts with uppercase)
isTypeish :: Text -> Bool
isTypeish t = case T.uncons t of
  Just (c, _) -> c `elem` ['A'..'Z']
  Nothing -> False

-- | Find all regex matches in text
findAllMatches :: Text -> String -> [Text]
findAllMatches src pat =
  let sourceStr = T.unpack src
      findAll :: String -> [String]
      findAll "" = []
      findAll s = case s =~ pat :: (String, String, String) of
        (_, "", _) -> []
        (_, match, after) ->
          if null match
          then []
          else match : findAll after
  in map T.pack $ findAll sourceStr
