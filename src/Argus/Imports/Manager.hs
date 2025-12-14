{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Argus.Imports.Manager
-- Description : Import manipulation for auto-fixes
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides functionality for parsing, modifying, and generating
-- import declarations. It handles:
--
-- * Adding new imports (with proper sorting and deduplication)
-- * Removing unused imports
-- * Qualifying imports
-- * Converting between import styles
--
-- == Usage
--
-- @
-- -- Parse existing imports
-- let imports = parseImports fileContent
--
-- -- Add a new import
-- let newImports = addImport (mkImport "Safe" ["headMay"]) imports
--
-- -- Generate the modified import section
-- let newContent = replaceImports fileContent newImports
-- @
module Argus.Imports.Manager
  ( -- * Configuration
    ImportManagerConfig (..)
  , defaultImportConfig

    -- * High-level API
  , applyImportChanges
  , addFixImports
  , removeFixImports
  , fixImportToParsed

    -- * Import Types
  , ParsedImport (..)
  , ImportStyle (..)
  , ImportModification (..)

    -- * Parsing
  , parseImports
  , parseImportsFromModule
  , getImportSpan

    -- * Modification
  , addImport
  , addImports
  , removeImport
  , removeImportSymbol
  , qualifyImport
  , unqualifyImport
  , makeImportExplicit
  , mergeImports

    -- * Generation
  , renderImport
  , renderImports
  , replaceImports
  , insertImport

    -- * Utilities
  , sortImports
  , groupImports
  , deduplicateImports
  , findImportForSymbol
  , importsSymbol
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (ToJSON, FromJSON)
import Data.List (sortBy, groupBy, nubBy)
import Data.List.NonEmpty (NonEmpty(..))
import Data.List.NonEmpty qualified as NE
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

-- GHC imports for parsing
import "ghc-lib-parser" GHC.Hs
import "ghc-lib-parser" GHC.Types.SrcLoc (GenLocated(..), unLoc)
import "ghc-lib-parser" GHC.Types.SrcLoc qualified as GHC
import "ghc-lib-parser" GHC.Types.Name.Reader (rdrNameOcc)
import "ghc-lib-parser" GHC.Types.Name.Occurrence (occNameString)
import "ghc-lib-parser" GHC.Data.FastString (unpackFS)

import Argus.Types (Fix(..), FixImport(..), ImportSymbol(..), SrcSpan, mkSrcSpanRaw, srcSpanStartLineRaw, srcSpanEndLineRaw)

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Configuration for import management
data ImportManagerConfig = ImportManagerConfig
  { imcAddMissingImports    :: Bool        -- ^ Add missing imports from fixes
  , imcRemoveUnusedImports  :: Bool        -- ^ Remove imports from fixes
  , imcOrganizeImports      :: Bool        -- ^ Sort and organize imports
  , imcExplicitImports      :: Bool        -- ^ Prefer explicit import lists
  , imcQualifyNewImports    :: Bool        -- ^ Use qualified imports when possible
  , imcGroupImports         :: Bool        -- ^ Group imports by category
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Sensible defaults for import management
defaultImportConfig :: ImportManagerConfig
defaultImportConfig = ImportManagerConfig
  { imcAddMissingImports   = True
  , imcRemoveUnusedImports = True
  , imcOrganizeImports     = False  -- Conservative default
  , imcExplicitImports     = True
  , imcQualifyNewImports   = False
  , imcGroupImports        = False
  }

--------------------------------------------------------------------------------
-- High-level API
--------------------------------------------------------------------------------

-- | Apply import changes from a Fix to source content
applyImportChanges :: ImportManagerConfig -> Text -> Fix -> Text
applyImportChanges config content fix =
  let -- Parse existing imports
      existingImports = parseImports content

      -- Add required imports
      withAdded = if imcAddMissingImports config && not (null (fixAddImports fix))
                  then addFixImports existingImports (fixAddImports fix)
                  else existingImports

      -- Remove imports for symbols that are no longer used
      withRemoved = if imcRemoveUnusedImports config && not (null (fixRemoveImports fix))
                    then removeFixImports withAdded (fixRemoveImports fix)
                    else withAdded

      -- Optionally organize
      finalImports = if imcOrganizeImports config
                     then sortImports withRemoved
                     else withRemoved

      -- Calculate import section bounds
      (startLine, endLine) = getImportBounds content existingImports

  in replaceImports content startLine endLine finalImports

-- | Get the line bounds of the import section
getImportBounds :: Text -> [ParsedImport] -> (Int, Int)
getImportBounds content imports =
  case mapMaybe piSpan imports of
    [] ->
      -- No existing imports, find module line and insert after
      let modLine = findModuleEndLine (T.lines content)
      in (modLine + 1, modLine)  -- Empty range at insertion point
    spans ->
      -- Find min/max lines from spans
      let startLines = map srcSpanStartLineRaw spans
          endLines = map srcSpanEndLineRaw spans
      in (minimum startLines, maximum endLines)

-- | Add imports specified by a Fix
addFixImports :: [ParsedImport] -> [FixImport] -> [ParsedImport]
addFixImports existing fixImports =
  let newParsedImports = map fixImportToParsed fixImports
  in mergeImports existing newParsedImports

-- | Remove imports specified by a Fix (by symbol name)
removeFixImports :: [ParsedImport] -> [Text] -> [ParsedImport]
removeFixImports imports symbolsToRemove =
  -- For each symbol to remove, try to remove it from imports
  -- If an import becomes empty after removing all its explicit symbols,
  -- we might want to remove the entire import (depends on style)
  foldl removeSymFromImports imports symbolsToRemove
  where
    removeSymFromImports imps sym =
      map (removeExplicitSymbol sym) imps

    removeExplicitSymbol sym imp =
      imp { piExplicit = filter (/= sym) (piExplicit imp) }

-- | Convert a FixImport to a ParsedImport
fixImportToParsed :: FixImport -> ParsedImport
fixImportToParsed FixImport{..} = ParsedImport
  { piModule = fimpModule
  , piQualified = fimpQualified /= Nothing
  , piAlias = fimpQualified
  , piHiding = fimpHiding
  , piExplicit = map (\s -> isymName s) fimpSymbols
  , piPackage = fimpPackage
  , piSpan = Nothing
  , piIsSafe = False
  , piIsSource = False
  }

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Style of import declaration
data ImportStyle
  = ISOpen                     -- ^ import Foo (no list)
  | ISExplicit                 -- ^ import Foo (x, y, z)
  | ISHiding                   -- ^ import Foo hiding (x, y)
  | ISQualified (Maybe Text)   -- ^ import qualified Foo [as X]
  | ISQualifiedExplicit (Maybe Text) [Text]  -- ^ import qualified Foo (x,y) [as X]
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A parsed import declaration
data ParsedImport = ParsedImport
  { piModule        :: Text               -- ^ Module name
  , piQualified     :: Bool               -- ^ Is import qualified?
  , piAlias         :: Maybe Text         -- ^ Alias (as X)
  , piHiding        :: Bool               -- ^ Is hiding import?
  , piExplicit      :: [Text]             -- ^ Explicit import list
  , piPackage       :: Maybe Text         -- ^ Package name
  , piSpan          :: Maybe SrcSpan      -- ^ Source location
  , piIsSafe        :: Bool               -- ^ Safe import?
  , piIsSource      :: Bool               -- ^ Source import?
  } deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON, NFData)

-- | Modification to an import
data ImportModification
  = IMAdd ParsedImport         -- ^ Add import
  | IMRemove Text              -- ^ Remove import by module name
  | IMRemoveSymbol Text Text   -- ^ Remove symbol from import
  | IMQualify Text (Maybe Text) -- ^ Qualify import with optional alias
  | IMUnqualify Text           -- ^ Remove qualification
  | IMAddSymbol Text Text      -- ^ Add symbol to import
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- | Parse import declarations from source text
parseImports :: Text -> [ParsedImport]
parseImports content =
  let importLines = filter isImportLine (zip [1..] (T.lines content))
  in mapMaybe (parseImportLine content) importLines
  where
    isImportLine (_, line) =
      T.isPrefixOf "import" (T.stripStart line) &&
      not (T.isPrefixOf "--" (T.stripStart line))

-- | Parse a single import line
parseImportLine :: Text -> (Int, Text) -> Maybe ParsedImport
parseImportLine _fullContent (lineNum, line) = do
  -- Remove leading/trailing whitespace
  let stripped = T.strip line

  -- Check it starts with import
  rest <- T.stripPrefix "import" stripped

  -- Parse the rest
  let tokens = T.words (T.strip rest)
  parseImportTokens lineNum tokens

-- | Parse import tokens into a ParsedImport
parseImportTokens :: Int -> [Text] -> Maybe ParsedImport
parseImportTokens lineNum tokens = case tokens of
  [] -> Nothing
  _ -> Just $ buildImport lineNum tokens
  where
    buildImport ln toks =
      let -- Check for qualified
          (isQual, afterQual) = case toks of
            ("qualified":rest) -> (True, rest)
            _ -> (False, toks)

          -- Check for safe
          (isSafe, afterSafe) = case afterQual of
            ("safe":rest) -> (True, rest)
            _ -> (False, afterQual)

          -- Check for package
          (pkg, afterPkg) = case afterSafe of
            (p:rest) | T.isPrefixOf "\"" p -> (Just (T.filter (/= '"') p), rest)
            _ -> (Nothing, afterSafe)

          -- Get module name
          (modName, afterMod) = case afterPkg of
            (m:rest) | not (isKeyword m) -> (m, rest)
            _ -> ("", afterPkg)

          -- Check for alias
          (alias, afterAlias) = case afterMod of
            ("as":a:rest) -> (Just a, rest)
            _ -> (Nothing, afterMod)

          -- Check for hiding
          (isHiding, afterHiding) = case afterAlias of
            ("hiding":rest) -> (True, rest)
            _ -> (False, afterAlias)

          -- Get explicit list
          explList = parseExplicitList afterHiding

      in ParsedImport
        { piModule = modName
        , piQualified = isQual
        , piAlias = alias
        , piHiding = isHiding
        , piExplicit = explList
        , piPackage = pkg
        , piSpan = Just $ mkSrcSpanRaw "<unknown>" ln 1 ln 1
        , piIsSafe = isSafe
        , piIsSource = False
        }

    isKeyword t = t `elem` ["as", "hiding", "qualified", "safe", "(", ")"]

    parseExplicitList :: [Text] -> [Text]
    parseExplicitList toks =
      let joined = T.unwords toks
          -- Extract content between parentheses
          content = case T.stripPrefix "(" joined of
            Just rest -> T.takeWhile (/= ')') rest
            Nothing -> ""
          -- Split by comma and clean up
          items = map T.strip $ T.splitOn "," content
      in filter (not . T.null) items

-- | Parse imports from a GHC parsed module
parseImportsFromModule :: HsModule GhcPs -> [ParsedImport]
parseImportsFromModule hsmod =
  map convertImportDecl (hsmodImports hsmod)

-- | Convert a GHC import declaration to our type
convertImportDecl :: LImportDecl GhcPs -> ParsedImport
convertImportDecl (L loc decl) = ParsedImport
  { piModule = T.pack $ moduleNameString $ unLoc $ ideclName decl
  , piQualified = isQualified (ideclQualified decl)
  , piAlias = fmap (T.pack . moduleNameString . unLoc) (ideclAs decl)
  , piHiding = isHiding (ideclImportList decl)
  , piExplicit = getExplicitList (ideclImportList decl)
  , piPackage = extractPkgQual (ideclPkgQual decl)
  , piSpan = locToSpan (locA loc)
  , piIsSafe = ideclSafe decl
  , piIsSource = ideclSource decl == IsBoot
  }
  where
    isQualified QualifiedPre = True
    isQualified QualifiedPost = True
    isQualified NotQualified = False

    isHiding Nothing = False
    isHiding (Just (EverythingBut, _)) = True
    isHiding (Just (Exactly, _)) = False

    getExplicitList Nothing = []
    getExplicitList (Just (_, L _ items)) = map extractIE items

    extractIE :: LIE GhcPs -> Text
    extractIE (L _ ie) = case ie of
      -- GHC 9.10: IE constructors: ext, name, exportdoc, [wildcard], [children]
      IEVar _ name _ -> T.pack $ occNameString $ rdrNameOcc $ ieWrappedName $ unLoc name
      IEThingAbs _ name _ -> T.pack $ occNameString $ rdrNameOcc $ ieWrappedName $ unLoc name
      IEThingAll _ name _ -> T.pack (occNameString (rdrNameOcc (ieWrappedName $ unLoc name)) <> "(..)")
      IEThingWith _ name _ _ _ ->
        T.pack (occNameString (rdrNameOcc (ieWrappedName $ unLoc name)) <> "(..)")
      IEModuleContents _ (L _ modName) -> "module " <> T.pack (moduleNameString modName)
      _ -> ""

    -- | Extract package qualifier (package imports are rare; we skip them for now)
    extractPkgQual :: a -> Maybe Text
    extractPkgQual _ = Nothing

    locToSpan :: GHC.SrcSpan -> Maybe SrcSpan
    locToSpan sp = case sp of
      GHC.RealSrcSpan rsp _ ->
        Just $ mkSrcSpanRaw
          (unpackFS $ GHC.srcSpanFile rsp)
          (GHC.srcSpanStartLine rsp)
          (GHC.srcSpanStartCol rsp)
          (GHC.srcSpanEndLine rsp)
          (GHC.srcSpanEndCol rsp)
      GHC.UnhelpfulSpan _ -> Nothing

-- | Get the span of the import section
getImportSpan :: [ParsedImport] -> Maybe (Int, Int)
getImportSpan [] = Nothing
getImportSpan imports =
  let spans = mapMaybe piSpan imports
  in if null spans
     then Nothing
     else Just (1, 1)  -- Placeholder, would need actual span calculation

--------------------------------------------------------------------------------
-- Modification
--------------------------------------------------------------------------------

-- | Add an import to the list
addImport :: FixImport -> [ParsedImport] -> [ParsedImport]
addImport fix imports =
  let newImport = fixImportToParseImport fix
  in mergeImports imports [newImport]

-- | Add multiple imports
addImports :: [FixImport] -> [ParsedImport] -> [ParsedImport]
addImports fixes imports =
  foldr addImport imports fixes

-- | Convert FixImport to ParsedImport
fixImportToParseImport :: FixImport -> ParsedImport
fixImportToParseImport FixImport{..} = ParsedImport
  { piModule = fimpModule
  , piQualified = fimpQualified /= Nothing
  , piAlias = fimpQualified
  , piHiding = fimpHiding
  , piExplicit = map isymName fimpSymbols
  , piPackage = fimpPackage
  , piSpan = Nothing
  , piIsSafe = False
  , piIsSource = False
  }

-- | Remove an import by module name
removeImport :: Text -> [ParsedImport] -> [ParsedImport]
removeImport modName = filter (\pImport -> piModule pImport /= modName)

-- | Remove a symbol from an import
removeImportSymbol :: Text -> Text -> [ParsedImport] -> [ParsedImport]
removeImportSymbol modName sym = map removeIfMatch
  where
    removeIfMatch pi
      | piModule pi == modName = pi { piExplicit = filter (/= sym) (piExplicit pi) }
      | otherwise = pi

-- | Qualify an import
qualifyImport :: Text -> Maybe Text -> [ParsedImport] -> [ParsedImport]
qualifyImport modName alias = map qualifyIfMatch
  where
    qualifyIfMatch pi
      | piModule pi == modName = pi { piQualified = True, piAlias = alias }
      | otherwise = pi

-- | Remove qualification from an import
unqualifyImport :: Text -> [ParsedImport] -> [ParsedImport]
unqualifyImport modName = map unqualifyIfMatch
  where
    unqualifyIfMatch pi
      | piModule pi == modName = pi { piQualified = False, piAlias = Nothing }
      | otherwise = pi

-- | Make an open import explicit by adding specific symbols
makeImportExplicit :: Text -> [Text] -> [ParsedImport] -> [ParsedImport]
makeImportExplicit modName syms = map makeExplicitIfMatch
  where
    makeExplicitIfMatch pi
      | piModule pi == modName && null (piExplicit pi) =
          pi { piExplicit = syms }
      | otherwise = pi

-- | Merge two import lists, combining explicit lists for same module
mergeImports :: [ParsedImport] -> [ParsedImport] -> [ParsedImport]
mergeImports existing new = deduplicateImports (existing ++ new)

--------------------------------------------------------------------------------
-- Generation
--------------------------------------------------------------------------------

-- | Render a single import to text
renderImport :: ParsedImport -> Text
renderImport ParsedImport{..} = T.intercalate " " $ filter (not . T.null)
  [ "import"
  , if piQualified then "qualified" else ""
  , if piIsSafe then "safe" else ""
  , maybe "" (\p -> "\"" <> p <> "\"") piPackage
  , piModule
  , maybe "" ("as " <>) piAlias
  , if piHiding then "hiding" else ""
  , renderExplicitList piExplicit
  ]

-- | Render explicit import list
renderExplicitList :: [Text] -> Text
renderExplicitList [] = ""
renderExplicitList syms = "(" <> T.intercalate ", " (sortSymbols syms) <> ")"

-- | Sort symbols for consistent rendering
sortSymbols :: [Text] -> [Text]
sortSymbols = sortBy compareSymbols
  where
    compareSymbols a b =
      let aIsType = case T.uncons a of
                      Just (c, _) -> c `elem` ['A'..'Z']
                      Nothing -> False
          bIsType = case T.uncons b of
                      Just (c, _) -> c `elem` ['A'..'Z']
                      Nothing -> False
      in case (aIsType, bIsType) of
        (True, False) -> LT  -- Types before functions
        (False, True) -> GT
        _ -> compare a b

-- | Render all imports to text
renderImports :: [ParsedImport] -> Text
renderImports imports =
  T.unlines $ map renderImport $ sortImports imports

-- | Replace imports in source text
replaceImports :: Text -> Int -> Int -> [ParsedImport] -> Text
replaceImports content startLine endLine newImports =
  let sourceLines = T.lines content
      beforeImports = take (startLine - 1) sourceLines
      afterImports = drop endLine sourceLines
      newImportLines = T.lines $ renderImports newImports
  in T.unlines $ beforeImports ++ newImportLines ++ afterImports

-- | Insert an import at the appropriate position
insertImport :: Text -> ParsedImport -> Text
insertImport content newImport =
  let sourceLines = T.lines content
      insertLine = findInsertPosition sourceLines (piModule newImport)
      (before, after) = splitAt insertLine sourceLines
      newLine = renderImport newImport
  in T.unlines $ before ++ [newLine] ++ after

-- | Find the best position to insert a new import
findInsertPosition :: [Text] -> Text -> Int
findInsertPosition sourceLines newModName =
  let importLineNums = findImportLines sourceLines
  in case importLineNums of
    [] -> findModuleEndLine sourceLines + 1
    _ ->
      -- Find where this import should go (alphabetically)
      let imports = zip importLineNums (map (sourceLines !!) (map pred importLineNums))
          insertPos = findAlphabeticPosition imports newModName
      in insertPos

-- | Find all import line numbers
findImportLines :: [Text] -> [Int]
findImportLines = map fst . filter (isImportLine . snd) . zip [1..]
  where
    isImportLine line =
      T.isPrefixOf "import" (T.stripStart line) &&
      not (T.isPrefixOf "--" (T.stripStart line))

-- | Find the line after module declaration
findModuleEndLine :: [Text] -> Int
findModuleEndLine sourceLines =
  case filter (("module" `T.isPrefixOf`) . T.stripStart . snd) (zip [1..] sourceLines) of
    [] -> 0
    ((n, _):_) -> n + 1

-- | Find alphabetic insertion position
findAlphabeticPosition :: [(Int, Text)] -> Text -> Int
findAlphabeticPosition imports newModName =
  case filter (\(_, line) -> extractModName line > newModName) imports of
    ((n, _):_) -> n
    [] -> case imports of
      [] -> 1
      _ -> fst (last imports) + 1
  where
    extractModName line =
      let tokens = T.words (T.stripStart line)
      in case dropWhile (`elem` ["import", "qualified", "safe"]) tokens of
        (m:_) -> m
        [] -> ""

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Sort imports by module name (with grouping)
sortImports :: [ParsedImport] -> [ParsedImport]
sortImports = sortBy (comparing sortKey)
  where
    sortKey pi = (moduleGroup (piModule pi), piModule pi)

    moduleGroup modName
      | "Prelude" `T.isInfixOf` modName = 0 :: Int
      | "Control" `T.isPrefixOf` modName = 1
      | "Data" `T.isPrefixOf` modName = 2
      | "GHC" `T.isPrefixOf` modName = 3
      | "System" `T.isPrefixOf` modName = 4
      | otherwise = 5

-- | Group imports by category
groupImports :: [ParsedImport] -> [[ParsedImport]]
groupImports imports =
  let sorted = sortImports imports
  in groupBy sameGroup sorted
  where
    sameGroup a b = moduleGroup (piModule a) == moduleGroup (piModule b)

    moduleGroup :: Text -> Int
    moduleGroup modName
      | "Prelude" `T.isInfixOf` modName = 0
      | isStandardLib modName = 1
      | otherwise = 2

    isStandardLib m = any (`T.isPrefixOf` m)
      ["Control", "Data", "GHC", "System", "Foreign", "Numeric", "Text."]

-- | Remove duplicate imports (merging explicit lists)
--
-- Uses NonEmpty to guarantee that groups are never empty, eliminating
-- the need for partial pattern matching or error calls.
--
-- IMPORTANT: Only merges imports that have:
-- 1. Same module name
-- 2. Same qualification status (both qualified or both unqualified)
-- 3. Same alias (both have same "as X" or both have none)
--
-- This prevents incorrectly merging:
-- - `import Data.Map (fromList)` with `import qualified Data.Map as M`
-- - `import qualified Data.Map as M` with `import qualified Data.Map as Map`
deduplicateImports :: [ParsedImport] -> [ParsedImport]
deduplicateImports = map mergeGroup . NE.groupBy sameImportStyle . sortBy (comparing importKey)
  where
    -- Key for sorting: module name, then qualified status, then alias
    importKey :: ParsedImport -> (Text, Bool, Maybe Text)
    importKey pi = (piModule pi, piQualified pi, piAlias pi)

    -- Only group imports with same module, qualification, and alias
    sameImportStyle :: ParsedImport -> ParsedImport -> Bool
    sameImportStyle a b = piModule a == piModule b
                       && piQualified a == piQualified b
                       && piAlias a == piAlias b

    -- | Merge a non-empty group of imports for the same module/style
    --
    -- The NonEmpty type guarantees we always have at least one import,
    -- making the function total without needing error cases.
    mergeGroup :: NonEmpty ParsedImport -> ParsedImport
    mergeGroup (x :| []) = x
    mergeGroup (x :| xs) = x
      { piExplicit = nubBy (==) $ concatMap piExplicit (x : xs)
      -- piQualified and piAlias are already the same for all items in group
      }

-- | Find an import that provides a symbol
findImportForSymbol :: Text -> [ParsedImport] -> Maybe ParsedImport
findImportForSymbol sym = listToMaybe . filter (`importsSymbol` sym)

-- | Check if an import provides a symbol
importsSymbol :: ParsedImport -> Text -> Bool
importsSymbol pi sym
  | null (piExplicit pi) && not (piHiding pi) = True  -- Open import
  | piHiding pi = sym `notElem` piExplicit pi
  | otherwise = sym `elem` piExplicit pi
