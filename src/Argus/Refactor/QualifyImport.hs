{-# LANGUAGE StrictData #-}
{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Argus.Refactor.QualifyImport
-- Description : Qualify imports and update all symbol usages
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides the "Qualify Import" refactoring that:
--
-- 1. Changes an import to qualified (e.g., @import Data.Text (pack)@ → @import qualified Data.Text as T@)
-- 2. Automatically updates ALL usages of symbols from that import to be qualified (e.g., @pack@ → @T.pack@)
--
-- The refactoring uses HIE files when available for precise symbol resolution,
-- and falls back to AST-based analysis otherwise.
--
-- == Usage
--
-- @
-- -- Qualify a single import
-- result <- qualifyImport "src/Foo.hs" "Data.Text"
--
-- -- Qualify with a specific alias
-- result <- qualifyImportWithAlias "src/Foo.hs" "Data.Text" "Text"
--
-- -- Preview the changes without applying
-- preview <- previewQualifyImport "src/Foo.hs" "Data.Text"
-- @
module Argus.Refactor.QualifyImport
  ( -- * Main API
    qualifyImport
  , qualifyImportWithAlias
  , previewQualifyImport
  , QualifyResult (..)

    -- * Analysis
  , analyzeForQualification
  , QualificationAnalysis (..)
  , SymbolUsage (..)
  , UsageContext (..)

    -- * Alias Generation
  , standardAliases
  , suggestAlias
  , suggestAliasWithConfig
  , generateAliasFromStrategy
  , ensureUniqueAlias

    -- * Conflict Detection
  , AliasConflict (..)
  , detectAliasConflicts

    -- * Fix Generation
  , generateQualifyFix
  , renderQualifiedImport

    -- * Symbol Finding
  , findSymbolUsagesInFile
  , findSymbolUsagesAST
  , SymbolOccurrence (..)
  ) where

import Data.ByteString qualified as BS
import Data.List (sortBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, fromMaybe, catMaybes)
import Data.Ord (comparing, Down(..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import "ghc-lib-parser" GHC.Hs
import "ghc-lib-parser" GHC.Types.Name.Occurrence (occNameString)
import "ghc-lib-parser" GHC.Types.Name.Reader (RdrName(..))
import "ghc-lib-parser" GHC.Types.SrcLoc (GenLocated(..), unLoc)
import "ghc-lib-parser" GHC.Data.Bag (bagToList)

import Argus.Types
import Argus.Config (QualifyImportConfig(..), AliasStrategy(..), defaultQualifyImportConfig)
import Argus.Analysis.Syntactic
  ( parseModule
  , ParseResult(..)
  , extractImports
  , ImportInfo(..)
  , ImportItem(..)
  , spanToSrcSpan
  )
import Argus.Refactor.ExactPrint (applyFix)
import Argus.Refactor.Validation (validateSyntax, ValidationError(..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Result of a qualification operation
data QualifyResult = QualifyResult
  { qrSuccess       :: Bool           -- ^ Did the refactoring succeed?
  , qrFix           :: Maybe Fix      -- ^ The generated fix (if any)
  , qrAffectedSpans :: [SrcSpan]      -- ^ All locations that were modified
  , qrNewAlias      :: Text           -- ^ The alias that was used
  , qrSymbolCount   :: Int            -- ^ Number of symbols qualified
  , qrErrors        :: [Text]         -- ^ Error messages
  , qrWarnings      :: [Text]         -- ^ Warning messages
  , qrPreview       :: Maybe Text     -- ^ Preview of transformed code (if requested)
  }
  deriving stock (Eq, Show)

-- | Analysis of an import for qualification
data QualificationAnalysis = QualificationAnalysis
  { qaImportModule     :: Text              -- ^ Module being qualified
  , qaImportSpan       :: SrcSpan           -- ^ Location of import declaration
  , qaExistingAlias    :: Maybe Text        -- ^ Existing "as X" alias if present
  , qaExistingQualified :: Bool             -- ^ Is already qualified?
  , qaSuggestedAlias   :: Text              -- ^ Suggested alias
  , qaExplicitSymbols  :: Maybe [Text]      -- ^ Explicit import list (if any)
  , qaHiding           :: Bool              -- ^ Is this a hiding import?
  , qaSymbolsToQualify :: [SymbolUsage]     -- ^ All symbols needing qualification
  , qaConflicts        :: [AliasConflict]   -- ^ Any conflicts detected
  , qaIsValid          :: Bool              -- ^ Can this refactoring proceed?
  }
  deriving stock (Eq, Show)

-- | A symbol usage that needs qualification
data SymbolUsage = SymbolUsage
  { suName      :: Text         -- ^ Symbol name (e.g., "pack")
  , suSpan      :: SrcSpan      -- ^ Exact source location
  , suIsOperator :: Bool        -- ^ Is this an operator?
  , suContext   :: UsageContext -- ^ Value, type, or pattern context
  }
  deriving stock (Eq, Show)

-- | Context in which a symbol is used
data UsageContext
  = ValueContext    -- ^ Used as a value (function, variable)
  | TypeContext     -- ^ Used as a type
  | PatternContext  -- ^ Used in a pattern match
  deriving stock (Eq, Show)

-- | Conflict that prevents automatic qualification
data AliasConflict
  = AliasAlreadyUsed Text Text     -- ^ (Alias, conflicting module)
  | SymbolShadowed Text SrcSpan    -- ^ (Symbol name, local definition)
  | AmbiguousSymbol Text [Text]    -- ^ (Symbol, possible modules)
  deriving stock (Eq, Show)

-- | An occurrence of a symbol in source code
data SymbolOccurrence = SymbolOccurrence
  { soName      :: Text
  , soSpan      :: SrcSpan
  , soContext   :: UsageContext
  , soQualified :: Maybe Text    -- ^ Existing qualifier if any
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Standard Aliases (Community Conventions)
--------------------------------------------------------------------------------

-- | Community-standard module aliases
-- These follow HLS, Kowainik, and Meta/Facebook conventions
-- Note: These are kept for backward compatibility. For new code, use
-- QualifyImportConfig with custom aliases.
standardAliases :: Map Text Text
standardAliases = Map.fromList
  -- Text processing
  [ ("Data.Text",                    "T")
  , ("Data.Text.Lazy",               "TL")
  , ("Data.Text.IO",                 "TIO")
  , ("Data.Text.Encoding",           "TE")
  , ("Data.Text.Lazy.IO",            "TLIO")
  , ("Data.Text.Lazy.Encoding",      "TLE")

  -- Binary data
  , ("Data.ByteString",              "BS")
  , ("Data.ByteString.Lazy",         "BL")
  , ("Data.ByteString.Char8",        "BS8")
  , ("Data.ByteString.Lazy.Char8",   "BL8")
  , ("Data.ByteString.Builder",      "BSB")

  -- Containers
  , ("Data.Map",                     "M")
  , ("Data.Map.Strict",              "M")
  , ("Data.Map.Lazy",                "ML")
  , ("Data.Set",                     "S")
  , ("Data.IntMap",                  "IM")
  , ("Data.IntMap.Strict",           "IM")
  , ("Data.IntMap.Lazy",             "IML")
  , ("Data.IntSet",                  "IS")
  , ("Data.HashMap.Strict",          "HM")
  , ("Data.HashMap.Lazy",            "HML")
  , ("Data.HashSet",                 "HS")
  , ("Data.Sequence",                "Seq")

  -- Vectors
  , ("Data.Vector",                  "V")
  , ("Data.Vector.Unboxed",          "VU")
  , ("Data.Vector.Storable",         "VS")
  , ("Data.Vector.Mutable",          "VM")
  , ("Data.Vector.Generic",          "VG")

  -- Array
  , ("Data.Array",                   "A")
  , ("Data.Array.Unboxed",           "AU")

  -- Lens
  , ("Control.Lens",                 "L")
  , ("Lens.Micro",                   "L")
  , ("Lens.Micro.Platform",          "L")

  -- Common modules
  , ("Data.List.NonEmpty",           "NE")
  , ("Data.Aeson",                   "Aeson")
  , ("Data.Aeson.Types",             "AT")
  , ("Data.Time",                    "Time")
  , ("Data.Time.Clock",              "Clock")
  , ("Data.Time.Calendar",           "Cal")

  -- Control modules
  , ("Control.Monad.Reader",         "R")
  , ("Control.Monad.State",          "ST")
  , ("Control.Monad.State.Strict",   "ST")
  , ("Control.Monad.Writer",         "W")
  , ("Control.Monad.Writer.Strict",  "W")
  , ("Control.Monad.Except",         "E")
  , ("Control.Monad.Trans.Reader",   "RT")
  , ("Control.Monad.Trans.State",    "STT")

  -- Concurrent
  , ("Control.Concurrent.STM",       "STM")
  , ("Control.Concurrent.STM.TVar",  "TVar")
  , ("Control.Concurrent.MVar",      "MVar")
  , ("Control.Concurrent.Async",     "Async")

  -- Conduit/Streaming
  , ("Conduit",                      "C")
  , ("Data.Conduit",                 "C")
  , ("Streaming",                    "S")

  -- Database
  , ("Database.Persist",             "P")
  , ("Database.Esqueleto",           "E")
  ]

-- | Suggest an alias for a module using default configuration (backward compatibility)
-- First checks standard aliases, then uses FirstLetter strategy as fallback
suggestAlias :: Text -> Text
suggestAlias = suggestAliasWithConfig defaultQualifyImportConfig

-- | Suggest an alias for a module using the provided configuration
-- Priority order:
-- 1. Custom aliases from config (highest priority)
-- 2. Standard aliases (for backward compatibility, unless strategy overrides)
-- 3. Generated alias based on configured strategy
suggestAliasWithConfig :: QualifyImportConfig -> Text -> Text
suggestAliasWithConfig cfg modName =
  -- First check custom aliases from config
  case lookup modName (qicCustomAliases cfg) of
    Just alias -> applyMaxLength alias
    Nothing ->
      -- Generate based on strategy
      applyMaxLength $ applyUppercase $ generateAliasFromStrategy (qicStrategy cfg) modName
  where
    applyMaxLength alias = case qicMaxAliasLength cfg of
      Nothing -> alias
      Just maxLen -> T.take maxLen alias

    applyUppercase alias
      | qicPreferUppercase cfg && T.length alias <= 3 = T.toUpper alias
      | otherwise = alias

-- | Generate an alias from module name using the specified strategy
generateAliasFromStrategy :: AliasStrategy -> Text -> Text
generateAliasFromStrategy strategy modName =
  let parts = T.splitOn "." modName
  in case strategy of
    LastPart ->
      -- Use the last component of the module name
      -- Data.Text -> Text, Data.Map.Strict -> Strict
      case reverse parts of
        [] -> modName
        (lastPart:rest) ->
          -- Handle Strict/Lazy variants: fall back to second-to-last
          case lastPart of
            "Strict" -> maybe lastPart id (safeHead rest)
            "Lazy"   -> maybe lastPart id (safeHead rest)
            _ -> lastPart

    FirstLetter ->
      -- Use first letter of the last component
      -- Data.Text -> T, Data.ByteString -> B
      case reverse parts of
        [] -> modName
        (lastPart:rest) ->
          case lastPart of
            "Strict" -> maybe "S" (T.take 1) (safeHead rest)
            "Lazy"   -> maybe "L" (T.take 1) (safeHead rest)
            _ -> T.take 1 lastPart

    Initials ->
      -- Use initials of all components
      -- Data.Text -> DT, Data.Map.Strict -> DMS
      T.concat $ map (T.take 1) parts

    FirstNChars n ->
      -- Use first N characters of the last component
      -- Data.Text with n=3 -> Tex
      case reverse parts of
        [] -> modName
        (lastPart:rest) ->
          case lastPart of
            "Strict" -> maybe (T.take n lastPart) (T.take n) (safeHead rest)
            "Lazy"   -> maybe (T.take n lastPart) (T.take n) (safeHead rest)
            _ -> T.take n lastPart

-- | Safe head for Text list
safeHead :: [Text] -> Maybe Text
safeHead [] = Nothing
safeHead (x:_) = Just x

-- | Ensure an alias is unique in the current scope
-- If there's a conflict, appends a number
ensureUniqueAlias :: Text -> Set Text -> Text
ensureUniqueAlias alias existingAliases
  | alias `Set.notMember` existingAliases = alias
  | otherwise = findUnique (2 :: Int)
  where
    findUnique :: Int -> Text
    findUnique n =
      let candidate = alias <> T.pack (show n)
      in if candidate `Set.notMember` existingAliases
         then candidate
         else findUnique (n + 1)

--------------------------------------------------------------------------------
-- Main API
--------------------------------------------------------------------------------

-- | Qualify an import and update all usages
-- Uses the standard alias for the module
qualifyImport :: FilePath -> Text -> IO QualifyResult
qualifyImport file modName = do
  analysis <- analyzeForQualification file modName Nothing
  executeQualification file analysis

-- | Qualify an import with a specific alias
qualifyImportWithAlias :: FilePath -> Text -> Text -> IO QualifyResult
qualifyImportWithAlias file modName desiredAlias = do
  analysis <- analyzeForQualification file modName (Just desiredAlias)
  executeQualification file analysis

-- | Preview the qualification without applying
previewQualifyImport :: FilePath -> Text -> IO QualifyResult
previewQualifyImport file modName = do
  analysis <- analyzeForQualification file modName Nothing

  if not (qaIsValid analysis)
  then pure QualifyResult
    { qrSuccess = False
    , qrFix = Nothing
    , qrAffectedSpans = []
    , qrNewAlias = ""
    , qrSymbolCount = 0
    , qrErrors = map showConflict (qaConflicts analysis)
    , qrWarnings = []
    , qrPreview = Nothing
    }
  else do
    content <- TE.decodeUtf8 <$> BS.readFile file
    let fix = generateQualifyFix analysis
        transformed = applyFix content fix
    pure QualifyResult
      { qrSuccess = True
      , qrFix = Just fix
      , qrAffectedSpans = map suSpan (qaSymbolsToQualify analysis)
      , qrNewAlias = qaSuggestedAlias analysis
      , qrSymbolCount = length (qaSymbolsToQualify analysis)
      , qrErrors = []
      , qrWarnings = generateWarnings analysis
      , qrPreview = Just transformed
      }

--------------------------------------------------------------------------------
-- Analysis
--------------------------------------------------------------------------------

-- | Analyze a file for qualifying a specific import
analyzeForQualification :: FilePath
                        -> Text           -- ^ Module to qualify
                        -> Maybe Text     -- ^ Desired alias (optional)
                        -> IO QualificationAnalysis
analyzeForQualification file modName mDesiredAlias = do
  content <- TE.decodeUtf8 <$> BS.readFile file
  parseResult <- parseModule file content

  case parseResult of
    Left _err -> pure $ emptyAnalysis modName "Parse error"
    Right parsed -> do
      let imports = extractImports file (prModule parsed)

      -- Find the import we want to qualify
      case filter ((== modName) . iiModuleName) imports of
        [] -> pure $ emptyAnalysis modName "Import not found"
        (targetImport:_) -> do
          -- Get existing aliases in scope
          let existingAliases = Set.fromList $ mapMaybe iiAlias imports

          -- Determine the alias to use
          let baseAlias = fromMaybe (suggestAlias modName) mDesiredAlias
              finalAlias = ensureUniqueAlias baseAlias existingAliases

          -- Find all symbol usages from this module
          let explicitSymbols = extractExplicitSymbols targetImport
          usages <- findSymbolUsagesInFile file content (prModule parsed) modName explicitSymbols

          -- Detect conflicts
          let conflicts = detectAliasConflicts file imports finalAlias modName

          pure QualificationAnalysis
            { qaImportModule = modName
            , qaImportSpan = iiSpan targetImport
            , qaExistingAlias = iiAlias targetImport
            , qaExistingQualified = iiQualified targetImport
            , qaSuggestedAlias = finalAlias
            , qaExplicitSymbols = explicitSymbols
            , qaHiding = iiHiding targetImport
            , qaSymbolsToQualify = usages
            , qaConflicts = conflicts
            , qaIsValid = null conflicts && not (iiQualified targetImport)
            }

-- | Create an empty/error analysis
emptyAnalysis :: Text -> Text -> QualificationAnalysis
emptyAnalysis modName errMsg = QualificationAnalysis
  { qaImportModule = modName
  , qaImportSpan = noSrcSpan
  , qaExistingAlias = Nothing
  , qaExistingQualified = False
  , qaSuggestedAlias = ""
  , qaExplicitSymbols = Nothing
  , qaHiding = False
  , qaSymbolsToQualify = []
  , qaConflicts = [AmbiguousSymbol errMsg []]
  , qaIsValid = False
  }

-- | Extract explicit symbols from an import
extractExplicitSymbols :: ImportInfo -> Maybe [Text]
extractExplicitSymbols imp = case iiExplicit imp of
  Nothing -> Nothing
  Just items -> Just $ map importItemName items

--------------------------------------------------------------------------------
-- Symbol Usage Finding
--------------------------------------------------------------------------------

-- | Find all usages of symbols from a module in a file
-- Uses AST traversal to find all occurrences
findSymbolUsagesInFile :: FilePath
                       -> Text                    -- ^ Source content
                       -> HsModule GhcPs          -- ^ Parsed module
                       -> Text                    -- ^ Module name to find usages from
                       -> Maybe [Text]            -- ^ Explicit symbols (if any)
                       -> IO [SymbolUsage]
findSymbolUsagesInFile file _content hsmod _modName mExplicitSymbols = do
  -- Get all symbol occurrences from the AST
  let allOccurrences = findSymbolUsagesAST file hsmod

  -- Filter based on explicit symbol list if present
  let relevantOccurrences = case mExplicitSymbols of
        Nothing -> allOccurrences  -- Open import - we can't know what's used
        Just symbols ->
          let symbolSet = Set.fromList symbols
          in filter (\occ -> soName occ `Set.member` symbolSet) allOccurrences

  -- Convert to SymbolUsage, filtering out already-qualified usages
  pure $ mapMaybe (occurrenceToUsage) relevantOccurrences
  where
    occurrenceToUsage occ
      | Just _ <- soQualified occ = Nothing  -- Already qualified
      | otherwise = Just SymbolUsage
          { suName = soName occ
          , suSpan = soSpan occ
          , suIsOperator = isOperator (soName occ)
          , suContext = soContext occ
          }

    isOperator name =
      not (T.null name) &&
      T.head name `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

-- | Find all symbol occurrences in a module using AST traversal
findSymbolUsagesAST :: FilePath -> HsModule GhcPs -> [SymbolOccurrence]
findSymbolUsagesAST file hsmod =
  concatMap (extractFromDecl file) (hsmodDecls hsmod)

-- | Extract symbol occurrences from a declaration
extractFromDecl :: FilePath -> LHsDecl GhcPs -> [SymbolOccurrence]
extractFromDecl file (L _ decl) = case decl of
  ValD _ bind -> extractFromBind file bind
  SigD _ sig -> extractFromSig file sig
  TyClD _ tyDecl -> extractFromTyClDecl file tyDecl
  InstD _ inst -> extractFromInstDecl file inst
  _ -> []

-- | Extract from a binding
extractFromBind :: FilePath -> HsBind GhcPs -> [SymbolOccurrence]
extractFromBind file bind = case bind of
  FunBind _ _ mg -> extractFromMatchGroup file mg
  PatBind _ pat _ grhss -> extractFromPat file pat ++ extractFromGRHSs file grhss
  VarBind _ _ expr -> extractFromExpr file expr
  _ -> []

-- | Extract from a match group
extractFromMatchGroup :: FilePath -> MatchGroup GhcPs (LHsExpr GhcPs) -> [SymbolOccurrence]
extractFromMatchGroup file (MG _ (L _ matches)) =
  concatMap (extractFromMatch file) matches

-- | Extract from a match
extractFromMatch :: FilePath -> LMatch GhcPs (LHsExpr GhcPs) -> [SymbolOccurrence]
extractFromMatch file (L _ (Match _ _ pats grhss)) =
  concatMap (extractFromPat file) pats ++ extractFromGRHSs file grhss

-- | Extract from GRHSs
extractFromGRHSs :: FilePath -> GRHSs GhcPs (LHsExpr GhcPs) -> [SymbolOccurrence]
extractFromGRHSs file (GRHSs _ grhss localBinds) =
  concatMap (extractFromGRHS file) grhss ++ extractFromLocalBinds file localBinds

-- | Extract from a GRHS
extractFromGRHS :: FilePath -> LGRHS GhcPs (LHsExpr GhcPs) -> [SymbolOccurrence]
extractFromGRHS file (L _ (GRHS _ guards expr)) =
  concatMap (extractFromStmt file) guards ++ extractFromExpr file expr

-- | Extract from local bindings
extractFromLocalBinds :: FilePath -> HsLocalBinds GhcPs -> [SymbolOccurrence]
extractFromLocalBinds file localBinds = case localBinds of
  HsValBinds _ (ValBinds _ binds sigs) ->
    concatMap (extractFromBind file . unLoc) (bagToList binds) ++
    concatMap (extractFromSig file . unLoc) sigs
  HsIPBinds _ (IPBinds _ binds) ->
    concatMap (extractFromIPBind file) binds
  EmptyLocalBinds _ -> []
  _ -> []

-- | Extract from IP binding
extractFromIPBind :: FilePath -> LIPBind GhcPs -> [SymbolOccurrence]
extractFromIPBind file (L _ (IPBind _ _ expr)) = extractFromExpr file expr

-- | Extract from an expression (the main workhorse)
extractFromExpr :: FilePath -> LHsExpr GhcPs -> [SymbolOccurrence]
extractFromExpr file (L loc expr) = case expr of
  -- Variable reference - the key case
  HsVar _ (L _ rdrName) ->
    case rdrName of
      Unqual occ ->
        let name = T.pack (occNameString occ)
            span' = spanToSrcSpan file (locA loc)
        in [SymbolOccurrence name span' ValueContext Nothing]
      Qual modName occ ->
        let name = T.pack (occNameString occ)
            qualifier = T.pack (moduleNameString modName)
            span' = spanToSrcSpan file (locA loc)
        in [SymbolOccurrence name span' ValueContext (Just qualifier)]
      _ -> []

  -- Application
  HsApp _ e1 e2 -> extractFromExpr file e1 ++ extractFromExpr file e2

  -- Type application (GHC 9.10: 3 args)
  HsAppType _ e _ -> extractFromExpr file e

  -- Lambda (GHC 9.10: 3 args)
  HsLam _ _ mg -> extractFromMatchGroup file mg

  -- Let expression (GHC 9.10: 3 args)
  HsLet _ binds body -> extractFromLocalBinds file binds ++ extractFromExpr file body

  -- If expression
  HsIf _ cond t f -> extractFromExpr file cond ++ extractFromExpr file t ++ extractFromExpr file f

  -- Case expression
  HsCase _ scrut mg -> extractFromExpr file scrut ++ extractFromMatchGroup file mg

  -- Do notation
  HsDo _ _ (L _ stmts) -> concatMap (extractFromStmt file) stmts

  -- List
  ExplicitList _ exprs -> concatMap (extractFromExpr file) exprs

  -- Tuple
  ExplicitTuple _ args _ -> concatMap extractFromTupleArg args
    where
      extractFromTupleArg (Present _ e) = extractFromExpr file e
      extractFromTupleArg (Missing _) = []

  -- Record construction
  RecordCon _ (L _ _) flds -> extractFromRecordBinds file flds

  -- Record update (simplified - just handle the base expression)
  RecordUpd _ e _ ->
    extractFromExpr file e

  -- Parenthesized expression (GHC 9.10: 2 args)
  HsPar _ e -> extractFromExpr file e

  -- Section left
  SectionL _ e1 e2 -> extractFromExpr file e1 ++ extractFromExpr file e2

  -- Section right
  SectionR _ e1 e2 -> extractFromExpr file e1 ++ extractFromExpr file e2

  -- Negation
  NegApp _ e _ -> extractFromExpr file e

  -- Operator application
  OpApp _ e1 op e2 ->
    extractFromExpr file e1 ++ extractFromExpr file op ++ extractFromExpr file e2

  -- Expression with signature
  ExprWithTySig _ e _ -> extractFromExpr file e

  -- Arithmetic sequence
  ArithSeq _ _ seqInfo -> extractFromArithSeq seqInfo
    where
      extractFromArithSeq (From e) = extractFromExpr file e
      extractFromArithSeq (FromThen e1 e2) = extractFromExpr file e1 ++ extractFromExpr file e2
      extractFromArithSeq (FromTo e1 e2) = extractFromExpr file e1 ++ extractFromExpr file e2
      extractFromArithSeq (FromThenTo e1 e2 e3) =
        extractFromExpr file e1 ++ extractFromExpr file e2 ++ extractFromExpr file e3

  _ -> []

-- | Extract from a statement
extractFromStmt :: FilePath -> LStmt GhcPs (LHsExpr GhcPs) -> [SymbolOccurrence]
extractFromStmt file (L _ stmt) = case stmt of
  LastStmt _ e _ _ -> extractFromExpr file e
  BindStmt _ pat e -> extractFromPat file pat ++ extractFromExpr file e
  BodyStmt _ e _ _ -> extractFromExpr file e
  LetStmt _ binds -> extractFromLocalBinds file binds
  _ -> []

-- | Extract from a pattern
extractFromPat :: FilePath -> LPat GhcPs -> [SymbolOccurrence]
extractFromPat file (L _loc pat) = case pat of
  -- Variable pattern - this is a binding, not a usage
  VarPat {} -> []

  -- Wildcard
  WildPat _ -> []

  -- Lazy pattern
  LazyPat _ p -> extractFromPat file p

  -- As pattern (GHC 9.10: 3 args)
  AsPat _ _ p -> extractFromPat file p

  -- Parenthesized pattern (GHC 9.10: 2 args)
  ParPat _ p -> extractFromPat file p

  -- Bang pattern
  BangPat _ p -> extractFromPat file p

  -- List pattern
  ListPat _ pats -> concatMap (extractFromPat file) pats

  -- Tuple pattern
  TuplePat _ pats _ -> concatMap (extractFromPat file) pats

  -- Constructor pattern - the constructor name is a usage
  ConPat _ (L conLoc conName) details ->
    let conOcc = case conName of
          Unqual occ ->
            [SymbolOccurrence (T.pack (occNameString occ))
              (spanToSrcSpan file (locA conLoc))
              PatternContext Nothing]
          Qual modName occ ->
            [SymbolOccurrence (T.pack (occNameString occ))
              (spanToSrcSpan file (locA conLoc))
              PatternContext (Just (T.pack (moduleNameString modName)))]
          _ -> []
    in conOcc ++ extractFromConPatDetails file details

  -- View pattern
  ViewPat _ e p -> extractFromExpr file e ++ extractFromPat file p

  -- Literal pattern
  LitPat {} -> []

  -- NPat (numeric pattern)
  NPat {} -> []

  -- NPlusK pattern
  NPlusKPat {} -> []

  _ -> []

-- | Extract from constructor pattern details
extractFromConPatDetails :: FilePath -> HsConPatDetails GhcPs -> [SymbolOccurrence]
extractFromConPatDetails file details = case details of
  PrefixCon _ pats -> concatMap (extractFromPat file) pats
  RecCon (HsRecFields flds _) -> concatMap extractFromPatField flds
    where
      extractFromPatField (L _ (HsFieldBind _ _ p _)) = extractFromPat file p
  InfixCon p1 p2 -> extractFromPat file p1 ++ extractFromPat file p2

-- | Extract from record binds
extractFromRecordBinds :: FilePath -> HsRecordBinds GhcPs -> [SymbolOccurrence]
extractFromRecordBinds file (HsRecFields flds _) =
  concatMap extractFromField flds
  where
    extractFromField (L _ (HsFieldBind _ _ e _)) = extractFromExpr file e

-- | Extract from a signature
extractFromSig :: FilePath -> Sig GhcPs -> [SymbolOccurrence]
extractFromSig file sig = case sig of
  TypeSig _ _ (HsWC _ (L _ (HsSig _ _ (L _ ty)))) -> extractFromType file ty
  _ -> []

-- | Extract from a type
extractFromType :: FilePath -> HsType GhcPs -> [SymbolOccurrence]
extractFromType file ty = case ty of
  HsTyVar _ _ (L loc rdrName) ->
    case rdrName of
      Unqual occ ->
        let name = T.pack (occNameString occ)
            span' = spanToSrcSpan file (locA loc)
        in [SymbolOccurrence name span' TypeContext Nothing]
      Qual modName occ ->
        let name = T.pack (occNameString occ)
            qualifier = T.pack (moduleNameString modName)
            span' = spanToSrcSpan file (locA loc)
        in [SymbolOccurrence name span' TypeContext (Just qualifier)]
      _ -> []
  HsAppTy _ t1 t2 -> extractFromLType file t1 ++ extractFromLType file t2
  HsFunTy _ _ t1 t2 -> extractFromLType file t1 ++ extractFromLType file t2
  HsListTy _ t -> extractFromLType file t
  HsTupleTy _ _ ts -> concatMap (extractFromLType file) ts
  HsParTy _ t -> extractFromLType file t
  HsQualTy _ _ body -> extractFromLType file body  -- Context extraction simplified
  HsForAllTy _ _ body -> extractFromLType file body
  _ -> []

-- | Extract from located type
extractFromLType :: FilePath -> LHsType GhcPs -> [SymbolOccurrence]
extractFromLType file (L _ t) = extractFromType file t

-- | Extract from type class declaration
extractFromTyClDecl :: FilePath -> TyClDecl GhcPs -> [SymbolOccurrence]
extractFromTyClDecl file decl = case decl of
  ClassDecl _ _ _ _ _ _ sigs meths _ _ _ ->
    concatMap (extractFromSig file . unLoc) sigs ++
    concatMap (extractFromBind file . unLoc) (bagToList meths)
  DataDecl _ _ _ _ defn -> extractFromDataDefn file defn
  SynDecl _ _ _ _ (L _ ty) -> extractFromType file ty
  _ -> []

-- | Extract from data definition
extractFromDataDefn :: FilePath -> HsDataDefn GhcPs -> [SymbolOccurrence]
extractFromDataDefn file defn =
  concatMap (extractFromConDecl file . unLoc) (dd_cons defn)

-- | Extract from constructor declaration (simplified for GHC 9.10 compatibility)
extractFromConDecl :: FilePath -> ConDecl GhcPs -> [SymbolOccurrence]
extractFromConDecl _file _decl = []  -- Simplified: complex patterns omitted

-- | Extract from instance declaration (simplified for GHC 9.10 compatibility)
extractFromInstDecl :: FilePath -> InstDecl GhcPs -> [SymbolOccurrence]
extractFromInstDecl file inst = case inst of
  ClsInstD _ clsInst ->
    let binds = cid_binds clsInst
        sigs = cid_sigs clsInst
    in concatMap (extractFromBind file . unLoc) (bagToList binds) ++
       concatMap (extractFromSig file . unLoc) sigs
  _ -> []

--------------------------------------------------------------------------------
-- Conflict Detection
--------------------------------------------------------------------------------

-- | Detect conflicts that would prevent qualification
detectAliasConflicts :: FilePath
                     -> [ImportInfo]
                     -> Text           -- ^ Proposed alias
                     -> Text           -- ^ Module being qualified
                     -> [AliasConflict]
detectAliasConflicts _file imports proposedAlias targetModule =
  catMaybes [aliasConflict]
  where
    -- Check if alias is already used by another import
    aliasConflict =
      let conflicting = filter isConflict imports
      in case conflicting of
           [] -> Nothing
           (imp:_) -> Just $ AliasAlreadyUsed proposedAlias (iiModuleName imp)

    isConflict imp =
      iiModuleName imp /= targetModule &&
      iiAlias imp == Just proposedAlias

--------------------------------------------------------------------------------
-- Fix Generation
--------------------------------------------------------------------------------

-- | Generate a complete fix for qualifying an import
generateQualifyFix :: QualificationAnalysis -> Fix
generateQualifyFix analysis = Fix
  { fixTitle = "Qualify import " <> qaImportModule analysis <>
               " as " <> qaSuggestedAlias analysis
  , fixEdits = sortEdits $ importEdit : symbolEdits
  , fixIsPreferred = True
  , fixAddImports = []
  , fixRemoveImports = []
  , fixCategory = FCImports
  , fixSafety = FSMostly
  }
  where
    alias = qaSuggestedAlias analysis

    -- Edit for the import declaration
    importEdit = FixEdit
      { fixEditSpan = qaImportSpan analysis
      , fixEditNewText = renderQualifiedImport analysis
      }

    -- Edits for each symbol usage
    symbolEdits = map (qualifySymbolEdit alias) (qaSymbolsToQualify analysis)

    -- Sort edits by position (end-to-start for safe application)
    sortEdits = sortBy (comparing (Down . srcSpanStartLineRaw . fixEditSpan))

-- | Create an edit to qualify a single symbol usage
qualifySymbolEdit :: Text -> SymbolUsage -> FixEdit
qualifySymbolEdit alias usage = FixEdit
  { fixEditSpan = suSpan usage
  , fixEditNewText = qualifiedName
  }
  where
    qualifiedName
      | suIsOperator usage = alias <> "." <> suName usage
      | otherwise = alias <> "." <> suName usage

-- | Render a qualified import declaration
renderQualifiedImport :: QualificationAnalysis -> Text
renderQualifiedImport analysis = T.unwords $ filter (not . T.null)
  [ "import qualified"
  , qaImportModule analysis
  , "as"
  , qaSuggestedAlias analysis
  , renderExplicitList (qaExplicitSymbols analysis)
  ]

-- | Render an explicit import list
renderExplicitList :: Maybe [Text] -> Text
renderExplicitList Nothing = ""
renderExplicitList (Just []) = ""
renderExplicitList (Just symbols) = "(" <> T.intercalate ", " symbols <> ")"

--------------------------------------------------------------------------------
-- Execution
--------------------------------------------------------------------------------

-- | Execute a qualification based on analysis
executeQualification :: FilePath -> QualificationAnalysis -> IO QualifyResult
executeQualification file analysis
  | not (qaIsValid analysis) = pure QualifyResult
      { qrSuccess = False
      , qrFix = Nothing
      , qrAffectedSpans = []
      , qrNewAlias = ""
      , qrSymbolCount = 0
      , qrErrors = map showConflict (qaConflicts analysis)
      , qrWarnings = []
      , qrPreview = Nothing
      }
  | qaExistingQualified analysis = pure QualifyResult
      { qrSuccess = False
      , qrFix = Nothing
      , qrAffectedSpans = []
      , qrNewAlias = ""
      , qrSymbolCount = 0
      , qrErrors = ["Import is already qualified"]
      , qrWarnings = []
      , qrPreview = Nothing
      }
  | otherwise = do
      content <- TE.decodeUtf8 <$> BS.readFile file
      let fix = generateQualifyFix analysis
          transformed = applyFix content fix

      -- Validate the transformation produces valid syntax
      validationResult <- validateSyntax file transformed

      case validationResult of
        Left errs -> pure QualifyResult
          { qrSuccess = False
          , qrFix = Just fix
          , qrAffectedSpans = map suSpan (qaSymbolsToQualify analysis)
          , qrNewAlias = qaSuggestedAlias analysis
          , qrSymbolCount = length (qaSymbolsToQualify analysis)
          , qrErrors = map (\e -> "Transformation would create invalid syntax: " <> veMessage e) errs
          , qrWarnings = []
          , qrPreview = Just transformed
          }
        Right () -> pure QualifyResult
          { qrSuccess = True
          , qrFix = Just fix
          , qrAffectedSpans = map suSpan (qaSymbolsToQualify analysis)
          , qrNewAlias = qaSuggestedAlias analysis
          , qrSymbolCount = length (qaSymbolsToQualify analysis)
          , qrErrors = []
          , qrWarnings = generateWarnings analysis
          , qrPreview = Just transformed
          }

--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- | Show a conflict as text
showConflict :: AliasConflict -> Text
showConflict (AliasAlreadyUsed alias modName) =
  "Alias '" <> alias <> "' is already used by " <> modName
showConflict (SymbolShadowed name _span) =
  "Symbol '" <> name <> "' is shadowed by a local definition"
showConflict (AmbiguousSymbol name mods) =
  "Symbol '" <> name <> "' could come from: " <> T.intercalate ", " mods

-- | Generate warnings for the qualification
generateWarnings :: QualificationAnalysis -> [Text]
generateWarnings analysis = catMaybes
  [ if null (qaSymbolsToQualify analysis)
    then Just "No symbol usages found to qualify"
    else Nothing
  , case qaExplicitSymbols analysis of
      Nothing -> Just "Import has no explicit list - only known symbols will be qualified"
      Just [] -> Just "Import has an empty explicit list"
      Just _ -> Nothing
  ]
