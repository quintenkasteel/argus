{-# LANGUAGE StrictData #-}
{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Linter.Analysis.Syntactic
-- Description : Syntactic analysis using ghc-lib-parser
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides syntactic analysis capabilities using GHC's parser
-- (via ghc-lib-parser). It extracts function information, type signatures,
-- and other syntactic elements needed for linting.
module Linter.Analysis.Syntactic
  ( -- * Parsing
    parseFile
  , parseModule
  , ParseResult (..)
  , ParseError (..)

    -- * Extraction
  , extractFunctions
  , extractTypes
  , extractImports
  , extractExports

    -- * Function representation
  , FunctionInfo (..)
  , ArgumentInfo (..)
  , TypeInfo (..)
  , ImportInfo (..)
  , ExportInfo (..)

    -- * Utilities
  , prettyPrintType
  , getSourceText
  , spanToSrcSpan
  ) where

import Control.Exception (try, SomeException)
import Data.ByteString qualified as BS
import Data.Foldable (foldl')
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import "ghc-lib-parser" GHC.Data.Bag (bagToList)
import "ghc-lib-parser" GHC.Driver.Session (DynFlags, defaultDynFlags, xopt_set)
import "ghc-lib-parser" GHC.Types.Error (getMessages, errMsgSpan)
import "ghc-lib-parser" GHC.Hs
import "ghc-lib-parser" GHC.LanguageExtensions.Type (Extension(..))
import "ghc-lib-parser" GHC.Parser.Annotation (locA)
import "ghc-lib-parser" GHC.Parser.Lexer qualified as Lexer
import "ghc-lib-parser" GHC.Types.Name.Occurrence (occNameString)
import "ghc-lib-parser" GHC.Types.Name.Reader (RdrName (..), rdrNameOcc)
import "ghc-lib-parser" GHC.Types.SrcLoc (GenLocated (L), unLoc)
import "ghc-lib-parser" GHC.Types.SrcLoc qualified as GHC
import "ghc-lib-parser" GHC.Utils.Outputable (showSDocUnsafe, ppr)
import Language.Haskell.GhclibParserEx.GHC.Parser qualified as GhclibParser
import Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeSettings)

import Linter.Types

--------------------------------------------------------------------------------
-- Parse Result Types
--------------------------------------------------------------------------------

-- | Parse error information
data ParseError = ParseError
  { peFile    :: FilePath
  , peLine    :: Int
  , peColumn  :: Int
  , peMessage :: Text
  }
  deriving stock (Eq, Show)

-- | Result of parsing a file
data ParseResult = ParseResult
  { prModule  :: HsModule GhcPs     -- ^ Parsed module
  , prSource  :: Text               -- ^ Original source
  , prFile    :: FilePath           -- ^ File path
  }

--------------------------------------------------------------------------------
-- Extracted Information Types
--------------------------------------------------------------------------------

-- | Information about a function
data FunctionInfo = FunctionInfo
  { fiName       :: Text              -- ^ Function name
  , fiSpan       :: SrcSpan           -- ^ Definition location
  , fiSignature  :: Maybe TypeInfo    -- ^ Type signature (if present)
  , fiArguments  :: [ArgumentInfo]    -- ^ Function arguments
  , fiBody       :: [(Int, Text)]     -- ^ Body lines (line number, text)
  , fiExported   :: Bool              -- ^ Is it exported?
  }
  deriving stock (Eq, Show)

-- | Information about a function argument
data ArgumentInfo = ArgumentInfo
  { aiName      :: Text        -- ^ Argument name/pattern
  , aiType      :: Maybe Text  -- ^ Argument type (if known from signature)
  , aiSpan      :: SrcSpan     -- ^ Source location
  , aiTypeSpan  :: Maybe SrcSpan -- ^ Type location in signature
  }
  deriving stock (Eq, Show)

-- | Information about a type
data TypeInfo = TypeInfo
  { tiText      :: Text        -- ^ Pretty-printed type
  , tiSpan      :: SrcSpan     -- ^ Source location
  , tiArgTypes  :: [Text]      -- ^ Argument types (for functions)
  , tiRetType   :: Text        -- ^ Return type
  }
  deriving stock (Eq, Show)

-- | Information about an import
data ImportInfo = ImportInfo
  { iiModuleName :: Text       -- ^ Imported module name
  , iiQualified  :: Bool       -- ^ Is it qualified?
  , iiAlias      :: Maybe Text -- ^ Alias (if any)
  , iiSpan       :: SrcSpan    -- ^ Import location
  , iiHiding     :: Bool       -- ^ Is it a hiding import?
  , iiExplicit   :: Maybe [Text] -- ^ Explicit import list
  }
  deriving stock (Eq, Show)

-- | Information about an export
data ExportInfo = ExportInfo
  { eiName   :: Text       -- ^ Exported name
  , eiSpan   :: SrcSpan    -- ^ Location in export list
  , eiIsType :: Bool       -- ^ Is it a type export?
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- | Parse a Haskell source file
parseFile :: FilePath -> IO (Either ParseError ParseResult)
parseFile path = do
  result <- try @SomeException $ BS.readFile path
  case result of
    Left err -> pure $ Left ParseError
      { peFile = path
      , peLine = 0
      , peColumn = 0
      , peMessage = T.pack $ show err
      }
    Right content -> parseModule path (TE.decodeUtf8 content)

-- | Parse a Haskell module from source text using ghc-lib-parser
parseModule :: FilePath -> Text -> IO (Either ParseError ParseResult)
parseModule path source = do
  let sourceStr = T.unpack source
      dynFlags = baseDynFlags
  pure $ case GhclibParser.parseFile path dynFlags sourceStr of
    Lexer.POk _state (L _ hsModule) -> Right ParseResult
      { prModule = hsModule
      , prSource = source
      , prFile = path
      }
    Lexer.PFailed pState ->
      let (line, col) = getParseErrorLoc pState
          errMsg = getParseErrorMsg pState
      in Left ParseError
        { peFile = path
        , peLine = line
        , peColumn = col
        , peMessage = errMsg
        }

-- | Base DynFlags for parsing with common extensions enabled
baseDynFlags :: DynFlags
baseDynFlags = foldl xopt_set (defaultDynFlags fakeSettings) defaultExtensions
  where
    -- Enable common extensions that most Haskell projects use
    defaultExtensions =
      [ BangPatterns
      , BinaryLiterals
      , ConstraintKinds
      , DataKinds
      , DefaultSignatures
      , DeriveAnyClass
      , DeriveDataTypeable
      , DeriveFoldable
      , DeriveFunctor
      , DeriveGeneric
      , DeriveTraversable
      , DerivingStrategies
      , DerivingVia
      , DuplicateRecordFields
      , EmptyCase
      , ExistentialQuantification
      , ExplicitForAll
      , FlexibleContexts
      , FlexibleInstances
      , FunctionalDependencies
      , GADTs
      , GeneralizedNewtypeDeriving
      , ImportQualifiedPost
      , InstanceSigs
      , KindSignatures
      , LambdaCase
      , MultiParamTypeClasses
      , MultiWayIf
      , NamedFieldPuns
      , NumericUnderscores
      , OverloadedStrings
      , PatternSynonyms
      , PolyKinds
      , RankNTypes
      , RecordWildCards
      , ScopedTypeVariables
      , StandaloneDeriving
      , StrictData
      , TupleSections
      , TypeApplications
      , TypeFamilies
      , TypeOperators
      , ViewPatterns
      ]

-- | Extract line and column from parse error state
getParseErrorLoc :: Lexer.PState -> (Int, Int)
getParseErrorLoc pState =
  let msgs = Lexer.getPsErrorMessages pState
      msgList = bagToList (getMessages msgs)
  in case msgList of
    (msgEnv:_) -> case GHC.srcSpanStart (errMsgSpan msgEnv) of
      GHC.RealSrcLoc loc _ -> (GHC.srcLocLine loc, GHC.srcLocCol loc)
      _ -> (1, 1)
    [] -> (1, 1)

-- | Extract error message from parse error state
-- Uses the Diagnostic API to get a proper error message
getParseErrorMsg :: Lexer.PState -> Text
getParseErrorMsg pState =
  let msgs = Lexer.getPsErrorMessages pState
      msgList = bagToList (getMessages msgs)
  in case msgList of
    (msgEnv:_) ->
      -- Extract location info for a meaningful message
      let srcSpan = errMsgSpan msgEnv
          locStr = case srcSpan of
            GHC.RealSrcSpan rss _ ->
              T.pack $ "line " ++ show (GHC.srcSpanStartLine rss) ++
                       ", column " ++ show (GHC.srcSpanStartCol rss)
            _ -> "unknown location"
      in "Parse error at " <> locStr
    [] -> "Parse error (no specific message)"


--------------------------------------------------------------------------------
-- Function Extraction
--------------------------------------------------------------------------------

-- | Extract all functions from a parsed module
extractFunctions :: FilePath -> Text -> HsModule GhcPs -> [FunctionInfo]
extractFunctions path source m = extractFromDecls path source (hsmodDecls m) exportedNames
  where
    exportedNames = getExportedNames m

-- | Get names that are exported from a module
getExportedNames :: HsModule GhcPs -> Maybe [Text]
getExportedNames m = case hsmodExports m of
  Nothing -> Nothing  -- Export everything
  Just (L _ exports) -> Just $ concatMap extractExportName exports

extractExportName :: LIE GhcPs -> [Text]
extractExportName (L _ ie) = case ie of
  IEVar _ wrappedName _ -> [ieWrappedNameToText wrappedName]
  IEThingAbs _ wrappedName _ -> [ieWrappedNameToText wrappedName]
  IEThingAll _ wrappedName _ -> [ieWrappedNameToText wrappedName]
  IEThingWith _ wrappedName _ _ _ -> [ieWrappedNameToText wrappedName]
  _ -> []

-- | Extract name from IEWrappedName
ieWrappedNameToText :: LIEWrappedName GhcPs -> Text
ieWrappedNameToText (L _ wrapped) = case wrapped of
  IEName _ (L _ name) -> rdrNameToText name
  IEPattern _ (L _ name) -> rdrNameToText name
  IEType _ (L _ name) -> rdrNameToText name

-- | Extract functions from declarations
extractFromDecls :: FilePath -> Text -> [LHsDecl GhcPs] -> Maybe [Text] -> [FunctionInfo]
extractFromDecls path source decls exportedNames = go decls Map.empty
  where
    go [] sigs = []
    go (L _ (SigD _ (TypeSig _ names ty)) : rest) sigs =
      let sigInfo = extractTypeInfo ty
          newSigs = foldr (\name m -> Map.insert (rdrNameToText $ unLoc name) sigInfo m) sigs names
      in go rest newSigs
    go (L loc (ValD _ (FunBind _ (L _ name) mg)) : rest) sigs =
      let fname = rdrNameToText name
          sig = Map.lookup fname sigs
          finfo = extractFunctionInfo path source (locA loc) fname sig mg exportedNames
      in finfo : go rest sigs
    go (_ : rest) sigs = go rest sigs

-- | Extract function info from a function binding
extractFunctionInfo :: FilePath -> Text -> GHC.SrcSpan -> Text -> Maybe TypeInfo
                    -> MatchGroup GhcPs (LHsExpr GhcPs) -> Maybe [Text] -> FunctionInfo
extractFunctionInfo path source loc name mSig mg exportedNames = FunctionInfo
  { fiName = name
  , fiSpan = spanToSrcSpan path loc
  , fiSignature = mSig
  , fiArguments = extractArguments path mSig mg
  , fiBody = extractBodyLines source loc
  , fiExported = case exportedNames of
      Nothing -> True  -- All exported
      Just names -> name `elem` names
  }

-- | Extract argument information from a match group
extractArguments :: FilePath -> Maybe TypeInfo -> MatchGroup GhcPs (LHsExpr GhcPs) -> [ArgumentInfo]
extractArguments path mSig (MG _ (L _ matches)) = case matches of
  [] -> []
  (L _ (Match _ _ pats _) : _) ->
    let argTypes = maybe [] tiArgTypes mSig
    in zipWith (extractArgInfo path) pats (argTypes ++ repeat "")

extractArgInfo :: FilePath -> LPat GhcPs -> Text -> ArgumentInfo
extractArgInfo path (L loc pat) argType = ArgumentInfo
  { aiName = patternToText pat
  , aiType = if T.null argType then Nothing else Just argType
  , aiSpan = spanToSrcSpan path (locA loc)
  , aiTypeSpan = Nothing
  }

-- | Convert a pattern to text
patternToText :: Pat GhcPs -> Text
patternToText = T.pack . showSDocUnsafe . ppr

-- | Extract body lines from source
extractBodyLines :: Text -> GHC.SrcSpan -> [(Int, Text)]
extractBodyLines source loc = case loc of
  GHC.RealSrcSpan rss _ ->
    let startLine = GHC.srcSpanStartLine rss
        endLine = GHC.srcSpanEndLine rss
        allLines = T.lines source
    in [(i, allLines !! (i - 1)) | i <- [startLine..endLine], i <= length allLines]
  _ -> []

--------------------------------------------------------------------------------
-- Type Extraction
--------------------------------------------------------------------------------

-- | Extract type information from a type signature
extractTypeInfo :: LHsSigWcType GhcPs -> TypeInfo
extractTypeInfo (HsWC _ (L loc (HsSig _ _ (L _ ty)))) =
  let typeText = prettyPrintType ty
      argTypes = extractArgTypes ty
      retType = extractReturnType ty
      srcSpan = case locA loc of
        GHC.RealSrcSpan rss _ -> SrcSpan
          { srcSpanFile = ""  -- Will be filled in by caller
          , srcSpanStartLine = GHC.srcSpanStartLine rss
          , srcSpanStartCol = GHC.srcSpanStartCol rss
          , srcSpanEndLine = GHC.srcSpanEndLine rss
          , srcSpanEndCol = GHC.srcSpanEndCol rss
          }
        _ -> noSrcSpan
  in TypeInfo
    { tiText = typeText
    , tiSpan = srcSpan
    , tiArgTypes = argTypes
    , tiRetType = retType
    }

-- | Extract argument types from a function type
extractArgTypes :: HsType GhcPs -> [Text]
extractArgTypes (HsFunTy _ _ arg res) =
  prettyPrintType (unLoc arg) : extractArgTypes (unLoc res)
extractArgTypes (HsParTy _ ty) = extractArgTypes (unLoc ty)
extractArgTypes (HsForAllTy _ _ ty) = extractArgTypes (unLoc ty)
extractArgTypes (HsQualTy _ _ ty) = extractArgTypes (unLoc ty)
extractArgTypes _ = []

-- | Extract return type from a function type
extractReturnType :: HsType GhcPs -> Text
extractReturnType (HsFunTy _ _ _ res) = extractReturnType (unLoc res)
extractReturnType (HsParTy _ ty) = extractReturnType (unLoc ty)
extractReturnType (HsForAllTy _ _ ty) = extractReturnType (unLoc ty)
extractReturnType (HsQualTy _ _ ty) = extractReturnType (unLoc ty)
extractReturnType ty = prettyPrintType ty

-- | Pretty print a type
prettyPrintType :: HsType GhcPs -> Text
prettyPrintType = T.pack . showSDocUnsafe . ppr

-- | Extract all type declarations from a module
extractTypes :: FilePath -> HsModule GhcPs -> [Symbol]
extractTypes path m = mapMaybe (extractTypeDecl path) (hsmodDecls m)

extractTypeDecl :: FilePath -> LHsDecl GhcPs -> Maybe Symbol
extractTypeDecl path (L loc (TyClD _ decl)) = case decl of
  DataDecl{tcdLName = L _ name} -> Just $ mkSymbol path (locA loc) name TypeConstructor
  SynDecl{tcdLName = L _ name} -> Just $ mkSymbol path (locA loc) name TypeConstructor
  ClassDecl{tcdLName = L _ name} -> Just $ mkSymbol path (locA loc) name TypeClass
  FamDecl _ (FamilyDecl{fdLName = L _ name}) -> Just $ mkSymbol path (locA loc) name TypeFamily
extractTypeDecl _ _ = Nothing

mkSymbol :: FilePath -> GHC.SrcSpan -> RdrName -> SymbolKind -> Symbol
mkSymbol path loc name kind = Symbol
  { symbolName = QualifiedName Nothing (rdrNameToText name)
  , symbolKind = kind
  , symbolSpan = spanToSrcSpan path loc
  , symbolExported = True  -- Simplified
  , symbolType = Nothing
  }

--------------------------------------------------------------------------------
-- Import Extraction
--------------------------------------------------------------------------------

-- | Extract all imports from a module
extractImports :: FilePath -> HsModule GhcPs -> [ImportInfo]
extractImports path m = map (extractImport path) (hsmodImports m)

extractImport :: FilePath -> LImportDecl GhcPs -> ImportInfo
extractImport path (L loc decl) = ImportInfo
  { iiModuleName = T.pack $ moduleNameString $ unLoc $ ideclName decl
  , iiQualified = ideclQualified decl /= NotQualified
  , iiAlias = fmap (T.pack . moduleNameString . unLoc) (ideclAs decl)
  , iiSpan = spanToSrcSpan path (locA loc)
  , iiHiding = case ideclImportList decl of
      Just (Exactly, _) -> False
      Just (EverythingBut, _) -> True
      Nothing -> False
  , iiExplicit = case ideclImportList decl of
      Just (Exactly, L _ ies) -> Just $ map (T.pack . showSDocUnsafe . ppr . unLoc) ies
      _ -> Nothing
  }

--------------------------------------------------------------------------------
-- Export Extraction
--------------------------------------------------------------------------------

-- | Extract all exports from a module
extractExports :: FilePath -> HsModule GhcPs -> [ExportInfo]
extractExports path m = case hsmodExports m of
  Nothing -> []
  Just (L _ exports) -> map (extractExport path) exports

extractExport :: FilePath -> LIE GhcPs -> ExportInfo
extractExport path (L loc ie) = ExportInfo
  { eiName = T.pack $ showSDocUnsafe $ ppr ie
  , eiSpan = spanToSrcSpan path (locA loc)
  , eiIsType = isTypeExport ie
  }

isTypeExport :: IE GhcPs -> Bool
isTypeExport (IEThingAbs _ _ _) = True
isTypeExport (IEThingAll _ _ _) = True
isTypeExport (IEThingWith _ _ _ _ _) = True
isTypeExport _ = False

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Convert RdrName to Text
rdrNameToText :: RdrName -> Text
rdrNameToText = T.pack . occNameString . rdrNameOcc

-- | Convert GHC SrcSpan to our SrcSpan
spanToSrcSpan :: FilePath -> GHC.SrcSpan -> SrcSpan
spanToSrcSpan path loc = case loc of
  GHC.RealSrcSpan rss _ -> SrcSpan
    { srcSpanFile = path
    , srcSpanStartLine = GHC.srcSpanStartLine rss
    , srcSpanStartCol = GHC.srcSpanStartCol rss
    , srcSpanEndLine = GHC.srcSpanEndLine rss
    , srcSpanEndCol = GHC.srcSpanEndCol rss
    }
  GHC.UnhelpfulSpan _ -> noSrcSpan { srcSpanFile = path }

-- | Get source text for a span
getSourceText :: Text -> SrcSpan -> Text
getSourceText source SrcSpan{..} =
  let allLines = T.lines source
      startIdx = srcSpanStartLine - 1
      endIdx = srcSpanEndLine - 1
  in if startIdx >= 0 && endIdx < length allLines
     then T.intercalate "\n" $ take (endIdx - startIdx + 1) $ drop startIdx allLines
     else ""
