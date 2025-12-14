{-# LANGUAGE StrictData #-}
{-# LANGUAGE PackageImports #-}

-- |
-- Module      : Argus.Analysis.Syntactic
-- Description : Syntactic analysis using ghc-lib-parser
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides syntactic analysis capabilities using GHC's parser
-- (via ghc-lib-parser). It extracts function information, type signatures,
-- and other syntactic elements needed for linting.
module Argus.Analysis.Syntactic
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
  , extractPragmas

    -- * Template Haskell detection
  , extractThSplices
  , extractQuasiQuotes
  , extractThQuotes
  , ThSpliceInfo (..)
  , QuasiQuoteInfo (..)
  , ThQuoteInfo (..)
  , PragmaInfo (..)

    -- * Instance detection
  , extractInstances
  , InstanceInfo (..)
  , isPotentialOrphan

    -- * Function representation
  , FunctionInfo (..)
  , ArgumentInfo (..)
  , TypeInfo (..)
  , ImportInfo (..)
  , ImportItem (..)
  , ExportInfo (..)

    -- * Utilities
  , prettyPrintType
  , getSourceText
  , spanToSrcSpan
  ) where

import Control.Exception (try, SomeException)
import Data.ByteString qualified as BS
-- import Data.Foldable (foldl') -- removed - unused
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import "ghc-lib-parser" GHC.Data.Bag (bagToList)
import "ghc-lib-parser" GHC.Driver.Session (DynFlags, defaultDynFlags, xopt_set)
import "ghc-lib-parser" GHC.Types.Error (getMessages, errMsgSpan)
import "ghc-lib-parser" GHC.Hs
import "ghc-lib-parser" GHC.LanguageExtensions.Type (Extension(..))
-- import "ghc-lib-parser" GHC.Parser.Annotation (locA) -- removed - unused
import "ghc-lib-parser" GHC.Parser.Lexer qualified as Lexer
import "ghc-lib-parser" GHC.Types.Name.Occurrence (occNameString)
import "ghc-lib-parser" GHC.Types.Name.Reader (RdrName (..), rdrNameOcc)
import "ghc-lib-parser" GHC.Types.SrcLoc (GenLocated (L), unLoc)
import "ghc-lib-parser" GHC.Types.SrcLoc qualified as GHC
import "ghc-lib-parser" GHC.Utils.Outputable (showSDocUnsafe, ppr)
import "ghc-lib-parser" GHC.Data.FastString (unpackFS)
import Language.Haskell.GhclibParserEx.GHC.Parser qualified as GhclibParser
import Language.Haskell.GhclibParserEx.GHC.Settings.Config (fakeSettings)

import Argus.Types

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
  , tiArgSpans  :: [SrcSpan]   -- ^ Source locations for each argument type
  , tiRetType   :: Text        -- ^ Return type
  }
  deriving stock (Eq, Show)

-- | Classification of an imported item
data ImportItem = ImportItem
  { importItemName       :: Text   -- ^ Name of the imported item
  , importItemIsType     :: Bool   -- ^ Is it a type import?
  , importItemIsOperator :: Bool   -- ^ Is it an operator?
  , importItemIsWildcard :: Bool   -- ^ Is it a wildcard import like (..)
  , importItemChildren   :: [Text] -- ^ Child items (methods/constructors) for imports like Class (method1, method2)
  }
  deriving stock (Eq, Show)

-- | Information about an import
data ImportInfo = ImportInfo
  { iiModuleName :: Text              -- ^ Imported module name
  , iiQualified  :: Bool              -- ^ Is it qualified?
  , iiAlias      :: Maybe Text        -- ^ Alias (if any)
  , iiSpan       :: SrcSpan           -- ^ Import location
  , iiHiding     :: Bool              -- ^ Is it a hiding import?
  , iiExplicit   :: Maybe [ImportItem] -- ^ Explicit import list with classification
  }
  deriving stock (Eq, Show)

-- | Information about an export
data ExportInfo = ExportInfo
  { eiName   :: Text       -- ^ Exported name
  , eiSpan   :: SrcSpan    -- ^ Location in export list
  , eiIsType :: Bool       -- ^ Is it a type export?
  }
  deriving stock (Eq, Show)

-- | Information about a Template Haskell splice
data ThSpliceInfo = ThSpliceInfo
  { tsiSpan       :: SrcSpan     -- ^ Location of the splice
  , tsiQuotedName :: Maybe Text  -- ^ Name quoted with '' if any
  , tsiExpr       :: Text        -- ^ Pretty-printed splice expression
  , tsiIsTyped    :: Bool        -- ^ Is it a typed splice $$(...)?
  }
  deriving stock (Eq, Show)

-- | Information about a QuasiQuote
data QuasiQuoteInfo = QuasiQuoteInfo
  { qqiSpan     :: SrcSpan  -- ^ Location of the quasiquote
  , qqiQuoter   :: Text     -- ^ The quoter name (e.g., "sql", "hamlet")
  , qqiContents :: Text     -- ^ The quoted contents
  }
  deriving stock (Eq, Show)

-- | Information about a TH quote (name quote)
data ThQuoteInfo = ThQuoteInfo
  { tqiSpan     :: SrcSpan  -- ^ Location of the quote
  , tqiName     :: Text     -- ^ The quoted name
  , tqiIsType   :: Bool     -- ^ Is it a type quote ''Name (vs value quote 'name)?
  }
  deriving stock (Eq, Show)

-- | Information about a LANGUAGE pragma
data PragmaInfo = PragmaInfo
  { piSpan       :: SrcSpan    -- ^ Location of the pragma
  , piExtension  :: Text       -- ^ Extension name (e.g., "TemplateHaskell")
  , piIsEnabled  :: Bool       -- ^ True if enabled, False if disabled with No prefix
  }
  deriving stock (Eq, Show)

-- | Information about an instance declaration
data InstanceInfo = InstanceInfo
  { iiInstanceSpan  :: SrcSpan    -- ^ Location of the instance
  , iiClassName     :: Text       -- ^ Name of the type class
  , iiTypeName      :: Text       -- ^ Primary type being instantiated
  , iiFullType      :: Text       -- ^ Full instance head
  , iiIsDeriving    :: Bool       -- ^ Is this a derived instance?
  , iiIsOrphan      :: Bool       -- ^ Is this potentially an orphan?
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
      , TemplateHaskell
      , TemplateHaskellQuotes
      , QuasiQuotes
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
  -- GHC 9.10: IE constructors: ext, name, exportdoc, [wildcard], [children]
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
    go [] _sigs = []
    go (L _ (SigD _ (TypeSig _ names ty)) : rest) sigs =
      let sigInfo = extractTypeInfo path ty
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
extractTypeInfo :: FilePath -> LHsSigWcType GhcPs -> TypeInfo
extractTypeInfo path (HsWC _ (L loc (HsSig _ _ (L _ ty)))) =
  let typeText = prettyPrintType ty
      (argTypes, argSpans) = unzip $ extractArgTypesWithSpans path ty
      retType = extractReturnType ty
      srcSpan = case locA loc of
        GHC.RealSrcSpan rss _ -> mkSrcSpanRaw
          path
          (GHC.srcSpanStartLine rss)
          (GHC.srcSpanStartCol rss)
          (GHC.srcSpanEndLine rss)
          (GHC.srcSpanEndCol rss)
        _ -> noSrcSpan
  in TypeInfo
    { tiText = typeText
    , tiSpan = srcSpan
    , tiArgTypes = argTypes
    , tiArgSpans = argSpans
    , tiRetType = retType
    }

-- | Extract argument types from a function type (just text)
_extractArgTypes :: HsType GhcPs -> [Text]
_extractArgTypes (HsFunTy _ _ arg res) =
  prettyPrintType (unLoc arg) : _extractArgTypes (unLoc res)
_extractArgTypes (HsParTy _ ty) = _extractArgTypes (unLoc ty)
_extractArgTypes (HsForAllTy _ _ ty) = _extractArgTypes (unLoc ty)
_extractArgTypes (HsQualTy _ _ ty) = _extractArgTypes (unLoc ty)
_extractArgTypes _ = []

-- | Extract argument types with their source spans
extractArgTypesWithSpans :: FilePath -> HsType GhcPs -> [(Text, SrcSpan)]
extractArgTypesWithSpans path (HsFunTy _ _ arg res) =
  let argText = prettyPrintType (unLoc arg)
      argSpan = spanToSrcSpan path (getLocA arg)
  in (argText, argSpan) : extractArgTypesWithSpans path (unLoc res)
extractArgTypesWithSpans path (HsParTy _ ty) = extractArgTypesWithSpans path (unLoc ty)
extractArgTypesWithSpans path (HsForAllTy _ _ ty) = extractArgTypesWithSpans path (unLoc ty)
extractArgTypesWithSpans path (HsQualTy _ _ ty) = extractArgTypesWithSpans path (unLoc ty)
extractArgTypesWithSpans _ _ = []

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
      Just (Exactly, L _ ies) -> Just $ map classifyImportItem ies
      _ -> Nothing
  }

-- | Classify an import entry as type/value/operator
-- GHC 9.10: IE constructors: ext, name, exportdoc, [wildcard], [children]
classifyImportItem :: LIE GhcPs -> ImportItem
classifyImportItem (L _ ie) = case ie of
  -- Type imports (IEThingAbs, IEThingAll, IEThingWith)
  IEThingAbs _ wrappedName _ ->
    mkImportItem (ieWrappedNameToText wrappedName) True False []
  IEThingAll _ wrappedName _ ->
    -- This is a wildcard import like ToJSON (..) - mark as wildcard
    mkImportItem (ieWrappedNameToText wrappedName) True True []
  IEThingWith _ wrappedName _ children _ ->
    -- Explicit list like ToJSON (toJSON, fromJSON) or HasTitle (title)
    -- Extract the parent and all child names
    let childNames = map ieWrappedNameToText children
    in mkImportItem (ieWrappedNameToText wrappedName) True False childNames
  -- Value imports (IEVar)
  IEVar _ wrappedName _ ->
    mkImportItem (ieWrappedNameToText wrappedName) False False []
  -- Fallback: use pretty-printer
  _ -> mkImportItem (T.pack $ showSDocUnsafe $ ppr ie) False False []
  where
    mkImportItem name isType isWildcard children = ImportItem
      { importItemName = name
      , importItemIsType = isType
      , importItemIsOperator = isOperatorImport name
      , importItemIsWildcard = isWildcard
      , importItemChildren = children
      }

-- | Check if an import name is an operator (contains only operator characters)
isOperatorImport :: Text -> Bool
isOperatorImport name =
  let stripped = T.dropAround (== '(') $ T.dropAround (== ')') name
  in not (T.null stripped) && T.all isOperatorChar stripped
  where
    isOperatorChar c = c `elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)

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
isTypeExport (IEThingAbs {}) = True
isTypeExport (IEThingAll {}) = True
isTypeExport (IEThingWith {}) = True
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
  GHC.RealSrcSpan rss _ -> mkSrcSpanRaw
    path
    (GHC.srcSpanStartLine rss)
    (GHC.srcSpanStartCol rss)
    (GHC.srcSpanEndLine rss)
    (GHC.srcSpanEndCol rss)
  GHC.UnhelpfulSpan _ -> noSrcSpan { srcSpanFile = path }

-- | Get source text for a span
getSourceText :: Text -> SrcSpan -> Text
getSourceText source SrcSpan{..} =
  let allLines = T.lines source
      startIdx = unLine srcSpanStartLine - 1
      endIdx = unLine srcSpanEndLine - 1
  in if startIdx >= 0 && endIdx < length allLines
     then T.intercalate "\n" $ take (endIdx - startIdx + 1) $ drop startIdx allLines
     else ""

--------------------------------------------------------------------------------
-- Template Haskell Extraction
--------------------------------------------------------------------------------

-- | Extract Template Haskell splices from a module
-- Detects both untyped $(...) and typed $$(...) splices
extractThSplices :: FilePath -> HsModule GhcPs -> [ThSpliceInfo]
extractThSplices path m = concatMap (extractSplicesFromDecl path) (hsmodDecls m)

-- | Extract splices from a declaration
extractSplicesFromDecl :: FilePath -> LHsDecl GhcPs -> [ThSpliceInfo]
extractSplicesFromDecl path (L _ (SpliceD _ (SpliceDecl _ splice _))) =
  [extractSpliceInfo path splice]
extractSplicesFromDecl path (L _ (ValD _ bind)) =
  extractSplicesFromBind path bind
extractSplicesFromDecl path (L _ (TyClD _ tyDecl)) =
  extractSplicesFromTyClDecl path tyDecl
extractSplicesFromDecl _ _ = []

-- | Extract splices from a binding
extractSplicesFromBind :: FilePath -> HsBind GhcPs -> [ThSpliceInfo]
extractSplicesFromBind path (FunBind _ _ mg) =
  extractSplicesFromMatchGroup path mg
extractSplicesFromBind path (PatBind _ _ _ grhss) =  -- GHC 9.10: 4 args
  extractSplicesFromGRHSs path grhss
extractSplicesFromBind _ _ = []

-- | Extract splices from a match group
extractSplicesFromMatchGroup :: FilePath -> MatchGroup GhcPs (LHsExpr GhcPs) -> [ThSpliceInfo]
extractSplicesFromMatchGroup path (MG _ (L _ matches)) =
  concatMap (extractSplicesFromMatch path) matches

-- | Extract splices from a match
extractSplicesFromMatch :: FilePath -> LMatch GhcPs (LHsExpr GhcPs) -> [ThSpliceInfo]
extractSplicesFromMatch path (L _ (Match _ _ _ grhss)) =
  extractSplicesFromGRHSs path grhss

-- | Extract splices from GRHSs
extractSplicesFromGRHSs :: FilePath -> GRHSs GhcPs (LHsExpr GhcPs) -> [ThSpliceInfo]
extractSplicesFromGRHSs path (GRHSs _ grhss localBinds) =
  concatMap (extractSplicesFromGRHS path) grhss
  ++ extractSplicesFromLocalBinds path localBinds

-- | Extract splices from a GRHS
extractSplicesFromGRHS :: FilePath -> LGRHS GhcPs (LHsExpr GhcPs) -> [ThSpliceInfo]
extractSplicesFromGRHS path (L _ (GRHS _ _ expr)) =
  extractSplicesFromExpr path expr

-- | Extract splices from local bindings
extractSplicesFromLocalBinds :: FilePath -> HsLocalBinds GhcPs -> [ThSpliceInfo]
extractSplicesFromLocalBinds path (HsValBinds _ (ValBinds _ binds _)) =
  concatMap (extractSplicesFromBind path . unLoc) (bagToList binds)
extractSplicesFromLocalBinds _ _ = []

-- | Extract splices from an expression (recursive)
extractSplicesFromExpr :: FilePath -> LHsExpr GhcPs -> [ThSpliceInfo]
extractSplicesFromExpr path (L _ expr) = case expr of
  HsUntypedSplice _ splice ->
    [extractSpliceInfoUnlocated path splice False]
  HsTypedSplice _ splicedExpr ->
    ThSpliceInfo
      { tsiSpan = noSrcSpan { srcSpanFile = path }
      , tsiQuotedName = Nothing
      , tsiExpr = T.pack $ showSDocUnsafe $ ppr splicedExpr
      , tsiIsTyped = True
      } : extractSplicesFromExpr path splicedExpr
  HsApp _ f a ->
    extractSplicesFromExpr path f ++ extractSplicesFromExpr path a
  HsLam _ _ mg ->  -- GHC 9.10: 3 args
    extractSplicesFromMatchGroup path mg
  HsCase _ scrut mg ->
    extractSplicesFromExpr path scrut ++ extractSplicesFromMatchGroup path mg
  HsLet _ binds body ->  -- GHC 9.10: 3 args
    extractSplicesFromLocalBinds path binds ++ extractSplicesFromExpr path body
  HsIf _ cond t f ->
    extractSplicesFromExpr path cond
    ++ extractSplicesFromExpr path t
    ++ extractSplicesFromExpr path f
  HsDo _ _ (L _ stmts) ->
    concatMap (extractSplicesFromStmt path) stmts
  ExplicitList _ exprs ->
    concatMap (extractSplicesFromExpr path) exprs
  ExplicitTuple _ args _ ->
    concatMap extractFromTupleArg args
  HsPar _ inner ->  -- GHC 9.4: 4 args
    extractSplicesFromExpr path inner
  OpApp _ l op r ->
    extractSplicesFromExpr path l
    ++ extractSplicesFromExpr path op
    ++ extractSplicesFromExpr path r
  NegApp _ e _ ->
    extractSplicesFromExpr path e
  SectionL _ e op ->
    extractSplicesFromExpr path e ++ extractSplicesFromExpr path op
  SectionR _ op e ->
    extractSplicesFromExpr path op ++ extractSplicesFromExpr path e
  _ -> []
  where
    extractFromTupleArg (Present _ e) = extractSplicesFromExpr path e
    extractFromTupleArg _ = []

-- | Extract splices from a statement
extractSplicesFromStmt :: FilePath -> LStmt GhcPs (LHsExpr GhcPs) -> [ThSpliceInfo]
extractSplicesFromStmt path (L _ stmt) = case stmt of
  BodyStmt _ body _ _ -> extractSplicesFromExpr path body
  BindStmt _ _ body -> extractSplicesFromExpr path body
  LetStmt _ binds -> extractSplicesFromLocalBinds path binds
  _ -> []

-- | Extract splices from type class declarations
extractSplicesFromTyClDecl :: FilePath -> TyClDecl GhcPs -> [ThSpliceInfo]
extractSplicesFromTyClDecl path ClassDecl{tcdMeths = meths} =
  concatMap (extractSplicesFromBind path . unLoc) (bagToList meths)
extractSplicesFromTyClDecl _ _ = []

-- | Extract info from a splice
extractSpliceInfo :: FilePath -> GenLocated l (HsUntypedSplice GhcPs) -> ThSpliceInfo
extractSpliceInfo path (L _ splice) = extractSpliceInfoUnlocated path splice False

-- | Extract info from an unlocated splice
extractSpliceInfoUnlocated :: FilePath -> HsUntypedSplice GhcPs -> Bool -> ThSpliceInfo
extractSpliceInfoUnlocated path splice isTyped = case splice of
  HsUntypedSpliceExpr _ expr -> ThSpliceInfo
    { tsiSpan = spanToSrcSpan path noSrcSpan'
    , tsiQuotedName = Nothing
    , tsiExpr = T.pack $ showSDocUnsafe $ ppr expr
    , tsiIsTyped = isTyped
    }
  HsQuasiQuote _ quoter _ -> ThSpliceInfo
    { tsiSpan = spanToSrcSpan path noSrcSpan'
    , tsiQuotedName = Just $ rdrNameToText quoter
    , tsiExpr = T.pack $ showSDocUnsafe $ ppr quoter
    , tsiIsTyped = isTyped
    }
  where
    noSrcSpan' = GHC.UnhelpfulSpan GHC.UnhelpfulNoLocationInfo

--------------------------------------------------------------------------------
-- QuasiQuote Extraction
--------------------------------------------------------------------------------

-- | Extract QuasiQuotes from a module
extractQuasiQuotes :: FilePath -> HsModule GhcPs -> [QuasiQuoteInfo]
extractQuasiQuotes path m = concatMap (extractQQFromDecl path) (hsmodDecls m)

-- | Extract quasiquotes from a declaration
extractQQFromDecl :: FilePath -> LHsDecl GhcPs -> [QuasiQuoteInfo]
extractQQFromDecl path (L _ (SpliceD _ (SpliceDecl _ splice _))) =
  extractQQFromSplice path splice
extractQQFromDecl path (L _ (ValD _ bind)) =
  extractQQFromBind path bind
extractQQFromDecl _ _ = []

-- | Extract quasiquotes from a binding
extractQQFromBind :: FilePath -> HsBind GhcPs -> [QuasiQuoteInfo]
extractQQFromBind path (FunBind _ _ mg) =
  extractQQFromMatchGroup path mg
extractQQFromBind path (PatBind _ _ _ grhss) =  -- GHC 9.10: 4 args
  extractQQFromGRHSs path grhss
extractQQFromBind _ _ = []

-- | Extract quasiquotes from a match group
extractQQFromMatchGroup :: FilePath -> MatchGroup GhcPs (LHsExpr GhcPs) -> [QuasiQuoteInfo]
extractQQFromMatchGroup path (MG _ (L _ matches)) =
  concatMap (extractQQFromMatch path) matches

-- | Extract quasiquotes from a match
extractQQFromMatch :: FilePath -> LMatch GhcPs (LHsExpr GhcPs) -> [QuasiQuoteInfo]
extractQQFromMatch path (L _ (Match _ _ _ grhss)) =
  extractQQFromGRHSs path grhss

-- | Extract quasiquotes from GRHSs
extractQQFromGRHSs :: FilePath -> GRHSs GhcPs (LHsExpr GhcPs) -> [QuasiQuoteInfo]
extractQQFromGRHSs path (GRHSs _ grhss localBinds) =
  concatMap (extractQQFromGRHS path) grhss
  ++ extractQQFromLocalBinds path localBinds

-- | Extract quasiquotes from a GRHS
extractQQFromGRHS :: FilePath -> LGRHS GhcPs (LHsExpr GhcPs) -> [QuasiQuoteInfo]
extractQQFromGRHS path (L _ (GRHS _ _ expr)) =
  extractQQFromExpr path expr

-- | Extract quasiquotes from local bindings
extractQQFromLocalBinds :: FilePath -> HsLocalBinds GhcPs -> [QuasiQuoteInfo]
extractQQFromLocalBinds path (HsValBinds _ (ValBinds _ binds _)) =
  concatMap (extractQQFromBind path . unLoc) (bagToList binds)
extractQQFromLocalBinds _ _ = []

-- | Extract quasiquotes from an expression
extractQQFromExpr :: FilePath -> LHsExpr GhcPs -> [QuasiQuoteInfo]
extractQQFromExpr path (L _ expr) = case expr of
  HsUntypedSplice _ splice ->
    extractQQFromSplice' path splice
  HsApp _ f a ->
    extractQQFromExpr path f ++ extractQQFromExpr path a
  HsLam _ _ mg ->  -- GHC 9.10: 3 args
    extractQQFromMatchGroup path mg
  HsCase _ scrut mg ->
    extractQQFromExpr path scrut ++ extractQQFromMatchGroup path mg
  HsLet _ binds body ->  -- GHC 9.10: 3 args
    extractQQFromLocalBinds path binds ++ extractQQFromExpr path body
  HsIf _ cond t f ->
    extractQQFromExpr path cond
    ++ extractQQFromExpr path t
    ++ extractQQFromExpr path f
  HsDo _ _ (L _ stmts) ->
    concatMap (extractQQFromStmt path) stmts
  ExplicitList _ exprs ->
    concatMap (extractQQFromExpr path) exprs
  HsPar _ inner ->  -- GHC 9.4: 4 args
    extractQQFromExpr path inner
  OpApp _ l op r ->
    extractQQFromExpr path l
    ++ extractQQFromExpr path op
    ++ extractQQFromExpr path r
  _ -> []

-- | Extract quasiquotes from a statement
extractQQFromStmt :: FilePath -> LStmt GhcPs (LHsExpr GhcPs) -> [QuasiQuoteInfo]
extractQQFromStmt path (L _ stmt) = case stmt of
  BodyStmt _ body _ _ -> extractQQFromExpr path body
  BindStmt _ _ body -> extractQQFromExpr path body
  LetStmt _ binds -> extractQQFromLocalBinds path binds
  _ -> []

-- | Extract quasiquote from a splice (located)
extractQQFromSplice :: FilePath -> GenLocated l (HsUntypedSplice GhcPs) -> [QuasiQuoteInfo]
extractQQFromSplice path (L _ splice) = extractQQFromSplice' path splice

-- | Extract quasiquote from a splice (unlocated)
extractQQFromSplice' :: FilePath -> HsUntypedSplice GhcPs -> [QuasiQuoteInfo]
extractQQFromSplice' path splice = case splice of
  HsQuasiQuote _ quoter (L _ quotedStr) -> [QuasiQuoteInfo
    { qqiSpan = noSrcSpan { srcSpanFile = path }
    , qqiQuoter = rdrNameToText quoter
    , qqiContents = T.pack $ unpackFS quotedStr
    }]
  _ -> []

--------------------------------------------------------------------------------
-- TH Quote (Name Quote) Extraction
--------------------------------------------------------------------------------

-- | Extract TH name quotes from a module ('name and ''Type)
extractThQuotes :: FilePath -> HsModule GhcPs -> [ThQuoteInfo]
extractThQuotes path m = concatMap (extractQuotesFromDecl path) (hsmodDecls m)

-- | Extract quotes from a declaration
extractQuotesFromDecl :: FilePath -> LHsDecl GhcPs -> [ThQuoteInfo]
extractQuotesFromDecl path (L _ (ValD _ bind)) =
  extractQuotesFromBind path bind
extractQuotesFromDecl _ _ = []

-- | Extract quotes from a binding
extractQuotesFromBind :: FilePath -> HsBind GhcPs -> [ThQuoteInfo]
extractQuotesFromBind path (FunBind _ _ mg) =
  extractQuotesFromMatchGroup path mg
extractQuotesFromBind path (PatBind _ _ _ grhss) =  -- GHC 9.10: 4 args
  extractQuotesFromGRHSs path grhss
extractQuotesFromBind _ _ = []

-- | Extract quotes from a match group
extractQuotesFromMatchGroup :: FilePath -> MatchGroup GhcPs (LHsExpr GhcPs) -> [ThQuoteInfo]
extractQuotesFromMatchGroup path (MG _ (L _ matches)) =
  concatMap (extractQuotesFromMatch path) matches

-- | Extract quotes from a match
extractQuotesFromMatch :: FilePath -> LMatch GhcPs (LHsExpr GhcPs) -> [ThQuoteInfo]
extractQuotesFromMatch path (L _ (Match _ _ _ grhss)) =
  extractQuotesFromGRHSs path grhss

-- | Extract quotes from GRHSs
extractQuotesFromGRHSs :: FilePath -> GRHSs GhcPs (LHsExpr GhcPs) -> [ThQuoteInfo]
extractQuotesFromGRHSs path (GRHSs _ grhss localBinds) =
  concatMap (extractQuotesFromGRHS path) grhss
  ++ extractQuotesFromLocalBinds path localBinds

-- | Extract quotes from a GRHS
extractQuotesFromGRHS :: FilePath -> LGRHS GhcPs (LHsExpr GhcPs) -> [ThQuoteInfo]
extractQuotesFromGRHS path (L _ (GRHS _ _ expr)) =
  extractQuotesFromExpr path expr

-- | Extract quotes from local bindings
extractQuotesFromLocalBinds :: FilePath -> HsLocalBinds GhcPs -> [ThQuoteInfo]
extractQuotesFromLocalBinds path (HsValBinds _ (ValBinds _ binds _)) =
  concatMap (extractQuotesFromBind path . unLoc) (bagToList binds)
extractQuotesFromLocalBinds _ _ = []

-- | Extract quotes from an expression
extractQuotesFromExpr :: FilePath -> LHsExpr GhcPs -> [ThQuoteInfo]
extractQuotesFromExpr path (L loc expr) = case expr of
  HsUntypedBracket _ bracket ->
    extractQuotesFromBracket path (locA loc) bracket
  HsTypedBracket _ inner ->
    extractQuotesFromExpr path inner
  HsApp _ f a ->
    extractQuotesFromExpr path f ++ extractQuotesFromExpr path a
  HsLam _ _ mg ->  -- GHC 9.10: 3 args
    extractQuotesFromMatchGroup path mg
  HsCase _ scrut mg ->
    extractQuotesFromExpr path scrut ++ extractQuotesFromMatchGroup path mg
  HsLet _ binds body ->  -- GHC 9.10: 3 args
    extractQuotesFromLocalBinds path binds ++ extractQuotesFromExpr path body
  HsIf _ cond t f ->
    extractQuotesFromExpr path cond
    ++ extractQuotesFromExpr path t
    ++ extractQuotesFromExpr path f
  HsDo _ _ (L _ stmts) ->
    concatMap (extractQuotesFromStmt path) stmts
  ExplicitList _ exprs ->
    concatMap (extractQuotesFromExpr path) exprs
  HsPar _ inner ->  -- GHC 9.4: 4 args
    extractQuotesFromExpr path inner
  OpApp _ l op r ->
    extractQuotesFromExpr path l
    ++ extractQuotesFromExpr path op
    ++ extractQuotesFromExpr path r
  _ -> []

-- | Extract quotes from a statement
extractQuotesFromStmt :: FilePath -> LStmt GhcPs (LHsExpr GhcPs) -> [ThQuoteInfo]
extractQuotesFromStmt path (L _ stmt) = case stmt of
  BodyStmt _ body _ _ -> extractQuotesFromExpr path body
  BindStmt _ _ body -> extractQuotesFromExpr path body
  LetStmt _ binds -> extractQuotesFromLocalBinds path binds
  _ -> []

-- | Extract quotes from a bracket
extractQuotesFromBracket :: FilePath -> GHC.SrcSpan -> HsQuote GhcPs -> [ThQuoteInfo]
extractQuotesFromBracket path loc bracket = case bracket of
  ExpBr _ expr -> [ThQuoteInfo
    { tqiSpan = spanToSrcSpan path loc
    , tqiName = T.pack $ showSDocUnsafe $ ppr expr
    , tqiIsType = False
    }]
  TypBr _ ty -> [ThQuoteInfo
    { tqiSpan = spanToSrcSpan path loc
    , tqiName = T.pack $ showSDocUnsafe $ ppr ty
    , tqiIsType = True
    }]
  VarBr _ _ name -> [ThQuoteInfo
    { tqiSpan = spanToSrcSpan path loc
    , tqiName = rdrNameToText $ unLoc name
    , tqiIsType = False
    }]
  DecBrL _ decls -> concatMap (extractQuotesFromDecl path) decls
  DecBrG _ _ -> []
  PatBr _ _ -> []

--------------------------------------------------------------------------------
-- LANGUAGE Pragma Extraction
--------------------------------------------------------------------------------

-- | Extract LANGUAGE pragmas from source text
-- This uses text-based extraction since pragmas are processed before parsing
extractPragmas :: FilePath -> Text -> [PragmaInfo]
extractPragmas path source =
  let allLines = zip [1..] (T.lines source)
  in concatMap (extractPragmaFromLine path) allLines

-- | Extract pragmas from a single line
extractPragmaFromLine :: FilePath -> (Int, Text) -> [PragmaInfo]
extractPragmaFromLine path (lineNum, line) =
  let stripped = T.strip line
  in if "{-#" `T.isPrefixOf` stripped && "LANGUAGE" `T.isInfixOf` stripped
     then parsePragmaLine path lineNum stripped
     else []

-- | Parse a pragma line into individual extension pragmas
parsePragmaLine :: FilePath -> Int -> Text -> [PragmaInfo]
parsePragmaLine path lineNum line =
  let -- Remove {-# and #-}
      withoutBrackets = T.dropWhileEnd (== '}') $ T.dropWhileEnd (== '#') $
                        T.dropWhileEnd (== '-') $ T.drop 3 $ T.strip line
      -- Find LANGUAGE keyword and get extensions
      afterLanguage = case T.breakOn "LANGUAGE" withoutBrackets of
        (_, rest) | not (T.null rest) -> T.drop 8 rest  -- Drop "LANGUAGE"
        _ -> ""
      -- Split by comma and parse each extension
      extensions = map T.strip $ T.splitOn "," afterLanguage
  in mapMaybe (parseSingleExtension path lineNum) extensions

-- | Parse a single extension
parseSingleExtension :: FilePath -> Int -> Text -> Maybe PragmaInfo
parseSingleExtension path lineNum ext
  | T.null ext = Nothing
  | "No" `T.isPrefixOf` ext = Just PragmaInfo
      { piSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length ext + 1)
      , piExtension = T.drop 2 ext
      , piIsEnabled = False
      }
  | otherwise = Just PragmaInfo
      { piSpan = mkSrcSpanRaw path lineNum 1 lineNum (T.length ext + 1)
      , piExtension = ext
      , piIsEnabled = True
      }

--------------------------------------------------------------------------------
-- Instance Extraction
--------------------------------------------------------------------------------

-- | Extract all instance declarations from a module
extractInstances :: FilePath -> Text -> HsModule GhcPs -> [InstanceInfo]
extractInstances path moduleName m =
  let decls = hsmodDecls m
      typeDecls = extractTypeNames m
  in mapMaybe (extractInstance path moduleName typeDecls) decls

-- | Extract type names defined in this module
extractTypeNames :: HsModule GhcPs -> Set Text
extractTypeNames m = Set.fromList $ mapMaybe getTypeName (hsmodDecls m)
  where
    getTypeName (L _ (TyClD _ decl)) = case decl of
      DataDecl{tcdLName = L _ name} -> Just $ rdrNameToText name
      SynDecl{tcdLName = L _ name} -> Just $ rdrNameToText name
      _ -> Nothing
    getTypeName _ = Nothing

-- | Extract instance info from a declaration
extractInstance :: FilePath -> Text -> Set Text -> LHsDecl GhcPs -> Maybe InstanceInfo
extractInstance path moduleName localTypes (L loc (InstD _ instDecl)) =
  case instDecl of
    ClsInstD _ clsInstDecl -> extractClsInstance path moduleName localTypes loc clsInstDecl
    _ -> Nothing
extractInstance _ _ _ _ = Nothing

-- | Extract class instance info
extractClsInstance :: FilePath -> Text -> Set Text -> SrcSpanAnnA -> ClsInstDecl GhcPs -> Maybe InstanceInfo
extractClsInstance path moduleName localTypes loc clsInst =
  let instType = cid_poly_ty clsInst
      (className, typeName, fullType) = extractInstanceHead instType
      isOrphan = isPotentialOrphan moduleName localTypes className typeName
  in Just InstanceInfo
    { iiInstanceSpan = spanToSrcSpan path (locA loc)
    , iiClassName = className
    , iiTypeName = typeName
    , iiFullType = fullType
    , iiIsDeriving = False
    , iiIsOrphan = isOrphan
    }

-- | Extract class name and type name from an instance head
extractInstanceHead :: LHsSigType GhcPs -> (Text, Text, Text)
extractInstanceHead (L _ (HsSig _ _ (L _ ty))) =
  let fullType = T.pack $ showSDocUnsafe $ ppr ty
      (className, typeName) = parseInstanceType ty
  in (className, typeName, fullType)

-- | Parse the instance type to extract class and type names
parseInstanceType :: HsType GhcPs -> (Text, Text)
parseInstanceType (HsAppTy _ (L _ cls) (L _ arg)) =
  (extractTypeName cls, extractTypeName arg)
parseInstanceType (HsParTy _ (L _ inner)) = parseInstanceType inner
parseInstanceType (HsQualTy _ _ (L _ inner)) = parseInstanceType inner
parseInstanceType (HsForAllTy _ _ (L _ inner)) = parseInstanceType inner
parseInstanceType ty = (extractTypeName ty, "")

-- | Extract the name from a type
extractTypeName :: HsType GhcPs -> Text
extractTypeName (HsTyVar _ _ (L _ name)) = rdrNameToText name
extractTypeName (HsAppTy _ (L _ cls) _) = extractTypeName cls
extractTypeName (HsParTy _ (L _ inner)) = extractTypeName inner
extractTypeName (HsOpTy _ _ _ (L _ op) _) = rdrNameToText op
extractTypeName ty = T.pack $ showSDocUnsafe $ ppr ty

-- | Check if an instance is potentially an orphan
-- An orphan instance is one where neither the class nor any of the types
-- in the instance head are defined in the current module
isPotentialOrphan :: Text -> Set Text -> Text -> Text -> Bool
isPotentialOrphan moduleName localTypes className typeName =
  let -- Check if class is likely defined locally (simple heuristic: starts with module prefix)
      modulePrefix = T.takeWhile (/= '.') moduleName
      classIsLocal = T.takeWhile (/= '.') className == modulePrefix || className `Set.member` localTypes
      typeIsLocal = typeName `Set.member` localTypes
      -- Standard library classes are never local
      _isStandardClass = className `elem` standardClasses
  in not classIsLocal && not typeIsLocal && not (T.null typeName)
  where
    standardClasses =
      [ "Eq", "Ord", "Show", "Read", "Enum", "Bounded"
      , "Num", "Real", "Integral", "Fractional", "Floating", "RealFrac", "RealFloat"
      , "Functor", "Applicative", "Monad", "MonadFail", "MonadIO"
      , "Foldable", "Traversable", "Semigroup", "Monoid"
      , "ToJSON", "FromJSON", "Generic", "NFData", "Hashable"
      , "Binary", "Serialize", "Default"
      ]

