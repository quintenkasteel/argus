{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}

-- |
-- Module      : Argus.AutoFix.Template
-- Description : Template system for reusable auto-fix patterns
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides a template system for defining reusable auto-fix patterns.
-- Templates can be parameterized, composed, and instantiated with specific values.
--
-- == Architecture
--
-- The template system consists of:
--
-- 1. **Template Definitions**: Parameterized fix specifications
-- 2. **Template Parameters**: Typed parameters with validation
-- 3. **Template Instantiation**: Converting templates to concrete fixes
-- 4. **Template Library**: Pre-built templates for common patterns
--
-- == Usage
--
-- @
-- -- Define a template for replacing partial functions
-- partialFunctionTemplate :: FixTemplate
-- partialFunctionTemplate = template "replace-partial" $ do
--   param "old" TypeText "The partial function to replace"
--   param "new" TypeText "The safe replacement function"
--   param "module" TypeText "The module providing the replacement"
--
--   templateBody $ do
--     replacePattern (paramRef "old") (paramRef "new")
--     addImport (paramRef "module") [paramRef "new"]
--     safety Safe
--     category Safety
--
-- -- Instantiate the template
-- headToMaybe :: FixSpec
-- headToMaybe = instantiate partialFunctionTemplate
--   [ ("old", "head")
--   , ("new", "listToMaybe")
--   , ("module", "Data.Maybe")
--   ]
-- @
--
-- == Pre-built Templates
--
-- @
-- -- Use pre-built templates from the library
-- myFixes :: [FixSpec]
-- myFixes =
--   [ instantiate replaceWithSafeTemplate
--       [("unsafe", "head"), ("safe", "headMay"), ("safeModule", "Safe")]
--   , instantiate addStrictVersionTemplate
--       [("lazy", "foldl"), ("strict", "foldl'"), ("strictModule", "Data.List")]
--   ]
-- @
module Argus.AutoFix.Template
  ( -- * Template Types
    FixTemplate (..)
  , TemplateParam (..)
  , ParamType (..)
  , TemplateBody (..)
  , TemplateExpr (..)

    -- * Template Building
  , template
  , TemplateBuilder
  , TemplateBuilderM (..)
  , runTemplateBuilder

    -- * Parameter Definitions
  , param
  , paramWithDefault
  , optionalParam
  , listParam

    -- * Template Body Operations
  , templateBody
  , templateReplace
  , templateReplacePattern
  , templateInsert
  , templateDelete
  , templateAddImport
  , templateRemoveImport
  , templateSafety
  , templateCategory
  , templateConfidence
  , templateNote
  , templateTag

    -- * Template References
  , paramRef
  , paramRefOr
  , paramJoin
  , paramConcat

    -- * Template Instantiation
  , instantiate
  , instantiateWithDefaults
  , tryInstantiate
  , InstantiationError (..)

    -- * Template Validation
  , validateTemplate
  , validateParams
  , TemplateValidation (..)

    -- * Template Composition
  , composeTemplates
  , extendTemplate
  , overrideParam
  , hideParam

    -- * Pre-built Template Library
  , replaceWithSafeTemplate
  , addStrictVersionTemplate
  , addQualifiedImportTemplate
  , removeUnusedImportTemplate
  , wrapWithFunctionTemplate
  , unwrapFunctionTemplate
  , modernizePatternTemplate

    -- * Template Utilities
  , templateToFixSpec
  , templateParams
  , templateDefaults
  , describeTemplate
  ) where

import Control.Monad.Writer.Strict (Writer, execWriter, tell)
import Data.Aeson (ToJSON, FromJSON)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Argus.Types
  ( FixImport (..)
  , ImportSymbol (..)
  , ImportSymbolType (..)
  )

import Argus.Rules.Types
  ( Category (..)
  , SafetyLevel (..)
  )

import Argus.AutoFix.Types
  ( Confidence
  , mkConfidence
  , highConfidence
  )

import Argus.AutoFix.DSL
  ( FixSpec (..)
  , FixExpr (..)
  , defaultValidationFlags
  )

--------------------------------------------------------------------------------
-- Template Types
--------------------------------------------------------------------------------

-- | A parameterized fix template.
data FixTemplate = FixTemplate
  { ftName          :: Text
    -- ^ Unique template name
  , ftDescription   :: Text
    -- ^ Human-readable description
  , ftParams        :: [TemplateParam]
    -- ^ Parameter definitions
  , ftBody          :: TemplateBody
    -- ^ Template body
  , ftCategory      :: Category
    -- ^ Default category
  , ftSafety        :: SafetyLevel
    -- ^ Default safety level
  , ftConfidence    :: Confidence
    -- ^ Default confidence
  , ftTags          :: Set Text
    -- ^ Tags
  , ftNotes         :: [Text]
    -- ^ Notes
  }
  deriving stock (Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

instance Eq FixTemplate where
  a == b = ftName a == ftName b

-- | A template parameter definition.
data TemplateParam = TemplateParam
  { tpName        :: Text
    -- ^ Parameter name
  , tpType        :: ParamType
    -- ^ Parameter type
  , tpDescription :: Text
    -- ^ Parameter description
  , tpDefault     :: Maybe Text
    -- ^ Optional default value
  , tpRequired    :: Bool
    -- ^ Is this parameter required?
  , tpValidator   :: Maybe Text
    -- ^ Regex for validation
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Types for template parameters.
data ParamType
  = TypeText
    -- ^ Plain text value
  | TypeIdentifier
    -- ^ Haskell identifier
  | TypeModuleName
    -- ^ Module name (e.g., "Data.List")
  | TypeOperator
    -- ^ Operator (e.g., "+", "<$>")
  | TypePattern
    -- ^ Pattern expression with metavariables
  | TypeList ParamType
    -- ^ List of values
  | TypeChoice [Text]
    -- ^ One of several options
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Template body containing expressions with parameter references.
data TemplateBody = TemplateBody
  { tbExprs        :: [TemplateExpr]
    -- ^ Template expressions
  , tbAddImports   :: [TemplateImport]
    -- ^ Imports to add
  , tbRemoveImports :: [TemplateExpr]
    -- ^ Modules to remove (as expressions)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Template expressions with parameter interpolation.
data TemplateExpr
  = TELiteral Text
    -- ^ Literal text
  | TEParamRef Text
    -- ^ Reference to a parameter
  | TEParamRefOr Text Text
    -- ^ Parameter reference with default
  | TEConcat [TemplateExpr]
    -- ^ Concatenation of expressions
  | TEJoin Text [TemplateExpr]
    -- ^ Join expressions with separator
  | TEReplace TemplateExpr TemplateExpr
    -- ^ Replace (old, new)
  | TEReplacePattern TemplateExpr TemplateExpr
    -- ^ Replace pattern (old, new)
  | TEInsert Int TemplateExpr
    -- ^ Insert at offset
  | TEDelete TemplateExpr
    -- ^ Delete matching
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Template import with parameter interpolation.
data TemplateImport = TemplateImport
  { tiModule  :: TemplateExpr
    -- ^ Module name expression
  , tiSymbols :: [TemplateExpr]
    -- ^ Symbol expressions
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Template Builder
--------------------------------------------------------------------------------

-- | State for template building.
data TemplateBuilderState = TemplateBuilderState
  { tbsParams      :: [TemplateParam]
  , tbsBody        :: TemplateBody
  , tbsCategory    :: Category
  , tbsSafety      :: SafetyLevel
  , tbsConfidence  :: Confidence
  , tbsTags        :: Set Text
  , tbsNotes       :: [Text]
  , tbsDescription :: Text
  }
  deriving stock (Eq, Show, Generic)

-- | Initial builder state.
emptyTemplateBuilderState :: TemplateBuilderState
emptyTemplateBuilderState = TemplateBuilderState
  { tbsParams = []
  , tbsBody = TemplateBody [] [] []
  , tbsCategory = Style
  , tbsSafety = Safe
  , tbsConfidence = highConfidence
  , tbsTags = Set.empty
  , tbsNotes = []
  , tbsDescription = ""
  }

-- | Template builder operations.
data TemplateBuilderOp
  = TBOpAddParam TemplateParam
  | TBOpAddExpr TemplateExpr
  | TBOpAddImport TemplateImport
  | TBOpRemoveImport TemplateExpr
  | TBOpSetCategory Category
  | TBOpSetSafety SafetyLevel
  | TBOpSetConfidence Confidence
  | TBOpAddTag Text
  | TBOpAddNote Text
  | TBOpSetDescription Text
  deriving stock (Eq, Show, Generic)

-- | Template builder monad.
newtype TemplateBuilderM a = TemplateBuilderM (Writer [TemplateBuilderOp] a)
  deriving newtype (Functor, Applicative, Monad)

-- | Type alias for the builder.
type TemplateBuilder = TemplateBuilderM ()

-- | Emit a builder operation.
emitTB :: TemplateBuilderOp -> TemplateBuilder
emitTB op = TemplateBuilderM (tell [op])

-- | Run the template builder and extract the state.
runTemplateBuilder :: TemplateBuilder -> TemplateBuilderState
runTemplateBuilder (TemplateBuilderM wrtr) = applyOps (execWriter wrtr) emptyTemplateBuilderState
  where
    applyOps :: [TemplateBuilderOp] -> TemplateBuilderState -> TemplateBuilderState
    applyOps [] st = st
    applyOps (op:ops) st = applyOps ops (applyOp op st)

    applyOp :: TemplateBuilderOp -> TemplateBuilderState -> TemplateBuilderState
    applyOp op st = case op of
      TBOpAddParam prm ->
        st { tbsParams = prm : tbsParams st }
      TBOpAddExpr ex ->
        let body = tbsBody st
        in st { tbsBody = body { tbExprs = ex : tbExprs body } }
      TBOpAddImport imp ->
        let body = tbsBody st
        in st { tbsBody = body { tbAddImports = imp : tbAddImports body } }
      TBOpRemoveImport modExpr ->
        let body = tbsBody st
        in st { tbsBody = body { tbRemoveImports = modExpr : tbRemoveImports body } }
      TBOpSetCategory cat ->
        st { tbsCategory = cat }
      TBOpSetSafety saf ->
        st { tbsSafety = saf }
      TBOpSetConfidence conf ->
        st { tbsConfidence = conf }
      TBOpAddTag tg ->
        st { tbsTags = Set.insert tg (tbsTags st) }
      TBOpAddNote nt ->
        st { tbsNotes = nt : tbsNotes st }
      TBOpSetDescription desc ->
        st { tbsDescription = desc }

-- | Create a template.
template :: Text -> TemplateBuilder -> FixTemplate
template name builder =
  let st = runTemplateBuilder builder
      body = tbsBody st
  in FixTemplate
    { ftName = name
    , ftDescription = if T.null (tbsDescription st)
                      then "Template: " <> name
                      else tbsDescription st
    , ftParams = reverse (tbsParams st)
    , ftBody = body { tbExprs = reverse (tbExprs body)
                    , tbAddImports = reverse (tbAddImports body)
                    , tbRemoveImports = reverse (tbRemoveImports body)
                    }
    , ftCategory = tbsCategory st
    , ftSafety = tbsSafety st
    , ftConfidence = tbsConfidence st
    , ftTags = tbsTags st
    , ftNotes = reverse (tbsNotes st)
    }

--------------------------------------------------------------------------------
-- Parameter Definitions
--------------------------------------------------------------------------------

-- | Define a required parameter.
param :: Text -> ParamType -> Text -> TemplateBuilder
param name ptype desc = emitTB $ TBOpAddParam TemplateParam
  { tpName = name
  , tpType = ptype
  , tpDescription = desc
  , tpDefault = Nothing
  , tpRequired = True
  , tpValidator = Nothing
  }

-- | Define a parameter with default value.
paramWithDefault :: Text -> ParamType -> Text -> Text -> TemplateBuilder
paramWithDefault name ptype desc defVal = emitTB $ TBOpAddParam TemplateParam
  { tpName = name
  , tpType = ptype
  , tpDescription = desc
  , tpDefault = Just defVal
  , tpRequired = False
  , tpValidator = Nothing
  }

-- | Define an optional parameter (no default, not required).
optionalParam :: Text -> ParamType -> Text -> TemplateBuilder
optionalParam name ptype desc = emitTB $ TBOpAddParam TemplateParam
  { tpName = name
  , tpType = ptype
  , tpDescription = desc
  , tpDefault = Nothing
  , tpRequired = False
  , tpValidator = Nothing
  }

-- | Define a list parameter.
listParam :: Text -> ParamType -> Text -> TemplateBuilder
listParam name innerType desc = param name (TypeList innerType) desc

--------------------------------------------------------------------------------
-- Template Body Operations
--------------------------------------------------------------------------------

-- | Begin template body definition (marker for readability).
templateBody :: TemplateBuilder -> TemplateBuilder
templateBody = id

-- | Replace text in template.
templateReplace :: TemplateExpr -> TemplateExpr -> TemplateBuilder
templateReplace old new = emitTB $ TBOpAddExpr $ TEReplace old new

-- | Replace pattern in template.
templateReplacePattern :: TemplateExpr -> TemplateExpr -> TemplateBuilder
templateReplacePattern old new = emitTB $ TBOpAddExpr $ TEReplacePattern old new

-- | Insert text in template.
templateInsert :: Int -> TemplateExpr -> TemplateBuilder
templateInsert offset text = emitTB $ TBOpAddExpr $ TEInsert offset text

-- | Delete text in template.
templateDelete :: TemplateExpr -> TemplateBuilder
templateDelete target = emitTB $ TBOpAddExpr $ TEDelete target

-- | Add import in template.
templateAddImport :: TemplateExpr -> [TemplateExpr] -> TemplateBuilder
templateAddImport modExpr symbolExprs = emitTB $ TBOpAddImport TemplateImport
  { tiModule = modExpr
  , tiSymbols = symbolExprs
  }

-- | Remove import in template.
templateRemoveImport :: TemplateExpr -> TemplateBuilder
templateRemoveImport modExpr = emitTB $ TBOpRemoveImport modExpr

-- | Set safety level in template.
templateSafety :: SafetyLevel -> TemplateBuilder
templateSafety = emitTB . TBOpSetSafety

-- | Set category in template.
templateCategory :: Category -> TemplateBuilder
templateCategory = emitTB . TBOpSetCategory

-- | Set confidence in template.
templateConfidence :: Double -> TemplateBuilder
templateConfidence = emitTB . TBOpSetConfidence . mkConfidence

-- | Add note to template.
templateNote :: Text -> TemplateBuilder
templateNote = emitTB . TBOpAddNote

-- | Add tag to template.
templateTag :: Text -> TemplateBuilder
templateTag = emitTB . TBOpAddTag

--------------------------------------------------------------------------------
-- Template References
--------------------------------------------------------------------------------

-- | Reference a parameter by name.
paramRef :: Text -> TemplateExpr
paramRef = TEParamRef

-- | Reference a parameter with a default value.
paramRefOr :: Text -> Text -> TemplateExpr
paramRefOr = TEParamRefOr

-- | Join multiple expressions with a separator.
paramJoin :: Text -> [TemplateExpr] -> TemplateExpr
paramJoin = TEJoin

-- | Concatenate expressions.
paramConcat :: [TemplateExpr] -> TemplateExpr
paramConcat = TEConcat

--------------------------------------------------------------------------------
-- Template Instantiation
--------------------------------------------------------------------------------

-- | Errors that can occur during instantiation.
data InstantiationError
  = MissingRequiredParam Text
    -- ^ Required parameter not provided
  | InvalidParamType Text ParamType Text
    -- ^ Parameter value doesn't match type
  | ParamValidationFailed Text Text
    -- ^ Parameter failed validation regex
  | UnknownParam Text
    -- ^ Unknown parameter provided
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Instantiate a template with parameter values.
instantiate :: FixTemplate -> [(Text, Text)] -> FixSpec
instantiate tmpl params =
  case tryInstantiate tmpl params of
    Right spec -> spec
    Left errs -> error $ "Template instantiation failed: " <> show errs

-- | Instantiate with defaults for missing optional parameters.
instantiateWithDefaults :: FixTemplate -> [(Text, Text)] -> FixSpec
instantiateWithDefaults tmpl params =
  let defaults = [(tpName p, d) | p <- ftParams tmpl, Just d <- [tpDefault p]]
      combined = params ++ defaults
  in instantiate tmpl combined

-- | Try to instantiate, returning errors if any.
tryInstantiate :: FixTemplate -> [(Text, Text)] -> Either [InstantiationError] FixSpec
tryInstantiate tmpl params = do
  -- Validate parameters
  errors <- validateInstantiation tmpl params
  if not (null errors)
    then Left errors
    else Right $ instantiateImpl tmpl (Map.fromList params)

-- | Validate instantiation parameters.
validateInstantiation :: FixTemplate -> [(Text, Text)] -> Either a [InstantiationError]
validateInstantiation tmpl params =
  let paramMap = Map.fromList params
      paramNames = Set.fromList (map fst params)
      templateParamNames = Set.fromList (map tpName (ftParams tmpl))

      -- Check for missing required params
      missingErrors =
        [ MissingRequiredParam (tpName p)
        | p <- ftParams tmpl
        , tpRequired p
        , tpName p `Map.notMember` paramMap
        ]

      -- Check for unknown params
      unknownErrors =
        [ UnknownParam name
        | name <- Set.toList paramNames
        , name `Set.notMember` templateParamNames
        ]

  in Right $ missingErrors ++ unknownErrors

-- | Internal implementation of instantiation.
instantiateImpl :: FixTemplate -> Map Text Text -> FixSpec
instantiateImpl tmpl paramMap =
  let body = ftBody tmpl
      -- Resolve all template expressions
      resolvedExprs = map (resolveExpr paramMap) (tbExprs body)
      resolvedImports = map (resolveImport paramMap) (tbAddImports body)
      resolvedRemoves = map (resolveExprText paramMap) (tbRemoveImports body)
  in FixSpec
    { fsName = ftName tmpl <> "-" <> T.intercalate "-" (Map.elems paramMap)
    , fsDescription = ftDescription tmpl
    , fsExprs = resolvedExprs
    , fsAddImports = resolvedImports
    , fsRemoveImports = resolvedRemoves
    , fsConfidence = ftConfidence tmpl
    , fsSafety = ftSafety tmpl
    , fsCategory = ftCategory tmpl
    , fsTags = ftTags tmpl
    , fsNotes = ftNotes tmpl
    , fsExplanation = Nothing
    , fsSourceRule = Just (ftName tmpl)
    , fsWithin = []
    , fsExcept = []
    , fsPriority = 0
    , fsDependencies = Set.empty
    , fsConflicts = Set.empty
    , fsValidation = defaultValidationFlags
    }

-- | Resolve a template expression to a fix expression.
resolveExpr :: Map Text Text -> TemplateExpr -> FixExpr
resolveExpr paramMap = \case
  TELiteral txt -> FEReplace txt txt  -- No-op for literal
  TEParamRef name -> case Map.lookup name paramMap of
    Just val -> FEReplace "" val  -- Will be used in context
    Nothing -> FEReplace "" ""
  TEParamRefOr name defVal -> case Map.lookup name paramMap of
    Just val -> FEReplace "" val
    Nothing -> FEReplace "" defVal
  TEConcat exprs ->
    FEReplace "" (T.concat (map (resolveExprText paramMap) exprs))
  TEJoin sep exprs ->
    FEReplace "" (T.intercalate sep (map (resolveExprText paramMap) exprs))
  TEReplace oldExpr newExpr ->
    FEReplacePattern (resolveExprText paramMap oldExpr) (resolveExprText paramMap newExpr)
  TEReplacePattern oldExpr newExpr ->
    FEReplacePattern (resolveExprText paramMap oldExpr) (resolveExprText paramMap newExpr)
  TEInsert offset textExpr ->
    FEInsert offset (resolveExprText paramMap textExpr)
  TEDelete targetExpr ->
    FEDelete (resolveExprText paramMap targetExpr)

-- | Resolve a template expression to text.
resolveExprText :: Map Text Text -> TemplateExpr -> Text
resolveExprText paramMap = \case
  TELiteral txt -> txt
  TEParamRef name -> Map.findWithDefault "" name paramMap
  TEParamRefOr name defVal -> Map.findWithDefault defVal name paramMap
  TEConcat exprs -> T.concat (map (resolveExprText paramMap) exprs)
  TEJoin sep exprs -> T.intercalate sep (map (resolveExprText paramMap) exprs)
  TEReplace _ newExpr -> resolveExprText paramMap newExpr
  TEReplacePattern _ newExpr -> resolveExprText paramMap newExpr
  TEInsert _ textExpr -> resolveExprText paramMap textExpr
  TEDelete targetExpr -> resolveExprText paramMap targetExpr

-- | Resolve a template import to a fix import.
resolveImport :: Map Text Text -> TemplateImport -> FixImport
resolveImport paramMap TemplateImport{..} = FixImport
  { fimpModule = resolveExprText paramMap tiModule
  , fimpSymbols = map (mkSymbol . resolveExprText paramMap) tiSymbols
  , fimpQualified = Nothing
  , fimpHiding = False
  , fimpPackage = Nothing
  }
  where
    mkSymbol name = ImportSymbol
      { isymName = name
      , isymType = inferType name
      , isymChildren = []
      }

    inferType :: Text -> ImportSymbolType
    inferType name
      | T.null name = ISTFunction
      | T.any (`elem` ("!#$%&*+./<=>?@\\^|-~:" :: String)) name = ISTOperator
      | T.head name >= 'A' && T.head name <= 'Z' = ISTType
      | otherwise = ISTFunction

--------------------------------------------------------------------------------
-- Template Validation
--------------------------------------------------------------------------------

-- | Result of template validation.
data TemplateValidation = TemplateValidation
  { tvValid    :: Bool
    -- ^ Is the template valid?
  , tvErrors   :: [Text]
    -- ^ Validation errors
  , tvWarnings :: [Text]
    -- ^ Validation warnings
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Validate a template definition.
validateTemplate :: FixTemplate -> TemplateValidation
validateTemplate tmpl =
  let paramErrors = validateParams (ftParams tmpl)
      bodyErrors = validateBody (ftBody tmpl) (map tpName (ftParams tmpl))
      allErrors = paramErrors ++ bodyErrors
  in TemplateValidation
    { tvValid = null allErrors
    , tvErrors = allErrors
    , tvWarnings = []
    }

-- | Validate template parameters.
validateParams :: [TemplateParam] -> [Text]
validateParams params =
  let names = map tpName params
      duplicates = findDuplicates names
      emptyNames = [tpName p | p <- params, T.null (tpName p)]
  in map ("Duplicate parameter: " <>) duplicates ++
     map (const "Empty parameter name") emptyNames
  where
    findDuplicates :: [Text] -> [Text]
    findDuplicates xs = Set.toList $ go Set.empty Set.empty xs
      where
        go _ dups [] = dups
        go seen dups (x:rest)
          | x `Set.member` seen = go seen (Set.insert x dups) rest
          | otherwise = go (Set.insert x seen) dups rest

-- | Validate template body.
validateBody :: TemplateBody -> [Text] -> [Text]
validateBody body paramNames =
  let exprRefs = concatMap collectRefs (tbExprs body)
      importRefs = concatMap collectImportRefs (tbAddImports body)
      removeRefs = concatMap collectRefs (tbRemoveImports body)
      allRefs = exprRefs ++ importRefs ++ removeRefs
      unknownRefs = filter (`notElem` paramNames) allRefs
  in map (\r -> "Unknown parameter reference: " <> r) unknownRefs

-- | Collect parameter references from an expression.
collectRefs :: TemplateExpr -> [Text]
collectRefs = \case
  TELiteral _ -> []
  TEParamRef name -> [name]
  TEParamRefOr name _ -> [name]
  TEConcat exprs -> concatMap collectRefs exprs
  TEJoin _ exprs -> concatMap collectRefs exprs
  TEReplace old new -> collectRefs old ++ collectRefs new
  TEReplacePattern old new -> collectRefs old ++ collectRefs new
  TEInsert _ text -> collectRefs text
  TEDelete target -> collectRefs target

-- | Collect refs from a template import.
collectImportRefs :: TemplateImport -> [Text]
collectImportRefs TemplateImport{..} =
  collectRefs tiModule ++ concatMap collectRefs tiSymbols

--------------------------------------------------------------------------------
-- Template Composition
--------------------------------------------------------------------------------

-- | Compose two templates by sequencing their bodies.
composeTemplates :: FixTemplate -> FixTemplate -> FixTemplate
composeTemplates t1 t2 = FixTemplate
  { ftName = ftName t1 <> "-then-" <> ftName t2
  , ftDescription = ftDescription t1 <> " then " <> ftDescription t2
  , ftParams = ftParams t1 ++ ftParams t2
  , ftBody = TemplateBody
      { tbExprs = tbExprs (ftBody t1) ++ tbExprs (ftBody t2)
      , tbAddImports = tbAddImports (ftBody t1) ++ tbAddImports (ftBody t2)
      , tbRemoveImports = tbRemoveImports (ftBody t1) ++ tbRemoveImports (ftBody t2)
      }
  , ftCategory = ftCategory t1
  , ftSafety = max (ftSafety t1) (ftSafety t2)
  , ftConfidence = min (ftConfidence t1) (ftConfidence t2)
  , ftTags = Set.union (ftTags t1) (ftTags t2)
  , ftNotes = ftNotes t1 ++ ftNotes t2
  }

-- | Extend a template with additional parameters and body.
extendTemplate :: FixTemplate -> TemplateBuilder -> FixTemplate
extendTemplate base builder =
  let ext = runTemplateBuilder builder
      baseBody = ftBody base
      extBody = tbsBody ext
  in base
    { ftParams = ftParams base ++ reverse (tbsParams ext)
    , ftBody = TemplateBody
        { tbExprs = tbExprs baseBody ++ reverse (tbExprs extBody)
        , tbAddImports = tbAddImports baseBody ++ reverse (tbAddImports extBody)
        , tbRemoveImports = tbRemoveImports baseBody ++ reverse (tbRemoveImports extBody)
        }
    , ftTags = Set.union (ftTags base) (tbsTags ext)
    , ftNotes = ftNotes base ++ reverse (tbsNotes ext)
    }

-- | Override a parameter's default value.
overrideParam :: Text -> Text -> FixTemplate -> FixTemplate
overrideParam name newDefault tmpl =
  tmpl { ftParams = map override (ftParams tmpl) }
  where
    override p
      | tpName p == name = p { tpDefault = Just newDefault }
      | otherwise = p

-- | Hide a parameter by making it non-required with a default.
hideParam :: Text -> Text -> FixTemplate -> FixTemplate
hideParam name defValue tmpl =
  tmpl { ftParams = map hide (ftParams tmpl) }
  where
    hide p
      | tpName p == name = p { tpRequired = False, tpDefault = Just defValue }
      | otherwise = p

--------------------------------------------------------------------------------
-- Pre-built Template Library
--------------------------------------------------------------------------------

-- | Template for replacing unsafe functions with safe alternatives.
replaceWithSafeTemplate :: FixTemplate
replaceWithSafeTemplate = template "replace-with-safe" $ do
  emitTB $ TBOpSetDescription "Replace an unsafe function with a safe alternative"
  param "unsafe" TypeIdentifier "The unsafe function to replace"
  param "safe" TypeIdentifier "The safe replacement function"
  param "safeModule" TypeModuleName "Module providing the safe function"

  templateBody $ do
    templateReplacePattern (paramRef "unsafe") (paramRef "safe")
    templateAddImport (paramRef "safeModule") [paramRef "safe"]
    templateSafety Safe
    templateCategory Safety
    templateConfidence 0.95

-- | Template for adding strict versions of lazy functions.
addStrictVersionTemplate :: FixTemplate
addStrictVersionTemplate = template "add-strict-version" $ do
  emitTB $ TBOpSetDescription "Replace a lazy function with its strict version"
  param "lazy" TypeIdentifier "The lazy function to replace"
  param "strict" TypeIdentifier "The strict replacement"
  param "strictModule" TypeModuleName "Module providing the strict function"

  templateBody $ do
    templateReplacePattern (paramRef "lazy") (paramRef "strict")
    templateAddImport (paramRef "strictModule") [paramRef "strict"]
    templateSafety Safe
    templateCategory Performance
    templateConfidence 0.9
    templateNote "Strict versions help avoid space leaks"

-- | Template for adding a qualified import.
addQualifiedImportTemplate :: FixTemplate
addQualifiedImportTemplate = template "add-qualified-import" $ do
  emitTB $ TBOpSetDescription "Add a qualified import"
  param "module" TypeModuleName "The module to import"
  param "qualifier" TypeIdentifier "The qualifier to use"

  templateBody $ do
    templateCategory Imports
    templateSafety Safe
    templateConfidence 1.0

-- | Template for removing unused imports.
removeUnusedImportTemplate :: FixTemplate
removeUnusedImportTemplate = template "remove-unused-import" $ do
  emitTB $ TBOpSetDescription "Remove an unused import"
  param "module" TypeModuleName "The unused module to remove"

  templateBody $ do
    templateRemoveImport (paramRef "module")
    templateCategory Imports
    templateSafety Safe
    templateConfidence 0.95

-- | Template for wrapping an expression with a function.
wrapWithFunctionTemplate :: FixTemplate
wrapWithFunctionTemplate = template "wrap-with-function" $ do
  emitTB $ TBOpSetDescription "Wrap an expression with a function call"
  param "pattern" TypePattern "The pattern to match"
  param "wrapper" TypeIdentifier "The function to wrap with"
  param "wrapperModule" TypeModuleName "Module providing the wrapper function"

  templateBody $ do
    templateReplacePattern
      (paramRef "pattern")
      (paramConcat [paramRef "wrapper", TELiteral " (", paramRef "pattern", TELiteral ")"])
    templateAddImport (paramRef "wrapperModule") [paramRef "wrapper"]
    templateSafety MostlySafe
    templateCategory Style
    templateConfidence 0.85

-- | Template for unwrapping a function application.
unwrapFunctionTemplate :: FixTemplate
unwrapFunctionTemplate = template "unwrap-function" $ do
  emitTB $ TBOpSetDescription "Remove a function wrapper"
  param "wrapper" TypeIdentifier "The function wrapper to remove"
  param "inner" TypePattern "The inner expression pattern"

  templateBody $ do
    templateReplacePattern
      (paramConcat [paramRef "wrapper", TELiteral " (", paramRef "inner", TELiteral ")"])
      (paramRef "inner")
    templateSafety NeedsReview
    templateCategory Style
    templateConfidence 0.7

-- | Template for modernizing patterns.
modernizePatternTemplate :: FixTemplate
modernizePatternTemplate = template "modernize-pattern" $ do
  emitTB $ TBOpSetDescription "Replace an old pattern with a modern equivalent"
  param "oldPattern" TypePattern "The old pattern to match"
  param "newPattern" TypePattern "The modern replacement"
  paramWithDefault "note" TypeText "Additional notes" ""

  templateBody $ do
    templateReplacePattern (paramRef "oldPattern") (paramRef "newPattern")
    templateCategory Modernization
    templateSafety Safe
    templateConfidence 0.9

--------------------------------------------------------------------------------
-- Template Utilities
--------------------------------------------------------------------------------

-- | Convert a template to a FixSpec (requires all params to have defaults).
templateToFixSpec :: FixTemplate -> Maybe FixSpec
templateToFixSpec tmpl
  | all hasDefault (ftParams tmpl) =
      Just $ instantiateWithDefaults tmpl []
  | otherwise = Nothing
  where
    hasDefault p = tpDefault p /= Nothing || not (tpRequired p)

-- | Get the list of parameter names.
templateParams :: FixTemplate -> [Text]
templateParams = map tpName . ftParams

-- | Get parameter defaults as a map.
templateDefaults :: FixTemplate -> Map Text Text
templateDefaults tmpl = Map.fromList
  [ (tpName p, d) | p <- ftParams tmpl, Just d <- [tpDefault p] ]

-- | Generate a human-readable description of the template.
describeTemplate :: FixTemplate -> Text
describeTemplate tmpl = T.unlines
  [ "Template: " <> ftName tmpl
  , "Description: " <> ftDescription tmpl
  , ""
  , "Parameters:"
  , T.unlines
      [ "  " <> tpName p <> " (" <> describeType (tpType p) <> ")"
        <> (if tpRequired p then " [required]" else "")
        <> maybe "" (\d -> " [default: " <> d <> "]") (tpDefault p)
        <> ": " <> tpDescription p
      | p <- ftParams tmpl
      ]
  , ""
  , "Category: " <> T.pack (show (ftCategory tmpl))
  , "Safety: " <> T.pack (show (ftSafety tmpl))
  ]
  where
    describeType :: ParamType -> Text
    describeType = \case
      TypeText -> "text"
      TypeIdentifier -> "identifier"
      TypeModuleName -> "module"
      TypeOperator -> "operator"
      TypePattern -> "pattern"
      TypeList inner -> "[" <> describeType inner <> "]"
      TypeChoice opts -> "one of: " <> T.intercalate ", " opts
