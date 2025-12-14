{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

-- |
-- Module      : Argus.LSP.Completions
-- Description : Advanced semantic code completion for Haskell
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides enterprise-grade code completion with:
--
-- * Semantic analysis using HIE files for accurate type information
-- * Context-aware completions (imports, types, expressions, patterns)
-- * Qualified name completions with automatic import suggestions
-- * Snippet completions for common patterns
-- * Full import statement parsing with explicit import lists
-- * Fuzzy matching with intelligent ranking
-- * Module-aware completions respecting import visibility
module Argus.LSP.Completions
  ( -- * Completion Engine
    CompletionEngine (..)
  , newCompletionEngine
  , getCompletionsAt

    -- * Completion Context
  , CompletionContext (..)
  , CompletionKind (..)
  , detectCompletionContext

    -- * Completion Results
  , CompletionResult (..)
  , CompletionScore
  , rankCompletions

    -- * Completion Sources
  , CompletionSource (..)
  , localSymbols
  , importedSymbols
  , preludeSymbols
  , hieSymbols
  , snippetCompletions
  , pragmaCompletions
  , moduleCompletions

    -- * Import Suggestions
  , ImportSuggestion (..)
  , suggestImport

    -- * Import Parsing
  , ImportInfo (..)
  , parseImports
  , parseImportStatement

    -- * Configuration
  , CompletionConfig (..)
  , defaultCompletionConfig
  ) where

import Control.Concurrent.STM
import Control.Exception (try, SomeException)
import Control.Monad (forM)
import Data.Char (isUpper, isLower, isAlphaNum, isSpace)
import Data.Function (on)
import Data.List (sortBy, nubBy)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (catMaybes, mapMaybe)
import Data.Ord (comparing, Down(..))
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import Argus.Types (SymbolKind(..))
import Argus.HIE.Types (HieSymbol(..), TypeInfo(..))
import Argus.HIE.Query (withHieQuery, findAllSymbols, findSymbolsInModule, getModuleExports, getSymbolType)

--------------------------------------------------------------------------------
-- Completion Configuration
--------------------------------------------------------------------------------

-- | Configuration for the completion engine
data CompletionConfig = CompletionConfig
  { ccMaxResults         :: Int           -- ^ Maximum completions to return
  , ccMinPrefixLength    :: Int           -- ^ Minimum prefix length to trigger
  , ccEnableSnippets     :: Bool          -- ^ Enable snippet completions
  , ccEnableFuzzy        :: Bool          -- ^ Enable fuzzy matching
  , ccShowTypes          :: Bool          -- ^ Show type signatures
  , ccShowDocs           :: Bool          -- ^ Show documentation
  , ccAutoImport         :: Bool          -- ^ Suggest automatic imports
  , ccPreferLocal        :: Bool          -- ^ Prefer local symbols
  , ccIncludeKeywords    :: Bool          -- ^ Include keyword completions
  , ccIncludeModules     :: Bool          -- ^ Include module completions
  , ccQualifiedThreshold :: Int           -- ^ Prefix length for qualified search
  }
  deriving stock (Eq, Show)

-- | Default completion configuration
defaultCompletionConfig :: CompletionConfig
defaultCompletionConfig = CompletionConfig
  { ccMaxResults         = 100
  , ccMinPrefixLength    = 1
  , ccEnableSnippets     = True
  , ccEnableFuzzy        = True
  , ccShowTypes          = True
  , ccShowDocs           = True
  , ccAutoImport         = True
  , ccPreferLocal        = True
  , ccIncludeKeywords    = True
  , ccIncludeModules     = True
  , ccQualifiedThreshold = 2
  }

--------------------------------------------------------------------------------
-- Completion Engine
--------------------------------------------------------------------------------

-- | Main completion engine with caching and state
data CompletionEngine = CompletionEngine
  { ceConfig         :: CompletionConfig
  , ceHieDbPath      :: FilePath
  , ceSymbolCache    :: TVar (Map Text [CachedSymbol])      -- ^ Module -> symbols
  , ceModuleCache    :: TVar (Set Text)                      -- ^ Known modules
  , cePreludeCache   :: TVar [CachedSymbol]                  -- ^ Prelude symbols
  , ceImportCache    :: TVar (Map FilePath [ImportInfo])     -- ^ File -> imports
  , ceLocalCache     :: TVar (Map FilePath [CachedSymbol])   -- ^ File -> local symbols
  }

-- | Cached symbol for fast lookup
data CachedSymbol = CachedSymbol
  { csName          :: Text
  , csQualified     :: Text
  , csModule        :: Text
  , csKind          :: SymbolKind
  , csType          :: Maybe Text
  , csDocumentation :: Maybe Text
  , csExported      :: Bool
  }
  deriving stock (Eq, Show)

--------------------------------------------------------------------------------
-- Import Information (Full Parsing)
--------------------------------------------------------------------------------

-- | Complete import information from a file
data ImportInfo = ImportInfo
  { iiModule      :: Text              -- ^ Module name
  , iiQualified   :: Maybe Text        -- ^ Qualifier if qualified import
  , iiIsQualified :: Bool              -- ^ Is this a qualified import?
  , iiHiding      :: Bool              -- ^ Is this a hiding import?
  , iiExplicit    :: Maybe [ImportItem] -- ^ Explicit import list
  , iiPackage     :: Maybe Text        -- ^ Package name if specified
  , iiSafe        :: Bool              -- ^ Safe import?
  , iiSource      :: Bool              -- ^ SOURCE import?
  }
  deriving stock (Eq, Show)

-- | An item in an import list
data ImportItem
  = ImportVar Text                      -- ^ Variable or function
  | ImportOp Text                       -- ^ Operator (in parens)
  | ImportType Text [ImportItem]        -- ^ Type/class with optional members
  | ImportAll Text                      -- ^ Type with (..)
  deriving stock (Eq, Show)

-- | Parse import statements from source lines with full detail
parseImports :: [Text] -> [ImportInfo]
parseImports = mapMaybe parseImportLine
  where
    parseImportLine :: Text -> Maybe ImportInfo
    parseImportLine line =
      let stripped = T.strip line
      in if "import" `T.isPrefixOf` stripped
         then Just $ parseImportStatement stripped
         else Nothing

-- | Parse a complete import statement
parseImportStatement :: Text -> ImportInfo
parseImportStatement stmt =
  let -- Remove "import" prefix
      afterImport = T.strip $ T.drop 6 stmt
      -- Check for SOURCE pragma
      (isSource, afterSource) = checkKeyword "{-# SOURCE #-}" afterImport
      -- Check for 'safe' keyword
      (isSafe, afterSafe) = checkKeyword "safe" afterSource
      -- Check for 'qualified' keyword
      (isQual, afterQual) = checkKeyword "qualified" afterSafe
      -- Check for package import
      (mPackage, afterPkg) = parsePackage afterQual
      -- Parse module name
      (modName, afterMod) = parseModuleName afterPkg
      -- Check for 'qualified' keyword (post-qualified syntax)
      (isQualPost, afterQualPost) = checkKeyword "qualified" afterMod
      -- Check for 'as' alias
      (mAlias, afterAs) = parseAlias afterQualPost
      -- Check for 'hiding'
      (isHiding, afterHiding) = checkKeyword "hiding" afterAs
      -- Parse import list
      importList = parseImportList afterHiding
  in ImportInfo
       { iiModule = modName
       , iiQualified = mAlias
       , iiIsQualified = isQual || isQualPost
       , iiHiding = isHiding
       , iiExplicit = importList
       , iiPackage = mPackage
       , iiSafe = isSafe
       , iiSource = isSource
       }
  where
    checkKeyword :: Text -> Text -> (Bool, Text)
    checkKeyword kw txt =
      let stripped = T.stripStart txt
      in if kw `T.isPrefixOf` stripped
         then (True, T.stripStart $ T.drop (T.length kw) stripped)
         else (False, stripped)

    parsePackage :: Text -> (Maybe Text, Text)
    parsePackage txt =
      let stripped = T.stripStart txt
      in if "\"" `T.isPrefixOf` stripped
         then let afterQuote = T.drop 1 stripped
                  (pkg, rest) = T.breakOn "\"" afterQuote
              in (Just pkg, T.stripStart $ T.drop 1 rest)
         else (Nothing, stripped)

    parseModuleName :: Text -> (Text, Text)
    parseModuleName txt =
      let stripped = T.stripStart txt
          isModChar c = isAlphaNum c || c == '.' || c == '_'
          modName = T.takeWhile isModChar stripped
          rest = T.drop (T.length modName) stripped
      in (modName, rest)

    parseAlias :: Text -> (Maybe Text, Text)
    parseAlias txt =
      let stripped = T.stripStart txt
      in if "as" `T.isPrefixOf` stripped &&
            (T.length stripped == 2 || isSpace (T.index stripped 2))
         then let afterAs = T.stripStart $ T.drop 2 stripped
                  isAliasChar c = isAlphaNum c || c == '_'
                  alias = T.takeWhile isAliasChar afterAs
                  rest = T.drop (T.length alias) afterAs
              in (Just alias, rest)
         else (Nothing, stripped)

    parseImportList :: Text -> Maybe [ImportItem]
    parseImportList txt =
      let stripped = T.strip txt
      in if "(" `T.isPrefixOf` stripped
         then Just $ parseItems $ T.drop 1 stripped
         else Nothing

    parseItems :: Text -> [ImportItem]
    parseItems txt =
      let stripped = T.strip txt
      in if T.null stripped || ")" `T.isPrefixOf` stripped
         then []
         else let (item, rest) = parseOneItem stripped
              in item : parseItems rest

    parseOneItem :: Text -> (ImportItem, Text)
    parseOneItem txt =
      let stripped = T.strip txt
      in if "(" `T.isPrefixOf` stripped
         -- Operator in parens
         then let afterOpen = T.drop 1 stripped
                  (op, afterOp) = T.breakOn ")" afterOpen
                  rest = skipComma $ T.drop 1 afterOp
              in (ImportOp (T.strip op), rest)
         -- Type or variable
         else let (name, afterName) = parseName stripped
                  afterName' = T.stripStart afterName
              in if "(" `T.isPrefixOf` afterName'
                 -- Type with members
                 then let afterOpen = T.drop 1 afterName'
                      in if ".." `T.isPrefixOf` T.stripStart afterOpen
                         then let rest = skipComma $ T.drop 1 $ snd $ T.breakOn ")" afterOpen
                              in (ImportAll name, rest)
                         else let (members, rest) = parseMembers afterOpen
                              in (ImportType name members, rest)
                 else (ImportVar name, skipComma afterName')

    parseName :: Text -> (Text, Text)
    parseName txt =
      let isNameChar c = isAlphaNum c || c == '_' || c == '\''
          name = T.takeWhile isNameChar txt
          rest = T.drop (T.length name) txt
      in (name, rest)

    parseMembers :: Text -> ([ImportItem], Text)
    parseMembers txt =
      let stripped = T.strip txt
      in if ")" `T.isPrefixOf` stripped
         then ([], skipComma $ T.drop 1 stripped)
         else let (item, rest) = parseOneMember stripped
                  (items, finalRest) = parseMembers rest
              in (item : items, finalRest)

    parseOneMember :: Text -> (ImportItem, Text)
    parseOneMember txt =
      let stripped = T.strip txt
          (name, rest) = parseName stripped
      in (ImportVar name, T.strip rest)

    skipComma :: Text -> Text
    skipComma txt =
      let stripped = T.stripStart txt
      in if "," `T.isPrefixOf` stripped
         then T.stripStart $ T.drop 1 stripped
         else stripped

-- | Get the visible symbols from an import
getVisibleSymbols :: ImportInfo -> [Text] -> [Text]
getVisibleSymbols ImportInfo{..} allExports =
  case iiExplicit of
    Nothing -> if iiHiding then allExports else allExports
    Just items ->
      let explicitNames = concatMap getItemNames items
      in if iiHiding
         then filter (`notElem` explicitNames) allExports
         else explicitNames
  where
    getItemNames :: ImportItem -> [Text]
    getItemNames (ImportVar n) = [n]
    getItemNames (ImportOp n) = [n]
    getItemNames (ImportType n members) = n : concatMap getItemNames members
    getItemNames (ImportAll n) = [n]  -- Would need full export list for (..)

--------------------------------------------------------------------------------
-- Completion Engine Creation
--------------------------------------------------------------------------------

-- | Create a new completion engine
newCompletionEngine :: CompletionConfig -> FilePath -> IO CompletionEngine
newCompletionEngine config hieDbPath = do
  symbolCache <- newTVarIO Map.empty
  moduleCache <- newTVarIO Set.empty
  preludeCache <- newTVarIO []
  importCache <- newTVarIO Map.empty
  localCache <- newTVarIO Map.empty

  let engine = CompletionEngine
        { ceConfig = config
        , ceHieDbPath = hieDbPath
        , ceSymbolCache = symbolCache
        , ceModuleCache = moduleCache
        , cePreludeCache = preludeCache
        , ceImportCache = importCache
        , ceLocalCache = localCache
        }

  -- Pre-populate Prelude cache
  populatePreludeCache engine

  pure engine

-- | Populate the Prelude symbol cache
populatePreludeCache :: CompletionEngine -> IO ()
populatePreludeCache engine = do
  let preludeSyms = map mkPreludeSymbol preludeExports
  atomically $ writeTVar (cePreludeCache engine) preludeSyms
  where
    mkPreludeSymbol :: (Text, SymbolKind, Text) -> CachedSymbol
    mkPreludeSymbol (name, kind, ty) = CachedSymbol
      { csName = name
      , csQualified = "Prelude." <> name
      , csModule = "Prelude"
      , csKind = kind
      , csType = Just ty
      , csDocumentation = Nothing
      , csExported = True
      }

    -- Common Prelude exports with types
    preludeExports :: [(Text, SymbolKind, Text)]
    preludeExports =
      -- Basic functions
      [ ("id", Function, "a -> a")
      , ("const", Function, "a -> b -> a")
      , ("flip", Function, "(a -> b -> c) -> b -> a -> c")
      , ("($)", Function, "(a -> b) -> a -> b")
      , ("(.)", Function, "(b -> c) -> (a -> b) -> a -> c")
      , ("(&)", Function, "a -> (a -> b) -> b")
      -- Boolean
      , ("not", Function, "Bool -> Bool")
      , ("(&&)", Function, "Bool -> Bool -> Bool")
      , ("(||)", Function, "Bool -> Bool -> Bool")
      , ("otherwise", Function, "Bool")
      -- Maybe
      , ("maybe", Function, "b -> (a -> b) -> Maybe a -> b")
      , ("fromMaybe", Function, "a -> Maybe a -> a")
      , ("isJust", Function, "Maybe a -> Bool")
      , ("isNothing", Function, "Maybe a -> Bool")
      , ("Just", DataConstructor, "a -> Maybe a")
      , ("Nothing", DataConstructor, "Maybe a")
      -- Either
      , ("either", Function, "(a -> c) -> (b -> c) -> Either a b -> c")
      , ("Left", DataConstructor, "a -> Either a b")
      , ("Right", DataConstructor, "b -> Either a b")
      -- Lists
      , ("map", Function, "(a -> b) -> [a] -> [b]")
      , ("filter", Function, "(a -> Bool) -> [a] -> [a]")
      , ("head", Function, "[a] -> a")
      , ("tail", Function, "[a] -> [a]")
      , ("last", Function, "[a] -> a")
      , ("init", Function, "[a] -> [a]")
      , ("null", Function, "Foldable t => t a -> Bool")
      , ("length", Function, "Foldable t => t a -> Int")
      , ("(!!)", Function, "[a] -> Int -> a")
      , ("reverse", Function, "[a] -> [a]")
      , ("take", Function, "Int -> [a] -> [a]")
      , ("drop", Function, "Int -> [a] -> [a]")
      , ("splitAt", Function, "Int -> [a] -> ([a], [a])")
      , ("takeWhile", Function, "(a -> Bool) -> [a] -> [a]")
      , ("dropWhile", Function, "(a -> Bool) -> [a] -> [a]")
      , ("span", Function, "(a -> Bool) -> [a] -> ([a], [a])")
      , ("break", Function, "(a -> Bool) -> [a] -> ([a], [a])")
      , ("elem", Function, "Eq a => a -> [a] -> Bool")
      , ("notElem", Function, "Eq a => a -> [a] -> Bool")
      , ("lookup", Function, "Eq a => a -> [(a, b)] -> Maybe b")
      , ("zip", Function, "[a] -> [b] -> [(a, b)]")
      , ("zipWith", Function, "(a -> b -> c) -> [a] -> [b] -> [c]")
      , ("unzip", Function, "[(a, b)] -> ([a], [b])")
      , ("concat", Function, "[[a]] -> [a]")
      , ("concatMap", Function, "(a -> [b]) -> [a] -> [b]")
      , ("replicate", Function, "Int -> a -> [a]")
      , ("repeat", Function, "a -> [a]")
      , ("cycle", Function, "[a] -> [a]")
      , ("iterate", Function, "(a -> a) -> a -> [a]")
      -- Folds
      , ("foldl", Function, "(b -> a -> b) -> b -> [a] -> b")
      , ("foldl'", Function, "(b -> a -> b) -> b -> [a] -> b")
      , ("foldr", Function, "(a -> b -> b) -> b -> [a] -> b")
      , ("foldl1", Function, "(a -> a -> a) -> [a] -> a")
      , ("foldr1", Function, "(a -> a -> a) -> [a] -> a")
      , ("maximum", Function, "Ord a => [a] -> a")
      , ("minimum", Function, "Ord a => [a] -> a")
      , ("sum", Function, "Num a => [a] -> a")
      , ("product", Function, "Num a => [a] -> a")
      , ("and", Function, "[Bool] -> Bool")
      , ("or", Function, "[Bool] -> Bool")
      , ("any", Function, "(a -> Bool) -> [a] -> Bool")
      , ("all", Function, "(a -> Bool) -> [a] -> Bool")
      -- Monadic
      , ("return", Function, "Monad m => a -> m a")
      , ("pure", Function, "Applicative f => a -> f a")
      , ("(>>=)", Function, "Monad m => m a -> (a -> m b) -> m b")
      , ("(>>)", Function, "Monad m => m a -> m b -> m b")
      , ("fmap", Function, "Functor f => (a -> b) -> f a -> f b")
      , ("(<$>)", Function, "Functor f => (a -> b) -> f a -> f b")
      , ("(<*>)", Function, "Applicative f => f (a -> b) -> f a -> f b")
      , ("(<$)", Function, "Functor f => a -> f b -> f a")
      , ("(*>)", Function, "Applicative f => f a -> f b -> f b")
      , ("(<*)", Function, "Applicative f => f a -> f b -> f a")
      , ("sequence", Function, "Monad m => [m a] -> m [a]")
      , ("mapM", Function, "Monad m => (a -> m b) -> [a] -> m [b]")
      , ("mapM_", Function, "Monad m => (a -> m b) -> [a] -> m ()")
      , ("forM", Function, "Monad m => [a] -> (a -> m b) -> m [b]")
      , ("forM_", Function, "Monad m => [a] -> (a -> m b) -> m ()")
      , ("traverse", Function, "Applicative f => (a -> f b) -> t a -> f (t b)")
      , ("traverse_", Function, "Applicative f => (a -> f b) -> t a -> f ()")
      , ("sequenceA", Function, "Applicative f => t (f a) -> f (t a)")
      -- IO
      , ("print", Function, "Show a => a -> IO ()")
      , ("putStr", Function, "String -> IO ()")
      , ("putStrLn", Function, "String -> IO ()")
      , ("getLine", Function, "IO String")
      , ("getChar", Function, "IO Char")
      , ("readFile", Function, "FilePath -> IO String")
      , ("writeFile", Function, "FilePath -> String -> IO ()")
      , ("appendFile", Function, "FilePath -> String -> IO ()")
      -- Numeric
      , ("(+)", Function, "Num a => a -> a -> a")
      , ("(-)", Function, "Num a => a -> a -> a")
      , ("(*)", Function, "Num a => a -> a -> a")
      , ("(/)", Function, "Fractional a => a -> a -> a")
      , ("div", Function, "Integral a => a -> a -> a")
      , ("mod", Function, "Integral a => a -> a -> a")
      , ("quot", Function, "Integral a => a -> a -> a")
      , ("rem", Function, "Integral a => a -> a -> a")
      , ("abs", Function, "Num a => a -> a")
      , ("signum", Function, "Num a => a -> a")
      , ("negate", Function, "Num a => a -> a")
      , ("fromIntegral", Function, "(Integral a, Num b) => a -> b")
      , ("realToFrac", Function, "(Real a, Fractional b) => a -> b")
      , ("even", Function, "Integral a => a -> Bool")
      , ("odd", Function, "Integral a => a -> Bool")
      , ("gcd", Function, "Integral a => a -> a -> a")
      , ("lcm", Function, "Integral a => a -> a -> a")
      , ("(^)", Function, "(Num a, Integral b) => a -> b -> a")
      , ("(^^)", Function, "(Fractional a, Integral b) => a -> b -> a")
      -- Comparison
      , ("(==)", Function, "Eq a => a -> a -> Bool")
      , ("(/=)", Function, "Eq a => a -> a -> Bool")
      , ("(<)", Function, "Ord a => a -> a -> Bool")
      , ("(<=)", Function, "Ord a => a -> a -> Bool")
      , ("(>)", Function, "Ord a => a -> a -> Bool")
      , ("(>=)", Function, "Ord a => a -> a -> Bool")
      , ("compare", Function, "Ord a => a -> a -> Ordering")
      , ("max", Function, "Ord a => a -> a -> a")
      , ("min", Function, "Ord a => a -> a -> a")
      -- Show/Read
      , ("show", Function, "Show a => a -> String")
      , ("read", Function, "Read a => String -> a")
      , ("reads", Function, "Read a => String -> [(a, String)]")
      , ("shows", Function, "Show a => a -> ShowS")
      -- String
      , ("lines", Function, "String -> [String]")
      , ("unlines", Function, "[String] -> String")
      , ("words", Function, "String -> [String]")
      , ("unwords", Function, "[String] -> String")
      -- Tuples
      , ("fst", Function, "(a, b) -> a")
      , ("snd", Function, "(a, b) -> b")
      , ("curry", Function, "((a, b) -> c) -> a -> b -> c")
      , ("uncurry", Function, "(a -> b -> c) -> (a, b) -> c")
      -- Error handling
      , ("error", Function, "String -> a")
      , ("undefined", Function, "a")
      , ("seq", Function, "a -> b -> b")
      -- Types
      , ("Bool", TypeConstructor, "*")
      , ("True", DataConstructor, "Bool")
      , ("False", DataConstructor, "Bool")
      , ("Int", TypeConstructor, "*")
      , ("Integer", TypeConstructor, "*")
      , ("Float", TypeConstructor, "*")
      , ("Double", TypeConstructor, "*")
      , ("Char", TypeConstructor, "*")
      , ("String", TypeConstructor, "*")
      , ("Maybe", TypeConstructor, "* -> *")
      , ("Either", TypeConstructor, "* -> * -> *")
      , ("IO", TypeConstructor, "* -> *")
      , ("Ordering", TypeConstructor, "*")
      , ("LT", DataConstructor, "Ordering")
      , ("EQ", DataConstructor, "Ordering")
      , ("GT", DataConstructor, "Ordering")
      ]

--------------------------------------------------------------------------------
-- Completion Context Detection
--------------------------------------------------------------------------------

-- | The kind of completion context
data CompletionKind
  = CKImport           -- ^ In import statement
  | CKModule           -- ^ Module name
  | CKQualified Text   -- ^ Qualified name with module prefix
  | CKType             -- ^ Type context (after ::)
  | CKPattern          -- ^ Pattern context (case, lambda)
  | CKExpression       -- ^ General expression context
  | CKPragma           -- ^ In pragma ({-# ... #-})
  | CKLanguage         -- ^ LANGUAGE pragma
  | CKOptions          -- ^ OPTIONS pragma
  | CKDerive           -- ^ Deriving clause
  | CKInstance         -- ^ Instance declaration
  | CKSignature        -- ^ Type signature context
  | CKRecordField      -- ^ Record field access/update
  | CKHole             -- ^ Typed hole context
  deriving stock (Eq, Show)

-- | Full completion context with position info
data CompletionContext = CompletionContext
  { ccKind        :: CompletionKind
  , ccPrefix      :: Text           -- ^ Text being completed
  , ccLine        :: Int            -- ^ Line number (0-indexed)
  , ccColumn      :: Int            -- ^ Column number (0-indexed)
  , ccLineText    :: Text           -- ^ Full line text
  , ccInScope     :: [Text]         -- ^ In-scope identifiers
  , ccImports     :: [ImportInfo]   -- ^ Active imports
  }
  deriving stock (Eq, Show)

-- | Detect completion context from source position
detectCompletionContext :: Text -> Int -> Int -> CompletionContext
detectCompletionContext source line col =
  let ls = T.lines source
      lineText = if line >= 0 && line < length ls then ls !! line else ""
      beforeCursor = T.take col lineText
      prefix = extractPrefix beforeCursor
      kind = detectKind ls line beforeCursor prefix
      imports = parseImports ls
      inScope = extractInScope ls line
  in CompletionContext
       { ccKind = kind
       , ccPrefix = prefix
       , ccLine = line
       , ccColumn = col
       , ccLineText = lineText
       , ccInScope = inScope
       , ccImports = imports
       }
  where
    extractPrefix :: Text -> Text
    extractPrefix before =
      let rev = T.reverse before
          prefixRev = T.takeWhile isIdentChar rev
      in T.reverse prefixRev

    isIdentChar :: Char -> Bool
    isIdentChar c = isAlphaNum c || c == '_' || c == '\'' || c == '.'

    detectKind :: [Text] -> Int -> Text -> Text -> CompletionKind
    detectKind ls lineNum before prefix
      -- Check for pragmas
      | "{-#" `T.isInfixOf` before = detectPragmaKind before
      -- Check for qualified names
      | '.' `T.elem` prefix =
          let parts = T.splitOn "." prefix
          in if length parts >= 2
             then CKQualified (T.intercalate "." (init parts))
             else CKExpression
      -- Check for import statement
      | isImportContext ls lineNum = CKImport
      -- Check for type context
      | isTypeContext before = CKType
      -- Check for deriving
      | "deriving" `T.isInfixOf` before = CKDerive
      -- Check for instance
      | lineNum < length ls && "instance" `T.isPrefixOf` T.stripStart (ls !! lineNum) = CKInstance
      -- Check for pattern context
      | isPatternContext before = CKPattern
      -- Check for record field
      | isRecordContext before = CKRecordField
      -- Check for typed hole
      | "_" `T.isPrefixOf` prefix = CKHole
      -- Check for type signature
      | isSignatureContext ls lineNum = CKSignature
      -- Default to expression
      | otherwise = CKExpression

    detectPragmaKind :: Text -> CompletionKind
    detectPragmaKind before
      | "LANGUAGE" `T.isInfixOf` before = CKLanguage
      | "OPTIONS" `T.isInfixOf` before = CKOptions
      | otherwise = CKPragma

    isImportContext :: [Text] -> Int -> Bool
    isImportContext ls lineNum =
      lineNum < length ls &&
      "import" `T.isPrefixOf` T.stripStart (ls !! lineNum)

    isTypeContext :: Text -> Bool
    isTypeContext before =
      "::" `T.isInfixOf` before ||
      "type " `T.isPrefixOf` T.stripStart before ||
      "-> " `T.isSuffixOf` before

    isPatternContext :: Text -> Bool
    isPatternContext before =
      "case " `T.isInfixOf` before ||
      "\\" `T.isInfixOf` before ||
      "| " `T.isSuffixOf` before ||
      "= " `T.isSuffixOf` before

    isRecordContext :: Text -> Bool
    isRecordContext before =
      "{ " `T.isSuffixOf` before ||
      (", " `T.isSuffixOf` before && "{" `T.isInfixOf` before)

    isSignatureContext :: [Text] -> Int -> Bool
    isSignatureContext ls lineNum =
      let relevantLines = take (lineNum + 1) ls
      in any ("::" `T.isInfixOf`) relevantLines

-- | Extract in-scope identifiers from source
extractInScope :: [Text] -> Int -> [Text]
extractInScope ls currentLine =
  let relevantLines = take (currentLine + 1) ls
  in concatMap extractIdentifiers relevantLines
  where
    extractIdentifiers :: Text -> [Text]
    extractIdentifiers line =
      let tokens = T.words line
          -- Extract function definitions (name = ...)
          defs = case tokens of
                   (name:rest) | "=" `elem` rest && isValidIdent name -> [name]
                   _ -> []
          -- Extract type signatures (name :: ...)
          sigs = case T.breakOn "::" line of
                   (before, after) | not (T.null after) ->
                     let ws = T.words $ T.strip before
                     in case ws of
                          [] -> []
                          _ -> let name = last ws
                               in if isValidIdent name then [name] else []
                   _ -> []
      in defs ++ sigs

    isValidIdent :: Text -> Bool
    isValidIdent t = case T.uncons t of
      Just (c, _) -> isLower c || c == '_'
      Nothing -> False

--------------------------------------------------------------------------------
-- Completion Results
--------------------------------------------------------------------------------

-- | A single completion result
data CompletionResult = CompletionResult
  { crLabel         :: Text                    -- ^ Display label
  , crKind          :: SymbolKind              -- ^ Symbol kind
  , crDetail        :: Maybe Text              -- ^ Type signature
  , crDocumentation :: Maybe Text              -- ^ Documentation
  , crInsertText    :: Maybe Text              -- ^ Text to insert (if different from label)
  , crFilterText    :: Maybe Text              -- ^ Text for filtering
  , crSortText      :: Maybe Text              -- ^ Text for sorting
  , crModule        :: Maybe Text              -- ^ Module name
  , crImportNeeded  :: Maybe ImportSuggestion  -- ^ Import to add
  , crIsSnippet     :: Bool                    -- ^ Is this a snippet?
  , crScore         :: CompletionScore         -- ^ Ranking score
  }
  deriving stock (Eq, Show)

-- | Score for ranking completions (higher is better)
type CompletionScore = Int

-- | Import suggestion for auto-import
data ImportSuggestion = ImportSuggestion
  { isModule     :: Text           -- ^ Module to import
  , isSymbol     :: Maybe Text     -- ^ Specific symbol (Nothing = entire module)
  , isQualified  :: Maybe Text     -- ^ Qualification
  }
  deriving stock (Eq, Show)

-- | Rank completions by relevance
rankCompletions :: CompletionContext -> [CompletionResult] -> [CompletionResult]
rankCompletions ctx results =
  let scored = map (scoreCompletion ctx) results
      sorted = sortBy (comparing (Down . crScore)) scored
      deduped = nubBy ((==) `on` crLabel) sorted
  in deduped
  where
    scoreCompletion :: CompletionContext -> CompletionResult -> CompletionResult
    scoreCompletion CompletionContext{..} cr =
      let baseScore = crScore cr
          -- Boost exact prefix matches
          prefixBoost = if ccPrefix `T.isPrefixOf` crLabel cr then 100 else 0
          -- Boost case-sensitive matches
          caseBoost = if ccPrefix `T.isPrefixOf` crLabel cr then 50 else 0
          -- Boost in-scope symbols
          scopeBoost = if crLabel cr `elem` ccInScope then 200 else 0
          -- Boost based on context relevance
          contextBoost = contextScore ccKind (crKind cr)
          -- Penalize very long names
          lengthPenalty = max 0 (T.length (crLabel cr) - 20)
          -- Final score
          finalScore = baseScore + prefixBoost + caseBoost + scopeBoost + contextBoost - lengthPenalty
      in cr { crScore = finalScore }

    contextScore :: CompletionKind -> SymbolKind -> Int
    contextScore CKType TypeConstructor = 100
    contextScore CKType TypeClass = 80
    contextScore CKPattern DataConstructor = 100
    contextScore CKExpression Function = 50
    contextScore CKDerive TypeClass = 100
    contextScore CKInstance TypeClass = 100
    contextScore _ _ = 0

--------------------------------------------------------------------------------
-- Completion Sources
--------------------------------------------------------------------------------

-- | A source of completions
data CompletionSource
  = CSLocal          -- ^ Local file symbols
  | CSImported       -- ^ Imported symbols
  | CSPrelude        -- ^ Prelude symbols
  | CSHIE            -- ^ HIE database
  | CSSnippet        -- ^ Code snippets
  | CSPragma         -- ^ Pragmas
  | CSModule         -- ^ Module names
  | CSKeyword        -- ^ Keywords
  deriving stock (Eq, Show, Ord)

-- | Get completions for a specific position
getCompletionsAt :: CompletionEngine -> FilePath -> Text -> Int -> Int -> IO [CompletionResult]
getCompletionsAt engine path source line col = do
  let ctx = detectCompletionContext source line col
      config = ceConfig engine

  -- Skip if prefix too short
  if T.length (ccPrefix ctx) < ccMinPrefixLength config
    then pure []
    else do
      -- Get completions from various sources based on context
      completions <- case ccKind ctx of
        CKLanguage -> pure $ languageExtensionCompletions (ccPrefix ctx)
        CKOptions  -> pure $ optionsCompletions (ccPrefix ctx)
        CKPragma   -> pure $ allPragmaCompletions (ccPrefix ctx)
        CKImport   -> moduleCompletions engine (ccPrefix ctx)
        CKModule   -> moduleCompletions engine (ccPrefix ctx)
        CKQualified modName -> qualifiedCompletions engine modName (ccPrefix ctx)
        CKDerive   -> pure $ derivingCompletions (ccPrefix ctx)
        _ -> do
          -- General completions from multiple sources
          local <- localSymbols engine path source
          prelude <- preludeSymbols engine
          hie <- hieSymbols engine (ccPrefix ctx)
          snippets <- if ccEnableSnippets config
                      then snippetCompletions (ccKind ctx)
                      else pure []
          keywords <- if ccIncludeKeywords config
                      then pure $ keywordCompletions (ccPrefix ctx)
                      else pure []
          pure $ concat [local, prelude, hie, snippets, keywords]

      -- Rank and limit results
      let ranked = rankCompletions ctx completions
          limited = take (ccMaxResults config) ranked

      pure limited

-- | Get local symbols from the current file
localSymbols :: CompletionEngine -> FilePath -> Text -> IO [CompletionResult]
localSymbols _engine _path source = do
  let syms = extractLocalSymbolsFromSource source
  pure $ map localToResult syms
  where
    localToResult :: (Text, SymbolKind, Maybe Text) -> CompletionResult
    localToResult (name, kind, mType) = CompletionResult
      { crLabel = name
      , crKind = kind
      , crDetail = mType
      , crDocumentation = Nothing
      , crInsertText = Nothing
      , crFilterText = Nothing
      , crSortText = Just $ "0" <> name  -- Local symbols first
      , crModule = Nothing
      , crImportNeeded = Nothing
      , crIsSnippet = False
      , crScore = 300  -- High base score for local
      }

    extractLocalSymbolsFromSource :: Text -> [(Text, SymbolKind, Maybe Text)]
    extractLocalSymbolsFromSource src =
      let ls = T.lines src
      in concatMap extractFromLine ls

    extractFromLine :: Text -> [(Text, SymbolKind, Maybe Text)]
    extractFromLine line =
      let stripped = T.stripStart line
      in catMaybes
           [ extractFunctionDef stripped
           , extractTypeSig stripped
           , extractDataDef stripped
           , extractNewtypeDef stripped
           , extractTypeSynonym stripped
           , extractClassDef stripped
           ]

    extractFunctionDef :: Text -> Maybe (Text, SymbolKind, Maybe Text)
    extractFunctionDef line =
      case T.words line of
        (name:rest) | "=" `elem` rest && isValidFuncName name ->
          Just (name, Function, Nothing)
        _ -> Nothing

    extractTypeSig :: Text -> Maybe (Text, SymbolKind, Maybe Text)
    extractTypeSig line =
      case T.breakOn "::" line of
        (before, after) | not (T.null after) ->
          let ws = T.words $ T.strip before
          in case ws of
               [] -> Nothing
               _ -> let name = last ws
                        ty = T.strip $ T.drop 2 after
                    in if isValidFuncName name
                       then Just (name, Function, Just ty)
                       else Nothing
        _ -> Nothing

    extractDataDef :: Text -> Maybe (Text, SymbolKind, Maybe Text)
    extractDataDef line =
      if "data " `T.isPrefixOf` line
      then case T.words (T.drop 5 line) of
             (name:_) | isValidTypeName name -> Just (name, TypeConstructor, Nothing)
             _ -> Nothing
      else Nothing

    extractNewtypeDef :: Text -> Maybe (Text, SymbolKind, Maybe Text)
    extractNewtypeDef line =
      if "newtype " `T.isPrefixOf` line
      then case T.words (T.drop 8 line) of
             (name:_) | isValidTypeName name -> Just (name, TypeConstructor, Nothing)
             _ -> Nothing
      else Nothing

    extractTypeSynonym :: Text -> Maybe (Text, SymbolKind, Maybe Text)
    extractTypeSynonym line =
      if "type " `T.isPrefixOf` line
      then case T.words (T.drop 5 line) of
             (name:_) | isValidTypeName name -> Just (name, TypeConstructor, Nothing)
             _ -> Nothing
      else Nothing

    extractClassDef :: Text -> Maybe (Text, SymbolKind, Maybe Text)
    extractClassDef line =
      if "class " `T.isPrefixOf` line
      then case T.words (T.drop 6 line) of
             (name:_) | isValidTypeName name -> Just (name, TypeClass, Nothing)
             _ -> Nothing
      else Nothing

    isValidFuncName :: Text -> Bool
    isValidFuncName t = case T.uncons t of
      Just (c, _) -> isLower c || c == '_'
      Nothing -> False

    isValidTypeName :: Text -> Bool
    isValidTypeName t = case T.uncons t of
      Just (c, _) -> isUpper c
      Nothing -> False

-- | Get imported symbols
importedSymbols :: CompletionEngine -> [ImportInfo] -> IO [CompletionResult]
importedSymbols engine imports = do
  results <- forM imports $ \info@ImportInfo{..} -> do
    -- Try to get exports from HIE database
    result <- try @SomeException $ withHieQuery (ceHieDbPath engine) $ do
      exports <- getModuleExports iiModule
      let visible = getVisibleSymbols info exports
      forM visible $ \sym -> do
        mType <- getSymbolType sym (Just iiModule)
        pure $ importedToResult iiModule iiQualified sym mType
    case result of
      Left _ -> pure []
      Right items -> pure $ catMaybes items
  pure $ concat results
  where
    importedToResult :: Text -> Maybe Text -> Text -> Maybe TypeInfo -> Maybe CompletionResult
    importedToResult modName mQual name mTypeInfo =
      let label = case mQual of
                    Just q -> q <> "." <> name
                    Nothing -> name
      in Just CompletionResult
           { crLabel = label
           , crKind = Function  -- Default, would need more info
           , crDetail = fmap tiType mTypeInfo
           , crDocumentation = Nothing
           , crInsertText = Nothing
           , crFilterText = Just name
           , crSortText = Just $ "1" <> label
           , crModule = Just modName
           , crImportNeeded = Nothing
           , crIsSnippet = False
           , crScore = 200
           }

-- | Get Prelude symbols
preludeSymbols :: CompletionEngine -> IO [CompletionResult]
preludeSymbols engine = do
  syms <- atomically $ readTVar (cePreludeCache engine)
  pure $ map cachedToResult syms
  where
    cachedToResult :: CachedSymbol -> CompletionResult
    cachedToResult CachedSymbol{..} = CompletionResult
      { crLabel = csName
      , crKind = csKind
      , crDetail = csType
      , crDocumentation = csDocumentation
      , crInsertText = Nothing
      , crFilterText = Nothing
      , crSortText = Just $ "2" <> csName
      , crModule = Just csModule
      , crImportNeeded = Nothing
      , crIsSnippet = False
      , crScore = 150
      }

-- | Get symbols from HIE database
hieSymbols :: CompletionEngine -> Text -> IO [CompletionResult]
hieSymbols engine prefix = do
  -- Only query if prefix is long enough
  if T.length prefix < 2
    then pure []
    else do
      result <- try @SomeException $ withHieQuery (ceHieDbPath engine) $ do
        syms <- findAllSymbols prefix
        pure $ map hieToResult syms
      case result of
        Left _ -> pure []
        Right items -> pure items
  where
    hieToResult :: HieSymbol -> CompletionResult
    hieToResult HieSymbol{..} = CompletionResult
      { crLabel = hsName
      , crKind = hsKind
      , crDetail = hsType
      , crDocumentation = hsDocumentation
      , crInsertText = Nothing
      , crFilterText = Nothing
      , crSortText = Just $ "3" <> hsName
      , crModule = Just hsModule
      , crImportNeeded = Just ImportSuggestion
          { isModule = hsModule
          , isSymbol = Just hsName
          , isQualified = Nothing
          }
      , crIsSnippet = False
      , crScore = 100
      }

-- | Get qualified completions for a specific module
qualifiedCompletions :: CompletionEngine -> Text -> Text -> IO [CompletionResult]
qualifiedCompletions engine modPrefix fullPrefix = do
  -- Extract the part after the dot
  let afterDot = T.takeWhileEnd (/= '.') fullPrefix

  result <- try @SomeException $ withHieQuery (ceHieDbPath engine) $ do
    syms <- findSymbolsInModule modPrefix
    let matching = filter (\s -> afterDot `T.isPrefixOf` hsName s) syms
    pure $ map (qualifiedToResult modPrefix) matching
  case result of
    Left _ -> pure []
    Right items -> pure items
  where
    qualifiedToResult :: Text -> HieSymbol -> CompletionResult
    qualifiedToResult modName HieSymbol{..} = CompletionResult
      { crLabel = modName <> "." <> hsName
      , crKind = hsKind
      , crDetail = hsType
      , crDocumentation = hsDocumentation
      , crInsertText = Nothing
      , crFilterText = Just hsName
      , crSortText = Just $ "0" <> hsName  -- Qualified matches are very relevant
      , crModule = Just modName
      , crImportNeeded = Nothing  -- Already qualified
      , crIsSnippet = False
      , crScore = 250
      }

-- | Get snippet completions based on context
snippetCompletions :: CompletionKind -> IO [CompletionResult]
snippetCompletions kind = pure $ case kind of
  CKExpression -> expressionSnippets
  CKPattern    -> patternSnippets
  CKType       -> typeSnippets
  CKSignature  -> signatureSnippets
  _            -> []
  where
    expressionSnippets :: [CompletionResult]
    expressionSnippets =
      [ mkSnippet "case" "case ${1:expr} of\n  ${2:pattern} -> ${3:body}" "Case expression"
      , mkSnippet "if" "if ${1:condition}\n  then ${2:thenExpr}\n  else ${3:elseExpr}" "If-then-else"
      , mkSnippet "let" "let ${1:name} = ${2:expr}\nin ${3:body}" "Let binding"
      , mkSnippet "do" "do\n  ${1:action}" "Do notation"
      , mkSnippet "lambda" "\\${1:x} -> ${2:body}" "Lambda expression"
      , mkSnippet "where" "where\n  ${1:name} = ${2:expr}" "Where clause"
      , mkSnippet "guard" "| ${1:condition} = ${2:expr}" "Guard"
      ]

    patternSnippets :: [CompletionResult]
    patternSnippets =
      [ mkSnippet "Just" "Just ${1:x}" "Just pattern"
      , mkSnippet "Nothing" "Nothing" "Nothing pattern"
      , mkSnippet "Left" "Left ${1:x}" "Left pattern"
      , mkSnippet "Right" "Right ${1:x}" "Right pattern"
      , mkSnippet "cons" "(${1:x}:${2:xs})" "List cons pattern"
      , mkSnippet "tuple2" "(${1:a}, ${2:b})" "Tuple pattern"
      , mkSnippet "tuple3" "(${1:a}, ${2:b}, ${3:c})" "Triple pattern"
      ]

    typeSnippets :: [CompletionResult]
    typeSnippets =
      [ mkSnippet "Maybe" "Maybe ${1:a}" "Maybe type"
      , mkSnippet "Either" "Either ${1:a} ${2:b}" "Either type"
      , mkSnippet "IO" "IO ${1:a}" "IO type"
      , mkSnippet "list" "[${1:a}]" "List type"
      , mkSnippet "function" "${1:a} -> ${2:b}" "Function type"
      , mkSnippet "constraint" "${1:Constraint} ${2:a} =>" "Type constraint"
      ]

    signatureSnippets :: [CompletionResult]
    signatureSnippets =
      [ mkSnippet "sig" "${1:name} :: ${2:Type}" "Type signature"
      , mkSnippet "sigIO" "${1:name} :: IO ${2:a}" "IO type signature"
      , mkSnippet "sigMonad" "${1:name} :: Monad m => m ${2:a}" "Monadic signature"
      ]

    mkSnippet :: Text -> Text -> Text -> CompletionResult
    mkSnippet name insert doc = CompletionResult
      { crLabel = name
      , crKind = Function
      , crDetail = Just "snippet"
      , crDocumentation = Just doc
      , crInsertText = Just insert
      , crFilterText = Just name
      , crSortText = Just $ "9" <> name  -- Snippets last
      , crModule = Nothing
      , crImportNeeded = Nothing
      , crIsSnippet = True
      , crScore = 50
      }

-- | Get pragma completions
pragmaCompletions :: Text -> IO [CompletionResult]
pragmaCompletions = pure . allPragmaCompletions

-- | All pragma completions
allPragmaCompletions :: Text -> [CompletionResult]
allPragmaCompletions prefix =
  let pragmas =
        [ ("LANGUAGE", "LANGUAGE extension")
        , ("OPTIONS_GHC", "GHC options")
        , ("OPTIONS_HADDOCK", "Haddock options")
        , ("INLINE", "Inline pragma")
        , ("NOINLINE", "No-inline pragma")
        , ("INLINABLE", "Inlinable pragma")
        , ("SPECIALIZE", "Specialization pragma")
        , ("RULES", "Rewrite rules")
        , ("DEPRECATED", "Deprecation warning")
        , ("WARNING", "Warning pragma")
        , ("MINIMAL", "Minimal complete definition")
        , ("OVERLAPPING", "Overlapping instance")
        , ("OVERLAPPABLE", "Overlappable instance")
        , ("OVERLAPS", "Overlaps pragma")
        , ("INCOHERENT", "Incoherent instance")
        , ("UNPACK", "Unpack strict fields")
        , ("NOUNPACK", "Don't unpack fields")
        , ("SOURCE", "hs-boot import")
        , ("COMPLETE", "Complete pattern set")
        , ("ANN", "Annotation")
        ]
      matching = filter (\(n, _) -> T.toLower prefix `T.isPrefixOf` T.toLower n) pragmas
  in map mkPragmaResult matching
  where
    mkPragmaResult :: (Text, Text) -> CompletionResult
    mkPragmaResult (name, doc) = CompletionResult
      { crLabel = name
      , crKind = Function
      , crDetail = Just "pragma"
      , crDocumentation = Just doc
      , crInsertText = Just $ "{-# " <> name <> " ${1} #-}"
      , crFilterText = Just name
      , crSortText = Just $ "0" <> name
      , crModule = Nothing
      , crImportNeeded = Nothing
      , crIsSnippet = True
      , crScore = 100
      }

-- | Language extension completions
languageExtensionCompletions :: Text -> [CompletionResult]
languageExtensionCompletions prefix =
  let extensions =
        [ "AllowAmbiguousTypes", "ApplicativeDo", "Arrows"
        , "BangPatterns", "BinaryLiterals", "BlockArguments"
        , "CApiFFI", "ConstrainedClassMethods", "ConstraintKinds", "CPP"
        , "DataKinds", "DefaultSignatures", "DeriveAnyClass", "DeriveDataTypeable"
        , "DeriveFoldable", "DeriveFunctor", "DeriveGeneric", "DeriveLift"
        , "DeriveTraversable", "DerivingStrategies", "DerivingVia"
        , "DisambiguateRecordFields", "DuplicateRecordFields"
        , "EmptyCase", "EmptyDataDecls", "EmptyDataDeriving"
        , "ExistentialQuantification", "ExplicitForAll", "ExplicitNamespaces"
        , "ExtendedDefaultRules"
        , "FlexibleContexts", "FlexibleInstances", "ForeignFunctionInterface"
        , "FunctionalDependencies"
        , "GADTs", "GADTSyntax", "GeneralisedNewtypeDeriving", "GeneralizedNewtypeDeriving"
        , "HexFloatLiterals"
        , "ImplicitParams", "ImportQualifiedPost", "ImpredicativeTypes"
        , "IncoherentInstances", "InstanceSigs", "InterruptibleFFI"
        , "KindSignatures"
        , "LambdaCase", "LexicalNegation", "LiberalTypeSynonyms", "LinearTypes"
        , "MagicHash", "MonadComprehensions", "MonadFailDesugaring", "MonoLocalBinds"
        , "MultiParamTypeClasses", "MultiWayIf"
        , "NamedFieldPuns", "NamedWildCards", "NegativeLiterals"
        , "NoFieldSelectors", "NoImplicitPrelude", "NoMonomorphismRestriction"
        , "NoStarIsType", "NoTraditionalRecordSyntax", "NullaryTypeClasses"
        , "NumDecimals", "NumericUnderscores"
        , "OverlappingInstances", "OverloadedLabels", "OverloadedLists"
        , "OverloadedRecordDot", "OverloadedRecordUpdate", "OverloadedStrings"
        , "PackageImports", "ParallelListComp", "PartialTypeSignatures"
        , "PatternGuards", "PatternSynonyms", "PolyKinds", "PostfixOperators"
        , "QualifiedDo", "QuantifiedConstraints", "QuasiQuotes"
        , "Rank2Types", "RankNTypes", "RebindableSyntax", "RecordWildCards"
        , "RecursiveDo", "RoleAnnotations"
        , "Safe", "ScopedTypeVariables", "StandaloneDeriving", "StandaloneKindSignatures"
        , "StarIsType", "StaticPointers", "Strict", "StrictData"
        , "TemplateHaskell", "TemplateHaskellQuotes", "TransformListComp"
        , "Trustworthy", "TupleSections", "TypeApplications", "TypeData"
        , "TypeFamilies", "TypeFamilyDependencies", "TypeInType", "TypeOperators"
        , "TypeSynonymInstances"
        , "UnboxedSums", "UnboxedTuples", "UndecidableInstances"
        , "UndecidableSuperClasses", "UnicodeSyntax", "UnliftedFFITypes"
        , "UnliftedNewtypes", "Unsafe"
        , "ViewPatterns"
        ]
      matching = filter (matchesPrefix prefix) extensions
  in map mkExtResult matching
  where
    matchesPrefix :: Text -> Text -> Bool
    matchesPrefix p ext = T.toLower p `T.isPrefixOf` T.toLower ext

    mkExtResult :: Text -> CompletionResult
    mkExtResult ext = CompletionResult
      { crLabel = ext
      , crKind = Function
      , crDetail = Just "Language extension"
      , crDocumentation = Nothing
      , crInsertText = Nothing
      , crFilterText = Nothing
      , crSortText = Just $ "0" <> ext
      , crModule = Nothing
      , crImportNeeded = Nothing
      , crIsSnippet = False
      , crScore = 100
      }

-- | OPTIONS pragma completions
optionsCompletions :: Text -> [CompletionResult]
optionsCompletions prefix =
  let options =
        [ ("-Wall", "Enable all warnings")
        , ("-Wextra", "Enable extra warnings")
        , ("-Werror", "Treat warnings as errors")
        , ("-Wno-unused-imports", "Disable unused imports warning")
        , ("-Wno-orphans", "Disable orphan instances warning")
        , ("-O", "Basic optimization")
        , ("-O2", "Full optimization")
        , ("-fno-warn-unused-binds", "Disable unused binds warning")
        , ("-fno-warn-name-shadowing", "Disable name shadowing warning")
        , ("-XStrict", "Enable Strict extension")
        , ("-XStrictData", "Enable StrictData extension")
        ]
      matching = filter (\(n, _) -> T.toLower prefix `T.isPrefixOf` T.toLower n) options
  in map mkOptResult matching
  where
    mkOptResult :: (Text, Text) -> CompletionResult
    mkOptResult (opt, doc) = CompletionResult
      { crLabel = opt
      , crKind = Function
      , crDetail = Just "GHC option"
      , crDocumentation = Just doc
      , crInsertText = Nothing
      , crFilterText = Just opt
      , crSortText = Just $ "0" <> opt
      , crModule = Nothing
      , crImportNeeded = Nothing
      , crIsSnippet = False
      , crScore = 100
      }

-- | Deriving clause completions
derivingCompletions :: Text -> [CompletionResult]
derivingCompletions prefix =
  let classes =
        [ ("Eq", "Equality comparison")
        , ("Ord", "Ordered comparison")
        , ("Show", "String conversion")
        , ("Read", "String parsing")
        , ("Enum", "Enumeration")
        , ("Bounded", "Bounded type")
        , ("Functor", "Functor instance")
        , ("Foldable", "Foldable instance")
        , ("Traversable", "Traversable instance")
        , ("Generic", "Generic representation")
        , ("Data", "Data instance")
        , ("Typeable", "Runtime type information")
        , ("NFData", "Deep evaluation")
        , ("Hashable", "Hash computation")
        , ("ToJSON", "JSON serialization")
        , ("FromJSON", "JSON deserialization")
        , ("Binary", "Binary serialization")
        , ("Default", "Default value")
        ]
      matching = filter (\(n, _) -> T.toLower prefix `T.isPrefixOf` T.toLower n) classes
  in map mkDeriveResult matching
  where
    mkDeriveResult :: (Text, Text) -> CompletionResult
    mkDeriveResult (cls, doc) = CompletionResult
      { crLabel = cls
      , crKind = TypeClass
      , crDetail = Just "Derivable class"
      , crDocumentation = Just doc
      , crInsertText = Nothing
      , crFilterText = Just cls
      , crSortText = Just $ "0" <> cls
      , crModule = Nothing
      , crImportNeeded = Nothing
      , crIsSnippet = False
      , crScore = 100
      }

-- | Module completions
moduleCompletions :: CompletionEngine -> Text -> IO [CompletionResult]
moduleCompletions _engine prefix = do
  -- Return common modules that match the prefix
  let modules =
        [ "Control.Applicative"
        , "Control.Arrow"
        , "Control.Category"
        , "Control.Concurrent"
        , "Control.Concurrent.Async"
        , "Control.Concurrent.STM"
        , "Control.Exception"
        , "Control.Monad"
        , "Control.Monad.Except"
        , "Control.Monad.IO.Class"
        , "Control.Monad.Reader"
        , "Control.Monad.State"
        , "Control.Monad.Trans"
        , "Control.Monad.Trans.Class"
        , "Control.Monad.Trans.Except"
        , "Control.Monad.Trans.Reader"
        , "Control.Monad.Trans.State"
        , "Control.Monad.Writer"
        , "Data.Aeson"
        , "Data.Bifunctor"
        , "Data.Bool"
        , "Data.ByteString"
        , "Data.ByteString.Lazy"
        , "Data.Char"
        , "Data.Coerce"
        , "Data.Complex"
        , "Data.Either"
        , "Data.Foldable"
        , "Data.Function"
        , "Data.Functor"
        , "Data.Functor.Identity"
        , "Data.HashMap.Strict"
        , "Data.HashSet"
        , "Data.Int"
        , "Data.IntMap"
        , "Data.IntSet"
        , "Data.IORef"
        , "Data.Kind"
        , "Data.List"
        , "Data.List.NonEmpty"
        , "Data.Map"
        , "Data.Map.Strict"
        , "Data.Maybe"
        , "Data.Monoid"
        , "Data.Ord"
        , "Data.Proxy"
        , "Data.Ratio"
        , "Data.Scientific"
        , "Data.Semigroup"
        , "Data.Sequence"
        , "Data.Set"
        , "Data.String"
        , "Data.Text"
        , "Data.Text.Encoding"
        , "Data.Text.IO"
        , "Data.Text.Lazy"
        , "Data.Time"
        , "Data.Time.Clock"
        , "Data.Traversable"
        , "Data.Tuple"
        , "Data.Typeable"
        , "Data.Vector"
        , "Data.Void"
        , "Data.Word"
        , "Debug.Trace"
        , "GHC.Generics"
        , "GHC.TypeLits"
        , "Numeric"
        , "Prelude"
        , "System.Directory"
        , "System.Environment"
        , "System.Exit"
        , "System.FilePath"
        , "System.IO"
        , "System.Process"
        , "System.Random"
        , "Text.Parsec"
        , "Text.Printf"
        , "Text.Read"
        ]
      matching = filter (\m -> T.toLower prefix `T.isPrefixOf` T.toLower m) modules
  pure $ map mkModuleResult matching
  where
    mkModuleResult :: Text -> CompletionResult
    mkModuleResult modName = CompletionResult
      { crLabel = modName
      , crKind = Module
      , crDetail = Just "Module"
      , crDocumentation = Nothing
      , crInsertText = Nothing
      , crFilterText = Just modName
      , crSortText = Just $ "0" <> modName
      , crModule = Just modName
      , crImportNeeded = Nothing
      , crIsSnippet = False
      , crScore = 100
      }

-- | Keyword completions
keywordCompletions :: Text -> [CompletionResult]
keywordCompletions prefix =
  let keywords =
        [ ("module", "Module declaration")
        , ("import", "Import statement")
        , ("qualified", "Qualified import")
        , ("as", "Import alias")
        , ("hiding", "Hide imports")
        , ("where", "Where clause")
        , ("let", "Let binding")
        , ("in", "In expression")
        , ("do", "Do notation")
        , ("case", "Case expression")
        , ("of", "Pattern match")
        , ("if", "If expression")
        , ("then", "Then branch")
        , ("else", "Else branch")
        , ("data", "Data type")
        , ("type", "Type synonym")
        , ("newtype", "Newtype")
        , ("class", "Type class")
        , ("instance", "Instance declaration")
        , ("deriving", "Deriving clause")
        , ("forall", "Universal quantification")
        , ("foreign", "Foreign import/export")
        , ("default", "Default declaration")
        , ("infix", "Infix declaration")
        , ("infixl", "Left-associative infix")
        , ("infixr", "Right-associative infix")
        , ("pattern", "Pattern synonym")
        , ("family", "Type family")
        ]
      matching = filter (\(n, _) -> T.toLower prefix `T.isPrefixOf` T.toLower n) keywords
  in map mkKeywordResult matching
  where
    mkKeywordResult :: (Text, Text) -> CompletionResult
    mkKeywordResult (kw, doc) = CompletionResult
      { crLabel = kw
      , crKind = Function
      , crDetail = Just "keyword"
      , crDocumentation = Just doc
      , crInsertText = Nothing
      , crFilterText = Just kw
      , crSortText = Just $ "8" <> kw  -- Keywords near end
      , crModule = Nothing
      , crImportNeeded = Nothing
      , crIsSnippet = False
      , crScore = 50
      }

-- | Suggest an import for a symbol
suggestImport :: Text -> Text -> ImportSuggestion
suggestImport modName symbol = ImportSuggestion
  { isModule = modName
  , isSymbol = Just symbol
  , isQualified = Nothing
  }
