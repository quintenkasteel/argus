{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Argus.Rules.SmartImports
-- Description : Intelligent import management with auto-fix support
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides intelligent import management including:
-- - Suggesting imports for unknown symbols
-- - Import organization and sorting
-- - Strict import upgrades (Map → Map.Strict)
-- - A comprehensive import database
module Argus.Rules.SmartImports
  ( -- * Import suggestions
    suggestImport
  , ImportSuggestion (..)
  , ImportDB

    -- * Import database
  , defaultImportDB
  , lookupSymbol

    -- * Import organization
  , organizeImports
  , ImportGroup (..)
  , ImportGroupResult (..)
  , groupImports

    -- * Strict import upgrades
  , suggestStrictImport
  , strictImportFixes
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Text qualified as T
import Data.Maybe ()
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)

import Argus.Types

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Information about where a symbol can be imported from
data ImportSuggestion = ImportSuggestion
  { isModuleName :: Text           -- ^ Module to import from
  , isQualifier  :: Maybe Text     -- ^ Suggested qualifier (e.g., "T" for Text)
  , isSymbol     :: Text           -- ^ The symbol itself
  , isQualified  :: Bool           -- ^ Should be qualified?
  , isPreferred  :: Bool           -- ^ Is this the preferred/canonical import?
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | The import database type
type ImportDB = Map Text [ImportSuggestion]

-- | Import group category
data ImportGroup
  = PreludeGroup       -- ^ Prelude and base
  | BaseGroup          -- ^ base package
  | ExternalGroup      -- ^ External packages
  | InternalGroup      -- ^ Project-internal modules
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Import Database
--------------------------------------------------------------------------------

-- | Default import database with common Haskell symbols
defaultImportDB :: ImportDB
defaultImportDB = Map.fromList
  -- Data.Text
  [ ("Text", [textSuggestion "Text" True])
  , ("pack", [textSuggestion "pack" False])
  , ("unpack", [textSuggestion "unpack" False])
  , ("strip", [textSuggestion "strip" False])
  , ("splitOn", [textSuggestion "splitOn" False])
  , ("intercalate", [textSuggestion "intercalate" False, listSuggestion "intercalate"])
  , ("isInfixOf", [textSuggestion "isInfixOf" False, listSuggestion "isInfixOf"])
  , ("isPrefixOf", [textSuggestion "isPrefixOf" False, listSuggestion "isPrefixOf"])
  , ("isSuffixOf", [textSuggestion "isSuffixOf" False, listSuggestion "isSuffixOf"])

  -- Data.Map
  , ("Map", [mapSuggestion "Map" True])
  , ("lookup", [mapSuggestion "lookup" True, listSuggestion "lookup"])
  , ("insert", [mapSuggestion "insert" True])
  , ("delete", [mapSuggestion "delete" True])
  , ("member", [mapSuggestion "member" True, setSuggestion "member" True])
  , ("fromList", [mapSuggestion "fromList" True, setSuggestion "fromList" True])
  , ("toList", [mapSuggestion "toList" True, setSuggestion "toList" True])
  , ("keys", [mapSuggestion "keys" True])
  , ("elems", [mapSuggestion "elems" True])
  , ("union", [mapSuggestion "union" True, setSuggestion "union" True])
  , ("intersection", [mapSuggestion "intersection" True, setSuggestion "intersection" True])
  , ("difference", [mapSuggestion "difference" True, setSuggestion "difference" True])

  -- Data.Set
  , ("Set", [setSuggestion "Set" True])
  , ("singleton", [setSuggestion "singleton" True])
  , ("empty", [mapSuggestion "empty" True, setSuggestion "empty" True])

  -- Data.ByteString
  , ("ByteString", [bsSuggestion "ByteString" True])
  , ("readFile", [bsSuggestion "readFile" True, textIOSuggestion "readFile"])
  , ("writeFile", [bsSuggestion "writeFile" True, textIOSuggestion "writeFile"])

  -- Control.Monad
  , ("when", [monadSuggestion "when"])
  , ("unless", [monadSuggestion "unless"])
  , ("void", [monadSuggestion "void"])
  , ("forM", [monadSuggestion "forM"])
  , ("forM_", [monadSuggestion "forM_"])
  , ("mapM", [monadSuggestion "mapM"])
  , ("mapM_", [monadSuggestion "mapM_"])
  , ("forever", [monadSuggestion "forever"])
  , ("join", [monadSuggestion "join"])
  , ("guard", [monadSuggestion "guard"])
  , ("msum", [monadSuggestion "msum"])
  , ("mfilter", [monadSuggestion "mfilter"])
  , ("filterM", [monadSuggestion "filterM"])
  , ("foldM", [monadSuggestion "foldM"])
  , ("foldM_", [monadSuggestion "foldM_"])
  , ("replicateM", [monadSuggestion "replicateM"])
  , ("replicateM_", [monadSuggestion "replicateM_"])

  -- Data.Maybe
  , ("fromMaybe", [maybeSuggestion "fromMaybe"])
  , ("catMaybes", [maybeSuggestion "catMaybes"])
  , ("mapMaybe", [maybeSuggestion "mapMaybe"])
  , ("isJust", [maybeSuggestion "isJust"])
  , ("isNothing", [maybeSuggestion "isNothing"])
  , ("listToMaybe", [maybeSuggestion "listToMaybe"])
  , ("maybeToList", [maybeSuggestion "maybeToList"])
  , ("fromJust", [maybeSuggestion "fromJust"])

  -- Data.Either
  , ("either", [eitherSuggestion "either"])
  , ("lefts", [eitherSuggestion "lefts"])
  , ("rights", [eitherSuggestion "rights"])
  , ("partitionEithers", [eitherSuggestion "partitionEithers"])
  , ("isLeft", [eitherSuggestion "isLeft"])
  , ("isRight", [eitherSuggestion "isRight"])
  , ("fromLeft", [eitherSuggestion "fromLeft"])
  , ("fromRight", [eitherSuggestion "fromRight"])

  -- Data.List
  , ("sort", [listSuggestion "sort"])
  , ("sortBy", [listSuggestion "sortBy"])
  , ("sortOn", [listSuggestion "sortOn"])
  , ("group", [listSuggestion "group"])
  , ("groupBy", [listSuggestion "groupBy"])
  , ("nub", [listSuggestion "nub"])
  , ("nubBy", [listSuggestion "nubBy"])
  , ("partition", [listSuggestion "partition"])
  , ("find", [listSuggestion "find"])
  , ("elemIndex", [listSuggestion "elemIndex"])
  , ("findIndex", [listSuggestion "findIndex"])
  , ("intersperse", [listSuggestion "intersperse"])
  , ("transpose", [listSuggestion "transpose"])

  -- Data.Foldable
  , ("fold", [foldableSuggestion "fold"])
  , ("foldMap", [foldableSuggestion "foldMap"])
  , ("toList", [foldableSuggestion "toList"])
  , ("null", [foldableSuggestion "null"])
  , ("length", [foldableSuggestion "length"])
  , ("elem", [foldableSuggestion "elem"])
  , ("maximum", [foldableSuggestion "maximum"])
  , ("minimum", [foldableSuggestion "minimum"])
  , ("sum", [foldableSuggestion "sum"])
  , ("product", [foldableSuggestion "product"])
  , ("any", [foldableSuggestion "any"])
  , ("all", [foldableSuggestion "all"])
  , ("and", [foldableSuggestion "and"])
  , ("or", [foldableSuggestion "or"])
  , ("traverse_", [foldableSuggestion "traverse_"])
  , ("for_", [foldableSuggestion "for_"])
  , ("sequenceA_", [foldableSuggestion "sequenceA_"])
  , ("asum", [foldableSuggestion "asum"])
  , ("msum", [foldableSuggestion "msum"])
  , ("concatMap", [foldableSuggestion "concatMap"])

  -- Data.Traversable
  , ("traverse", [traversableSuggestion "traverse"])
  , ("sequenceA", [traversableSuggestion "sequenceA"])
  , ("for", [traversableSuggestion "for"])
  , ("mapAccumL", [traversableSuggestion "mapAccumL"])
  , ("mapAccumR", [traversableSuggestion "mapAccumR"])

  -- Data.Functor
  , ("(<$>)", [functorSuggestion "(<$>)"])
  , ("(<&>)", [functorSuggestion "(<&>)"])
  , ("($>)", [functorSuggestion "($>)"])
  , ("(<$)", [functorSuggestion "(<$)"])

  -- Control.Applicative
  , ("liftA2", [applicativeSuggestion "liftA2"])
  , ("liftA3", [applicativeSuggestion "liftA3"])
  , ("(<|>)", [applicativeSuggestion "(<|>)"])
  , ("empty", [applicativeSuggestion "empty"])
  , ("optional", [applicativeSuggestion "optional"])

  -- Data.Bifunctor
  , ("bimap", [bifunctorSuggestion "bimap"])
  , ("first", [bifunctorSuggestion "first"])
  , ("second", [bifunctorSuggestion "second"])

  -- Data.Char
  , ("isAlpha", [charSuggestion "isAlpha"])
  , ("isDigit", [charSuggestion "isDigit"])
  , ("isAlphaNum", [charSuggestion "isAlphaNum"])
  , ("isSpace", [charSuggestion "isSpace"])
  , ("isUpper", [charSuggestion "isUpper"])
  , ("isLower", [charSuggestion "isLower"])
  , ("toUpper", [charSuggestion "toUpper"])
  , ("toLower", [charSuggestion "toLower"])
  , ("ord", [charSuggestion "ord"])
  , ("chr", [charSuggestion "chr"])

  -- Data.Ord
  , ("comparing", [ordSuggestion "comparing"])
  , ("Down", [ordSuggestion "Down"])

  -- Data.Function
  , ("on", [functionSuggestion "on"])
  , ("(&)", [functionSuggestion "(&)"])
  , ("fix", [functionSuggestion "fix"])

  -- Data.IORef
  , ("IORef", [ioRefSuggestion "IORef"])
  , ("newIORef", [ioRefSuggestion "newIORef"])
  , ("readIORef", [ioRefSuggestion "readIORef"])
  , ("writeIORef", [ioRefSuggestion "writeIORef"])
  , ("modifyIORef'", [ioRefSuggestion "modifyIORef'"])
  , ("atomicModifyIORef'", [ioRefSuggestion "atomicModifyIORef'"])

  -- Control.Exception
  , ("Exception", [exceptionSuggestion "Exception"])
  , ("SomeException", [exceptionSuggestion "SomeException"])
  , ("throw", [exceptionSuggestion "throw"])
  , ("throwIO", [exceptionSuggestion "throwIO"])
  , ("catch", [exceptionSuggestion "catch"])
  , ("try", [exceptionSuggestion "try"])
  , ("bracket", [exceptionSuggestion "bracket"])
  , ("bracket_", [exceptionSuggestion "bracket_"])
  , ("finally", [exceptionSuggestion "finally"])
  , ("onException", [exceptionSuggestion "onException"])

  -- Control.Concurrent
  , ("forkIO", [concurrentSuggestion "forkIO"])
  , ("threadDelay", [concurrentSuggestion "threadDelay"])
  , ("MVar", [mvarSuggestion "MVar"])
  , ("newMVar", [mvarSuggestion "newMVar"])
  , ("newEmptyMVar", [mvarSuggestion "newEmptyMVar"])
  , ("takeMVar", [mvarSuggestion "takeMVar"])
  , ("putMVar", [mvarSuggestion "putMVar"])
  , ("readMVar", [mvarSuggestion "readMVar"])
  , ("modifyMVar'", [mvarSuggestion "modifyMVar'"])

  -- Control.Concurrent.STM
  , ("STM", [stmSuggestion "STM"])
  , ("atomically", [stmSuggestion "atomically"])
  , ("TVar", [tvarSuggestion "TVar"])
  , ("newTVar", [tvarSuggestion "newTVar"])
  , ("readTVar", [tvarSuggestion "readTVar"])
  , ("writeTVar", [tvarSuggestion "writeTVar"])
  , ("modifyTVar'", [tvarSuggestion "modifyTVar'"])
  , ("retry", [stmSuggestion "retry"])
  , ("orElse", [stmSuggestion "orElse"])

  -- System.IO
  , ("Handle", [ioSuggestion "Handle"])
  , ("stdin", [ioSuggestion "stdin"])
  , ("stdout", [ioSuggestion "stdout"])
  , ("stderr", [ioSuggestion "stderr"])
  , ("withFile", [ioSuggestion "withFile"])
  , ("openFile", [ioSuggestion "openFile"])
  , ("hClose", [ioSuggestion "hClose"])
  , ("hPutStrLn", [ioSuggestion "hPutStrLn"])
  , ("hGetLine", [ioSuggestion "hGetLine"])

  -- GHC.Generics
  , ("Generic", [genericsSuggestion "Generic"])

  -- Data.Coerce
  , ("coerce", [coerceSuggestion "coerce"])
  , ("Coercible", [coerceSuggestion "Coercible"])

  -- Data.Proxy
  , ("Proxy", [proxySuggestion "Proxy"])

  -- Data.Void
  , ("Void", [voidSuggestion "Void"])
  , ("absurd", [voidSuggestion "absurd"])

  -- Data.Monoid
  , ("Monoid", [monoidSuggestion "Monoid"])
  , ("mempty", [monoidSuggestion "mempty"])
  , ("mappend", [monoidSuggestion "mappend"])
  , ("mconcat", [monoidSuggestion "mconcat"])
  , ("(<>)", [semigroupSuggestion "(<>)"])
  , ("Semigroup", [semigroupSuggestion "Semigroup"])

  -- Numeric types
  , ("Natural", [naturalSuggestion "Natural"])
  , ("Int8", [int8Suggestion "Int8"])
  , ("Int16", [int16Suggestion "Int16"])
  , ("Int32", [int32Suggestion "Int32"])
  , ("Int64", [int64Suggestion "Int64"])
  , ("Word8", [word8Suggestion "Word8"])
  , ("Word16", [word16Suggestion "Word16"])
  , ("Word32", [word32Suggestion "Word32"])
  , ("Word64", [word64Suggestion "Word64"])
  ]
  where
    textSuggestion sym qual = ImportSuggestion "Data.Text" (Just "T") sym qual True
    mapSuggestion sym qual = ImportSuggestion "Data.Map.Strict" (Just "Map") sym qual True
    setSuggestion sym qual = ImportSuggestion "Data.Set" (Just "Set") sym qual True
    bsSuggestion sym qual = ImportSuggestion "Data.ByteString" (Just "BS") sym qual True
    textIOSuggestion sym = ImportSuggestion "Data.Text.IO" (Just "TIO") sym True True
    monadSuggestion sym = ImportSuggestion "Control.Monad" Nothing sym False True
    maybeSuggestion sym = ImportSuggestion "Data.Maybe" Nothing sym False True
    eitherSuggestion sym = ImportSuggestion "Data.Either" Nothing sym False True
    listSuggestion sym = ImportSuggestion "Data.List" Nothing sym False True
    foldableSuggestion sym = ImportSuggestion "Data.Foldable" Nothing sym False True
    traversableSuggestion sym = ImportSuggestion "Data.Traversable" Nothing sym False True
    functorSuggestion sym = ImportSuggestion "Data.Functor" Nothing sym False True
    applicativeSuggestion sym = ImportSuggestion "Control.Applicative" Nothing sym False True
    bifunctorSuggestion sym = ImportSuggestion "Data.Bifunctor" Nothing sym False True
    charSuggestion sym = ImportSuggestion "Data.Char" Nothing sym False True
    ordSuggestion sym = ImportSuggestion "Data.Ord" Nothing sym False True
    functionSuggestion sym = ImportSuggestion "Data.Function" Nothing sym False True
    ioRefSuggestion sym = ImportSuggestion "Data.IORef" Nothing sym False True
    exceptionSuggestion sym = ImportSuggestion "Control.Exception" Nothing sym False True
    concurrentSuggestion sym = ImportSuggestion "Control.Concurrent" Nothing sym False True
    mvarSuggestion sym = ImportSuggestion "Control.Concurrent.MVar" Nothing sym False True
    stmSuggestion sym = ImportSuggestion "Control.Concurrent.STM" Nothing sym False True
    tvarSuggestion sym = ImportSuggestion "Control.Concurrent.STM.TVar" Nothing sym False True
    ioSuggestion sym = ImportSuggestion "System.IO" Nothing sym False True
    genericsSuggestion sym = ImportSuggestion "GHC.Generics" Nothing sym False True
    coerceSuggestion sym = ImportSuggestion "Data.Coerce" Nothing sym False True
    proxySuggestion sym = ImportSuggestion "Data.Proxy" Nothing sym False True
    voidSuggestion sym = ImportSuggestion "Data.Void" Nothing sym False True
    monoidSuggestion sym = ImportSuggestion "Data.Monoid" Nothing sym False True
    semigroupSuggestion sym = ImportSuggestion "Data.Semigroup" Nothing sym False True
    naturalSuggestion sym = ImportSuggestion "Numeric.Natural" Nothing sym False True
    int8Suggestion sym = ImportSuggestion "Data.Int" Nothing sym False True
    int16Suggestion sym = ImportSuggestion "Data.Int" Nothing sym False True
    int32Suggestion sym = ImportSuggestion "Data.Int" Nothing sym False True
    int64Suggestion sym = ImportSuggestion "Data.Int" Nothing sym False True
    word8Suggestion sym = ImportSuggestion "Data.Word" Nothing sym False True
    word16Suggestion sym = ImportSuggestion "Data.Word" Nothing sym False True
    word32Suggestion sym = ImportSuggestion "Data.Word" Nothing sym False True
    word64Suggestion sym = ImportSuggestion "Data.Word" Nothing sym False True

--------------------------------------------------------------------------------
-- Import Suggestions
--------------------------------------------------------------------------------

-- | Look up a symbol in the import database
lookupSymbol :: Text -> ImportDB -> [ImportSuggestion]
lookupSymbol symbol db = Map.findWithDefault [] symbol db

-- | Suggest imports for an unknown symbol
suggestImport :: Text -> [ImportSuggestion]
suggestImport symbol = lookupSymbol symbol defaultImportDB

-- | Generate a fix for adding an import
_generateImportFix :: ImportSuggestion -> FilePath -> Text -> Fix
_generateImportFix ImportSuggestion{..} _path _content = Fix
  { fixTitle = "Import " <> isSymbol <> " from " <> isModuleName
  , fixEdits = []  -- Would need to find import section
  , fixIsPreferred = isPreferred
  , fixAddImports = [FixImport
      { fimpModule = isModuleName
      , fimpSymbols = [ImportSymbol isSymbol ISTFunction []]
      , fimpQualified = Nothing
      , fimpHiding = False
      , fimpPackage = Nothing
      }]
  , fixRemoveImports = []
  , fixCategory = FCImports
  , fixSafety = FSAlways
  }

--------------------------------------------------------------------------------
-- Strict Import Suggestions
--------------------------------------------------------------------------------

-- | Lazy to strict module mappings
strictImportFixes :: [(Text, Text)]
strictImportFixes =
  [ ("Data.Map", "Data.Map.Strict")
  , ("Data.IntMap", "Data.IntMap.Strict")
  , ("Data.HashMap.Lazy", "Data.HashMap.Strict")
  , ("Control.Monad.State", "Control.Monad.State.Strict")
  , ("Control.Monad.Trans.State", "Control.Monad.Trans.State.Strict")
  , ("Control.Monad.Writer", "Control.Monad.Writer.Strict")
  , ("Control.Monad.Trans.Writer", "Control.Monad.Trans.Writer.Strict")
  , ("Control.Monad.RWS", "Control.Monad.RWS.Strict")
  , ("Control.Monad.Trans.RWS", "Control.Monad.Trans.RWS.Strict")
  ]

-- | Suggest strict import for a lazy module
suggestStrictImport :: Text -> Maybe (Text, Fix)
suggestStrictImport modName = case lookup modName strictImportFixes of
  Nothing -> Nothing
  Just strictMod -> Just (strictMod, Fix
    { fixTitle = "Use " <> strictMod <> " instead"
    , fixEdits = []  -- Would need actual import span
    , fixIsPreferred = True
    , fixAddImports = []
    , fixRemoveImports = [modName]
    , fixCategory = FCSpaceLeaks
    , fixSafety = FSMostly
    })

--------------------------------------------------------------------------------
-- Import Organization
--------------------------------------------------------------------------------

-- | Determine the group for a module
moduleGroup :: Text -> ImportGroup
moduleGroup modName
  | modName == "Prelude" = PreludeGroup
  -- Check for external packages first (before base prefix matching)
  | "Aeson" `T.isInfixOf` modName = ExternalGroup
  | "Lens" `T.isInfixOf` modName = ExternalGroup
  | "Conduit" `T.isInfixOf` modName = ExternalGroup
  | "Network" `T.isPrefixOf` modName = ExternalGroup
  | "Database" `T.isPrefixOf` modName = ExternalGroup
  | "Test" `T.isPrefixOf` modName = ExternalGroup
  -- Base library prefixes
  | "Data." `T.isPrefixOf` modName = BaseGroup
  | "Control." `T.isPrefixOf` modName = BaseGroup
  | "System." `T.isPrefixOf` modName = BaseGroup
  | "GHC." `T.isPrefixOf` modName = BaseGroup
  | "Text." `T.isPrefixOf` modName = BaseGroup
  -- Assume project-internal if it doesn't match above patterns
  | otherwise = InternalGroup

-- | Representation of import grouping result
data ImportGroupResult = ImportGroupResult
  { igrGroup :: ImportGroup
  , igrModules :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Group imports by category
groupImports :: [Text] -> [ImportGroupResult]
groupImports modules =
  let grouped = Map.toList $ Map.fromListWith (++)
                  [(moduleGroup m, [m]) | m <- modules]
  in map (uncurry ImportGroupResult) grouped

-- | Organize imports (sort within groups, separate groups)
organizeImports :: [Text] -> [Text]
organizeImports modules =
  let groups = groupImports modules
      sortedGroups = map (\(ImportGroupResult _ ms) -> T.intercalate "\n" (T.strip <$> ms)) groups
  in concatMap T.lines sortedGroups
