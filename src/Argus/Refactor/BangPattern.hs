{-# LANGUAGE StrictData #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Argus.Refactor.BangPattern
-- Description : Automatic bang pattern insertion for strictness
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides automatic insertion of bang patterns to enforce
-- strictness in Haskell code, particularly for:
--
-- * Accumulator arguments in recursive functions
-- * Strict bindings in let/where clauses
-- * Function parameters that should be strict
-- * Data constructor fields that need strictness
module Argus.Refactor.BangPattern
  ( -- * Configuration
    BangPatternConfig (..)
  , defaultBangPatternConfig
  , StrictnessMode (..)
  , InsertionStrategy (..)

    -- * Analysis
  , BangPatternAnalysis (..)
  , BangPatternCandidate (..)
  , CandidateKind (..)
  , analyzeBangPatterns
  , findBangPatternCandidates

    -- * Fix Generation
  , generateBangPatternFixes
  , insertBangPattern
  , insertBangPatterns

    -- * Pattern Detection
  , detectAccumulatorPatterns
  , detectStrictBindings
  , detectLazyParameters
  , detectRecordFields

    -- * Utilities
  , isAccumulatorName
  , isStrictContext
  , shouldBeStrict
  , bangPatternAnalysisToFixes
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), (.=), (.:?), (.!=))
import Data.Aeson qualified as Aeson
import Data.Char (isAlphaNum, isLower)
import Data.List (sortBy)
import Data.Maybe (mapMaybe, catMaybes, fromMaybe)
import Data.Ord (comparing, Down(..))
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Text.Regex.TDFA ((=~))

import Argus.Analysis.TextProcessing (stripStringsAndComments)
import Argus.Types

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | How aggressive to be with strictness insertions
data StrictnessMode
  = Conservative  -- ^ Only insert bangs for clear accumulator patterns
  | Moderate      -- ^ Include likely strict bindings
  | Aggressive    -- ^ Insert bangs for all potential candidates
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToJSON StrictnessMode where
  toJSON = \case
    Conservative -> Aeson.String "conservative"
    Moderate -> Aeson.String "moderate"
    Aggressive -> Aeson.String "aggressive"

instance FromJSON StrictnessMode where
  parseJSON = Aeson.withText "StrictnessMode" $ \case
    "conservative" -> pure Conservative
    "moderate" -> pure Moderate
    "aggressive" -> pure Aggressive
    other -> fail $ "Unknown StrictnessMode: " <> T.unpack other

-- | Strategy for where to insert bang patterns
data InsertionStrategy
  = InsertInPattern      -- ^ Insert in function pattern: f !acc = ...
  | InsertInLet          -- ^ Use let !x = ... in pattern-like contexts
  | InsertSeq            -- ^ Use seq: let x = acc in x `seq` f x
  | InsertBangBefore     -- ^ Insert $! before application: f $! acc
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)

instance ToJSON InsertionStrategy where
  toJSON = \case
    InsertInPattern -> Aeson.String "pattern"
    InsertInLet -> Aeson.String "let"
    InsertSeq -> Aeson.String "seq"
    InsertBangBefore -> Aeson.String "bang_before"

instance FromJSON InsertionStrategy where
  parseJSON = Aeson.withText "InsertionStrategy" $ \case
    "pattern" -> pure InsertInPattern
    "let" -> pure InsertInLet
    "seq" -> pure InsertSeq
    "bang_before" -> pure InsertBangBefore
    other -> fail $ "Unknown InsertionStrategy: " <> T.unpack other

-- | Configuration for bang pattern insertion
data BangPatternConfig = BangPatternConfig
  { bpcEnabled           :: Bool               -- ^ Enable bang pattern analysis
  , bpcMode              :: StrictnessMode     -- ^ How aggressive
  , bpcStrategy          :: InsertionStrategy  -- ^ Where to insert
  , bpcAccumulatorNames  :: [Text]             -- ^ Names suggesting accumulators
  , bpcExcludePatterns   :: [Text]             -- ^ Patterns to skip
  , bpcRequireAnnotation :: Bool               -- ^ Require existing strictness hints
  , bpcCheckRecordFields :: Bool               -- ^ Check data constructor fields
  , bpcCheckLetBindings  :: Bool               -- ^ Check let/where bindings
  , bpcCheckLambdas      :: Bool               -- ^ Check lambda parameters
  , bpcMinFunctionSize   :: Int                -- ^ Min lines for function analysis
  , bpcMaxCandidates     :: Int                -- ^ Max candidates per file
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON BangPatternConfig where
  toJSON BangPatternConfig{..} = Aeson.object
    [ "enabled" .= bpcEnabled
    , "mode" .= bpcMode
    , "strategy" .= bpcStrategy
    , "accumulator_names" .= bpcAccumulatorNames
    , "exclude_patterns" .= bpcExcludePatterns
    , "require_annotation" .= bpcRequireAnnotation
    , "check_record_fields" .= bpcCheckRecordFields
    , "check_let_bindings" .= bpcCheckLetBindings
    , "check_lambdas" .= bpcCheckLambdas
    , "min_function_size" .= bpcMinFunctionSize
    , "max_candidates" .= bpcMaxCandidates
    ]

instance FromJSON BangPatternConfig where
  parseJSON = Aeson.withObject "BangPatternConfig" $ \o -> do
    bpcEnabled <- o .:? "enabled" .!= True
    bpcMode <- o .:? "mode" .!= Moderate
    bpcStrategy <- o .:? "strategy" .!= InsertInPattern
    bpcAccumulatorNames <- o .:? "accumulator_names" .!= defaultAccumulatorNames
    bpcExcludePatterns <- o .:? "exclude_patterns" .!= []
    bpcRequireAnnotation <- o .:? "require_annotation" .!= False
    bpcCheckRecordFields <- o .:? "check_record_fields" .!= True
    bpcCheckLetBindings <- o .:? "check_let_bindings" .!= True
    bpcCheckLambdas <- o .:? "check_lambdas" .!= False
    bpcMinFunctionSize <- o .:? "min_function_size" .!= 3
    bpcMaxCandidates <- o .:? "max_candidates" .!= 100
    pure BangPatternConfig{..}

-- | Default accumulator name patterns
defaultAccumulatorNames :: [Text]
defaultAccumulatorNames =
  [ "acc", "accum", "accumulator"
  , "sum", "total", "count", "counter"
  , "result", "res", "out", "output"
  , "state", "st", "s"
  , "len", "length", "size"
  , "min", "max", "minVal", "maxVal"
  , "n", "m", "i", "j", "k"
  ]

-- | Default configuration
defaultBangPatternConfig :: BangPatternConfig
defaultBangPatternConfig = BangPatternConfig
  { bpcEnabled = True
  , bpcMode = Moderate
  , bpcStrategy = InsertInPattern
  , bpcAccumulatorNames = defaultAccumulatorNames
  , bpcExcludePatterns = []
  , bpcRequireAnnotation = False
  , bpcCheckRecordFields = True
  , bpcCheckLetBindings = True
  , bpcCheckLambdas = False
  , bpcMinFunctionSize = 3
  , bpcMaxCandidates = 100
  }

--------------------------------------------------------------------------------
-- Analysis Types
--------------------------------------------------------------------------------

-- | Kind of bang pattern candidate
data CandidateKind
  = AccumulatorArg         -- ^ Accumulator in recursive function
  | StrictLetBinding       -- ^ Strict let/where binding
  | RecursiveParameter     -- ^ Parameter in recursive call
  | RecordFieldStrict      -- ^ Record field needing strictness
  | LambdaParameter        -- ^ Lambda that should be strict
  | FoldAccumulator        -- ^ Accumulator in fold-like pattern
  | StateAccumulator       -- ^ State-like accumulator
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A candidate location for bang pattern insertion
data BangPatternCandidate = BangPatternCandidate
  { bpcVarName       :: Text            -- ^ Variable name
  , bpcVarSpan       :: SrcSpan         -- ^ Location of the variable
  , bpcKind          :: CandidateKind   -- ^ Type of candidate
  , bpcConfidence    :: Double          -- ^ 0.0 to 1.0 confidence score
  , bpcContext       :: Text            -- ^ Surrounding code context
  , bpcFunctionName  :: Maybe Text      -- ^ Enclosing function name
  , bpcReason        :: Text            -- ^ Why this needs a bang
  , bpcFixText       :: Text            -- ^ The suggested fix text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Complete analysis of bang pattern opportunities
data BangPatternAnalysis = BangPatternAnalysis
  { bpaPath             :: FilePath
  , bpaCandidates       :: [BangPatternCandidate]
  , bpaAccumulatorCount :: Int
  , bpaLetBindingCount  :: Int
  , bpaRecordFieldCount :: Int
  , bpaLambdaCount      :: Int
  , bpaTotalConfidence  :: Double
  , bpaWarnings         :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Analysis Implementation
--------------------------------------------------------------------------------

-- | Perform full bang pattern analysis on source code
analyzeBangPatterns :: BangPatternConfig -> FilePath -> Text -> BangPatternAnalysis
analyzeBangPatterns config path content
  | not (bpcEnabled config) = emptyAnalysis path
  | otherwise =
    let candidates = findBangPatternCandidates config path content
        filtered = filterByMode (bpcMode config) candidates
        limited = take (bpcMaxCandidates config) $
                  sortBy (comparing (Down . bpcConfidence)) filtered
        accCount = length $ filter ((== AccumulatorArg) . bpcKind) limited
        letCount = length $ filter ((== StrictLetBinding) . bpcKind) limited
        recCount = length $ filter ((== RecordFieldStrict) . bpcKind) limited
        lamCount = length $ filter ((== LambdaParameter) . bpcKind) limited
        totalConf = sum $ map bpcConfidence limited
        warnings = generateWarnings config limited
    in BangPatternAnalysis
      { bpaPath = path
      , bpaCandidates = limited
      , bpaAccumulatorCount = accCount
      , bpaLetBindingCount = letCount
      , bpaRecordFieldCount = recCount
      , bpaLambdaCount = lamCount
      , bpaTotalConfidence = totalConf
      , bpaWarnings = warnings
      }

-- | Empty analysis result
emptyAnalysis :: FilePath -> BangPatternAnalysis
emptyAnalysis path = BangPatternAnalysis
  { bpaPath = path
  , bpaCandidates = []
  , bpaAccumulatorCount = 0
  , bpaLetBindingCount = 0
  , bpaRecordFieldCount = 0
  , bpaLambdaCount = 0
  , bpaTotalConfidence = 0
  , bpaWarnings = []
  }

-- | Filter candidates based on strictness mode
filterByMode :: StrictnessMode -> [BangPatternCandidate] -> [BangPatternCandidate]
filterByMode mode = filter $ \c ->
  case mode of
    Conservative -> bpcConfidence c >= 0.8
    Moderate -> bpcConfidence c >= 0.5
    Aggressive -> bpcConfidence c >= 0.2

-- | Generate warnings for the analysis
generateWarnings :: BangPatternConfig -> [BangPatternCandidate] -> [Text]
generateWarnings config candidates =
  let lowConfidence = filter (\c -> bpcConfidence c < 0.5) candidates
      hasLowConf = not (null lowConfidence) && bpcMode config == Aggressive
      manyAccs = length (filter ((== AccumulatorArg) . bpcKind) candidates) > 10
  in catMaybes
    [ if hasLowConf
        then Just $ "Low confidence candidates included in aggressive mode: " <>
                    T.pack (show (length lowConfidence))
        else Nothing
    , if manyAccs
        then Just "Many accumulator patterns detected - consider reviewing function design"
        else Nothing
    ]

-- | Find all bang pattern candidates in source code
findBangPatternCandidates :: BangPatternConfig -> FilePath -> Text -> [BangPatternCandidate]
findBangPatternCandidates config path content =
  let accCandidates = detectAccumulatorPatterns config path content
      letCandidates = if bpcCheckLetBindings config
                      then detectStrictBindings config path content
                      else []
      recCandidates = if bpcCheckRecordFields config
                      then detectRecordFields config path content
                      else []
      lamCandidates = if bpcCheckLambdas config
                      then detectLazyParameters config path content
                      else []
  in accCandidates ++ letCandidates ++ recCandidates ++ lamCandidates

--------------------------------------------------------------------------------
-- Accumulator Pattern Detection
--------------------------------------------------------------------------------

-- | Detect accumulator patterns in recursive functions
detectAccumulatorPatterns :: BangPatternConfig -> FilePath -> Text -> [BangPatternCandidate]
detectAccumulatorPatterns config path content =
  let linesWithNum = zip [1..] (T.lines content)
      functions = findRecursiveFunctions linesWithNum
  in concatMap (findAccumulatorsInFunction config path linesWithNum) functions

-- | Information about a recursive function
data RecursiveFunction = RecursiveFunction
  { rfName      :: Text
  , rfStartLine :: Int
  , rfEndLine   :: Int
  , rfParams    :: [Text]
  }
  deriving stock (Eq, Show)

-- | Find recursive functions in the source
findRecursiveFunctions :: [(Int, Text)] -> [RecursiveFunction]
findRecursiveFunctions linesWithNum =
  let funDefs = findFunctionDefinitions linesWithNum
  in mapMaybe (checkRecursive linesWithNum) funDefs

-- | Find function definitions
findFunctionDefinitions :: [(Int, Text)] -> [(Int, Text, [Text])]
findFunctionDefinitions = mapMaybe extractFunDef
  where
    extractFunDef (lineNum, line)
      | T.null stripped = Nothing
      | T.isPrefixOf "--" stripped = Nothing
      | T.isPrefixOf "{-" stripped = Nothing
      | otherwise = extractDef lineNum stripped
      where
        stripped = T.stripStart line

    extractDef lineNum line =
      -- Pattern: funcName param1 param2 ... = body
      -- or: funcName param1 param2 ...
      --       | guard = body
      let parts = T.words line
      in case parts of
        (name:rest)
          | isValidFuncName name
          , (params, afterParams) <- span (\w -> w `notElem` ["=", "|", "where"]) rest
          , not (null afterParams) || not (null params)
          -> Just (lineNum, name, filter isValidParam params)
        _ -> Nothing

    isValidFuncName name =
      case T.uncons name of
        Nothing -> False
        Just (c, _) ->
          isLower c &&
          T.all (\ch -> isAlphaNum ch || ch == '\'' || ch == '_') name &&
          name `notElem` ["let", "in", "where", "do", "case", "of", "if", "then", "else"]

    isValidParam p =
      case T.uncons p of
        Nothing -> False
        Just (c, _) ->
          (isLower c || c == '_') &&
          T.all (\ch -> isAlphaNum ch || ch == '\'' || ch == '_') p

-- | Check if a function is recursive
checkRecursive :: [(Int, Text)] -> (Int, Text, [Text]) -> Maybe RecursiveFunction
checkRecursive linesWithNum (startLine, name, params) =
  let endLine = findFunctionEnd startLine linesWithNum
      bodyLines = takeWhile (\(n, _) -> n <= endLine) $
                  dropWhile (\(n, _) -> n < startLine) linesWithNum
      hasRecursiveCall = any (\(_, l) -> nameInCode name l) bodyLines
  in if hasRecursiveCall
     then Just RecursiveFunction
       { rfName = name
       , rfStartLine = startLine
       , rfEndLine = endLine
       , rfParams = params
       }
     else Nothing

-- | Check if a name appears in code (not in comments/strings)
nameInCode :: Text -> Text -> Bool
nameInCode name line =
  let stripped = stripStringsAndComments line
      -- Match as word boundary
      regex = "\\b" <> T.unpack name <> "\\b"
  in T.unpack stripped =~ (regex :: String)

-- | Find where a function ends
findFunctionEnd :: Int -> [(Int, Text)] -> Int
findFunctionEnd startLine linesWithNum =
  let afterStart = dropWhile (\(n, _) -> n <= startLine) linesWithNum
      -- Function ends when we see a new top-level definition or blank line + def
      nextTopLevel = findNextTopLevel afterStart
  in fromMaybe (fst $ last linesWithNum) nextTopLevel

-- | Find next top-level definition
findNextTopLevel :: [(Int, Text)] -> Maybe Int
findNextTopLevel [] = Nothing
findNextTopLevel ((n, line):rest)
  | isTopLevelDef line = Just (n - 1)
  | otherwise = findNextTopLevel rest
  where
    isTopLevelDef l =
      let stripped = T.stripStart l
      in case T.uncons l of
        Nothing -> False  -- Empty line is not a top-level def
        Just (firstChar, _) ->
          not (T.null stripped) &&
          firstChar /= ' ' &&
          not (T.isPrefixOf "--" stripped) &&
          not (T.isPrefixOf "{-" stripped) &&
          (stripped =~ ("^[a-z_][a-zA-Z0-9_']*\\s" :: String) ||
           stripped =~ ("^[a-z_][a-zA-Z0-9_']*$" :: String))

-- | Find accumulators in a recursive function
findAccumulatorsInFunction :: BangPatternConfig
                           -> FilePath
                           -> [(Int, Text)]
                           -> RecursiveFunction
                           -> [BangPatternCandidate]
findAccumulatorsInFunction config path linesWithNum RecursiveFunction{..} =
  let accumulatorParams = filter (isAccumulatorName (bpcAccumulatorNames config)) rfParams
      -- Find the parameter positions in the definition line
      defLine = fromMaybe "" $ lookup rfStartLine linesWithNum
      candidates = mapMaybe (makeCandidate defLine) accumulatorParams
  in candidates
  where
    makeCandidate defLine param =
      let col = findParamColumn param defLine
      in case col of
        Nothing -> Nothing
        Just c ->
          let paramSpan = mkSrcSpanRaw path rfStartLine c rfStartLine (c + T.length param)
              confidence = computeConfidence param
              context = T.take 80 defLine
          in Just BangPatternCandidate
            { bpcVarName = param
            , bpcVarSpan = paramSpan
            , bpcKind = AccumulatorArg
            , bpcConfidence = confidence
            , bpcContext = context
            , bpcFunctionName = Just rfName
            , bpcReason = "Accumulator argument '" <> param <>
                         "' in recursive function should be strict"
            , bpcFixText = "!" <> param
            }

    computeConfidence param
      | param `elem` ["acc", "accum", "accumulator"] = 0.95
      | param `elem` ["sum", "total", "count"] = 0.9
      | param `elem` ["result", "res", "out"] = 0.85
      | param `elem` ["n", "m", "i", "j", "k"] = 0.6
      | otherwise = 0.7

-- | Find the column of a parameter in a line
findParamColumn :: Text -> Text -> Maybe Int
findParamColumn param line =
  let regex = "\\b" <> T.unpack param <> "\\b"
      matches = T.unpack line =~ (regex :: String) :: Bool
  in if matches
     then findFirstMatch param line 1
     else Nothing

-- | Find first occurrence of param as a word
findFirstMatch :: Text -> Text -> Int -> Maybe Int
findFirstMatch param line col =
  case T.uncons line of
    Nothing -> Nothing
    Just (_, lineTail)
      | T.isPrefixOf param line && isWordBoundary (T.drop (T.length param) line) ->
          Just col
      | otherwise -> findFirstMatch param lineTail (col + 1)
  where
    isWordBoundary rest =
      case T.uncons rest of
        Nothing -> True
        Just (c, _) -> not (isAlphaNum c || c == '\'')

-- | Check if a name suggests an accumulator
isAccumulatorName :: [Text] -> Text -> Bool
isAccumulatorName patterns name =
  let lowerName = T.toLower name
  in any (`T.isInfixOf` lowerName) (map T.toLower patterns)

--------------------------------------------------------------------------------
-- Strict Binding Detection
--------------------------------------------------------------------------------

-- | Detect let/where bindings that should be strict
detectStrictBindings :: BangPatternConfig -> FilePath -> Text -> [BangPatternCandidate]
detectStrictBindings config path content =
  let linesWithNum = zip [1..] (T.lines content)
      letBindings = findLetBindings linesWithNum
      whereBindings = findWhereBindings linesWithNum
  in mapMaybe (analyzeBinding config path) (letBindings ++ whereBindings)

-- | Find let bindings
findLetBindings :: [(Int, Text)] -> [(Int, Text, Text)]
findLetBindings = mapMaybe extractLet
  where
    extractLet (lineNum, line)
      | "let " `T.isInfixOf` stripped || T.isPrefixOf "let " stripped =
          case extractLetBinding stripped of
            Just (name, rest) -> Just (lineNum, name, rest)
            Nothing -> Nothing
      | otherwise = Nothing
      where
        stripped = T.stripStart line

    extractLetBinding text =
      let afterLet = T.strip $ T.drop 4 $ snd $ T.breakOn "let " text
          parts = T.words afterLet
      in case parts of
        (name:"=":_) | isBindingName name -> Just (name, afterLet)
        _ -> Nothing

-- | Find where bindings
findWhereBindings :: [(Int, Text)] -> [(Int, Text, Text)]
findWhereBindings linesWithNum =
  let whereIndices = map fst $ filter (hasWhere . snd) linesWithNum
  in concatMap (extractWhereBindings linesWithNum) whereIndices
  where
    hasWhere line = "where" `T.isInfixOf` stripStringsAndComments line

    extractWhereBindings lns whereIdx =
      let afterWhere = takeWhile (isWhereBinding whereIdx) $
                       dropWhile (\(n, _) -> n <= whereIdx) lns
      in mapMaybe extractBinding afterWhere

    isWhereBinding whereIdx (n, line) =
      let stripped = T.stripStart line
          indent = T.length line - T.length stripped
      in n > whereIdx && indent > 0 && not (T.null stripped)

    extractBinding (lineNum, line)
      | (name:"=":_) <- T.words (T.stripStart line)
      , isBindingName name
      = Just (lineNum, name, T.stripStart line)
      | otherwise = Nothing

-- | Check if a name is a valid binding name
isBindingName :: Text -> Bool
isBindingName name =
  case T.uncons name of
    Nothing -> False
    Just (c, _) ->
      isLower c &&
      T.all (\ch -> isAlphaNum ch || ch == '\'' || ch == '_') name &&
      name `notElem` ["let", "in", "where", "do", "case", "of", "if", "then", "else"]

-- | Analyze a binding for strictness
analyzeBinding :: BangPatternConfig -> FilePath -> (Int, Text, Text) -> Maybe BangPatternCandidate
analyzeBinding config path (lineNum, name, context) =
  let shouldBeStrictBinding = shouldBeStrict (bpcAccumulatorNames config) name context
  in if shouldBeStrictBinding
     then Just BangPatternCandidate
       { bpcVarName = name
       , bpcVarSpan = mkSrcSpanRaw path lineNum 1 lineNum (1 + T.length name)
       , bpcKind = StrictLetBinding
       , bpcConfidence = 0.6
       , bpcContext = context
       , bpcFunctionName = Nothing
       , bpcReason = "Let binding '" <> name <> "' may benefit from strictness"
       , bpcFixText = "!" <> name
       }
     else Nothing

--------------------------------------------------------------------------------
-- Record Field Detection
--------------------------------------------------------------------------------

-- | Detect record fields that should be strict
detectRecordFields :: BangPatternConfig -> FilePath -> Text -> [BangPatternCandidate]
detectRecordFields config path content =
  let linesWithNum = zip [1..] (T.lines content)
      dataDecls = findDataDeclarations linesWithNum
  in concatMap (findStrictableFields config path) dataDecls

-- | Find data declarations
findDataDeclarations :: [(Int, Text)] -> [(Int, [Text])]
findDataDeclarations linesWithNum =
  let dataLines = filter (isDataDecl . snd) linesWithNum
  in map (\(n, _) -> (n, extractFields n linesWithNum)) dataLines
  where
    isDataDecl line =
      let stripped = T.stripStart line
      in T.isPrefixOf "data " stripped || T.isPrefixOf "newtype " stripped

    extractFields startLine lns =
      let declLines = takeWhile (isPartOfDecl startLine) $
                      dropWhile (\(n, _) -> n < startLine) lns
          allText = T.unlines (map snd declLines)
      in extractFieldNames allText

    isPartOfDecl startLine (n, line)
      | n == startLine = True
      | T.null (T.stripStart line) = False
      | otherwise = case T.uncons line of
          Nothing -> False
          Just (c, _) -> c == ' ' || "deriving" `T.isPrefixOf` T.stripStart line

    extractFieldNames text =
      -- Look for record syntax: { field1 :: Type, field2 :: Type }
      let afterBrace = T.dropWhile (/= '{') text
      in if T.null afterBrace
         then []
         else extractRecordFields afterBrace

    extractRecordFields text =
      let fields = T.splitOn "," $ T.takeWhile (/= '}') $ T.drop 1 text
      in mapMaybe extractFieldName fields

    extractFieldName field =
      let stripped = T.strip field
          parts = T.words stripped
      in case parts of
        (name:_) | isBindingName name -> Just name
        _ -> Nothing

-- | Find fields that could benefit from strictness
findStrictableFields :: BangPatternConfig -> FilePath -> (Int, [Text]) -> [BangPatternCandidate]
findStrictableFields config path (lineNum, fields) =
  mapMaybe mkCandidate fields
  where
    mkCandidate field =
      let shouldStrict = isStrictContext (bpcAccumulatorNames config) field
      in if shouldStrict
         then Just BangPatternCandidate
           { bpcVarName = field
           , bpcVarSpan = mkSrcSpanRaw path lineNum 1 lineNum (1 + T.length field)
           , bpcKind = RecordFieldStrict
           , bpcConfidence = 0.5
           , bpcContext = field
           , bpcFunctionName = Nothing
           , bpcReason = "Record field '" <> field <> "' may benefit from strictness annotation"
           , bpcFixText = "!" <> field
           }
         else Nothing

--------------------------------------------------------------------------------
-- Lambda Parameter Detection
--------------------------------------------------------------------------------

-- | Detect lambda parameters that should be strict
detectLazyParameters :: BangPatternConfig -> FilePath -> Text -> [BangPatternCandidate]
detectLazyParameters config path content
  | not (bpcCheckLambdas config) = []
  | otherwise =
      let linesWithNum = zip [1..] (T.lines content)
          lambdas = findLambdas linesWithNum
      in mapMaybe (analyzeLambda config path) lambdas

-- | Find lambdas in source
findLambdas :: [(Int, Text)] -> [(Int, Text, [Text])]
findLambdas = mapMaybe extractLambda
  where
    extractLambda (lineNum, line)
      | "\\" `T.isInfixOf` stripped =
          let params = extractLambdaParams stripped
          in if null params
             then Nothing
             else Just (lineNum, stripped, params)
      | otherwise = Nothing
      where
        stripped = stripStringsAndComments line

    extractLambdaParams text =
      -- Find \param1 param2 -> body
      let afterSlash = T.dropWhile (/= '\\') text
      in if T.null afterSlash
         then []
         else let afterBackslash = T.drop 1 afterSlash
                  beforeArrow = T.takeWhile (\c -> c /= '-' && c /= '→') afterBackslash
              in filter isBindingName $ T.words beforeArrow

-- | Analyze a lambda for strict parameter opportunities
analyzeLambda :: BangPatternConfig -> FilePath -> (Int, Text, [Text]) -> Maybe BangPatternCandidate
analyzeLambda config path (lineNum, context, params) =
  let strictParams = filter (isAccumulatorName (bpcAccumulatorNames config)) params
  in case strictParams of
    [] -> Nothing
    (p:_) -> Just BangPatternCandidate
      { bpcVarName = p
      , bpcVarSpan = mkSrcSpanRaw path lineNum 1 lineNum (1 + T.length p)
      , bpcKind = LambdaParameter
      , bpcConfidence = 0.4
      , bpcContext = context
      , bpcFunctionName = Nothing
      , bpcReason = "Lambda parameter '" <> p <> "' may benefit from strictness"
      , bpcFixText = "!" <> p
      }

--------------------------------------------------------------------------------
-- Fix Generation
--------------------------------------------------------------------------------

-- | Generate fixes from bang pattern analysis
generateBangPatternFixes :: BangPatternConfig -> BangPatternAnalysis -> [Fix]
generateBangPatternFixes config analysis =
  let candidates = bpaCandidates analysis
      strategy = bpcStrategy config
  in mapMaybe (candidateToFix strategy) candidates

-- | Convert a candidate to a fix
candidateToFix :: InsertionStrategy -> BangPatternCandidate -> Maybe Fix
candidateToFix strategy BangPatternCandidate{..} =
  case strategy of
    InsertInPattern -> Just Fix
      { fixTitle = "Insert bang pattern for '" <> bpcVarName <> "'"
      , fixEdits = [mkEdit bpcVarSpan ("!" <> bpcVarName)]
      , fixIsPreferred = True
      , fixAddImports = []
      , fixRemoveImports = []
      , fixCategory = FCPerformance
      , fixSafety = FSMostly
      }

    InsertInLet -> Just Fix
      { fixTitle = "Use strict let binding for '" <> bpcVarName <> "'"
      , fixEdits = [mkEdit bpcVarSpan ("!" <> bpcVarName)]
      , fixIsPreferred = True
      , fixAddImports = []
      , fixRemoveImports = []
      , fixCategory = FCPerformance
      , fixSafety = FSMostly
      }

    InsertSeq -> Just Fix
      { fixTitle = "Use seq for strict evaluation of '" <> bpcVarName <> "'"
      , fixEdits = [mkEdit bpcVarSpan (bpcVarName <> " `seq`")]
      , fixIsPreferred = False
      , fixAddImports = []
      , fixRemoveImports = []
      , fixCategory = FCPerformance
      , fixSafety = FSReview
      }

    InsertBangBefore -> Just Fix
      { fixTitle = "Use $! for strict application of '" <> bpcVarName <> "'"
      , fixEdits = [mkEdit bpcVarSpan ("$! " <> bpcVarName)]
      , fixIsPreferred = False
      , fixAddImports = []
      , fixRemoveImports = []
      , fixCategory = FCPerformance
      , fixSafety = FSReview
      }
  where
    mkEdit srcSpan newText = FixEdit
      { fixEditSpan = srcSpan
      , fixEditNewText = newText
      }

-- | Insert a bang pattern at a specific location
insertBangPattern :: FilePath -> SrcSpan -> Text -> Text -> Text
insertBangPattern _path targetSpan varName content =
  let linesArr = T.lines content
      targetLine = srcSpanStartLineRaw targetSpan
      targetCol = srcSpanStartColRaw targetSpan
  in if targetLine <= 0 || targetLine > length linesArr
     then content
     else
       let (before, targetAndAfter) = splitAt (targetLine - 1) linesArr
           (oldLine, after) = case targetAndAfter of
             [] -> ("", [])
             (l:ls) -> (l, ls)
           newLine = insertBangAt (targetCol - 1) varName oldLine
       in T.unlines (before ++ [newLine] ++ after)

-- | Insert bang at column position
insertBangAt :: Int -> Text -> Text -> Text
insertBangAt col varName line =
  let (before, rest) = T.splitAt col line
      -- Skip the variable name in rest
      afterVar = T.drop (T.length varName) rest
  in before <> "!" <> varName <> afterVar

-- | Insert multiple bang patterns
insertBangPatterns :: FilePath -> [(SrcSpan, Text)] -> Text -> Text
insertBangPatterns path insertions content =
  -- Apply in reverse order to preserve positions
  let sorted = sortBy (comparing (Down . srcSpanStartLine . fst)) insertions
  in foldl' (\c (sp, var) -> insertBangPattern path sp var c) content sorted

--------------------------------------------------------------------------------
-- Utility Functions
--------------------------------------------------------------------------------

-- | Check if something is in a strict context
isStrictContext :: [Text] -> Text -> Bool
isStrictContext patterns name =
  isAccumulatorName patterns name

-- | Check if a binding should be strict based on context
shouldBeStrict :: [Text] -> Text -> Text -> Bool
shouldBeStrict patterns name context =
  isAccumulatorName patterns name ||
  any (`T.isInfixOf` context) ["sum", "foldl'", "length", "count", "total"]

-- | Convert analysis to list of fixes
bangPatternAnalysisToFixes :: BangPatternConfig -> BangPatternAnalysis -> [Fix]
bangPatternAnalysisToFixes = generateBangPatternFixes
