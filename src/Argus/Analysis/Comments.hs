{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Analysis.Comments
-- Description : Comprehensive comment extraction and classification
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides proper comment handling for Argus, addressing the
-- previous line-based comment detection limitations.
--
-- == Features
--
-- * Multi-line block comment detection with proper nesting
-- * Classification of comment types (line, block, Haddock, pragma)
-- * String literal awareness (avoids false positives)
-- * Efficient span-based lookups
-- * Support for suppression directives in comments
--
-- == Comment Types
--
-- Argus recognizes several types of comments:
--
-- * __Line comments__: @-- comment text@
-- * __Block comments__: @{- comment text -}@ (including nested)
-- * __Haddock line__: @-- | documentation@ or @-- ^ documentation@
-- * __Haddock block__: @{-| documentation -}@ or @{-^ documentation -}@
-- * __Pragmas__: @{-# LANGUAGE ... #-}@
--
-- == Usage
--
-- @
-- let comments = extractComments sourceText
-- let index = buildCommentIndex comments
-- isInComment index (SrcSpan ... )  -- Check if span is in a comment
-- @
module Argus.Analysis.Comments
  ( -- * Comment Types
    CommentType (..)
  , CommentInfo (..)
  , CommentIndex

    -- * Comment Extraction
  , extractComments
  , extractCommentsWithStrings

    -- * Index Building
  , buildCommentIndex
  , emptyCommentIndex

    -- * Querying
  , isInComment
  , isInCommentType
  , isInAnyComment
  , getCommentAt
  , getCommentsInRange
  , getAllComments

    -- * Suppression Directives
  , extractSuppressions
  , Suppression (..)
  , SuppressionScope (..)

    -- * Position Types
  , Position (..)
  , Range (..)
  , positionToRange
  , rangeOverlaps
  , rangeContains

    -- * Configuration
  , CommentConfig (..)
  , defaultCommentConfig
  ) where

import Data.Aeson (ToJSON, FromJSON)
import Data.List (sortBy, foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe, listToMaybe)
import Data.Ord (comparing)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Argus.Types (Line(..), Column(..))

--------------------------------------------------------------------------------
-- Comment Types
--------------------------------------------------------------------------------

-- | Types of comments recognized by Argus.
data CommentType
  = LineComment        -- ^ Single-line comment: -- ...
  | BlockComment       -- ^ Multi-line block comment: {- ... -}
  | HaddockLine        -- ^ Haddock line comment: -- | or -- ^
  | HaddockBlock       -- ^ Haddock block comment: {-| or {-^
  | PragmaComment      -- ^ GHC pragma: {-# ... #-}
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Information about a single comment in the source.
data CommentInfo = CommentInfo
  { commentType      :: CommentType   -- ^ Type of comment
  , commentStartLine :: {-# UNPACK #-} !Int  -- ^ Starting line (1-indexed)
  , commentStartCol  :: {-# UNPACK #-} !Int  -- ^ Starting column (1-indexed)
  , commentEndLine   :: {-# UNPACK #-} !Int  -- ^ Ending line
  , commentEndCol    :: {-# UNPACK #-} !Int  -- ^ Ending column
  , commentContent   :: Text          -- ^ The comment text (excluding delimiters)
  , commentNesting   :: {-# UNPACK #-} !Int  -- ^ Nesting depth (for block comments)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Position in source file.
data Position = Position
  { posLine   :: {-# UNPACK #-} !Int  -- ^ Line number (1-indexed)
  , posColumn :: {-# UNPACK #-} !Int  -- ^ Column number (1-indexed)
  }
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A range in the source file.
data Range = Range
  { rangeStart :: Position
  , rangeEnd   :: Position
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Convert a comment to its range.
commentToRange :: CommentInfo -> Range
commentToRange CommentInfo{..} = Range
  { rangeStart = Position commentStartLine commentStartCol
  , rangeEnd = Position commentEndLine commentEndCol
  }

-- | Convert a position to a single-character range.
positionToRange :: Position -> Range
positionToRange pos = Range pos pos

-- | Check if two ranges overlap.
rangeOverlaps :: Range -> Range -> Bool
rangeOverlaps r1 r2 =
  not (rangeEnd r1 < rangeStart r2 || rangeEnd r2 < rangeStart r1)

-- | Check if first range contains second range.
rangeContains :: Range -> Range -> Bool
rangeContains outer inner =
  rangeStart outer <= rangeStart inner && rangeEnd inner <= rangeEnd outer

--------------------------------------------------------------------------------
-- Comment Index
--------------------------------------------------------------------------------

-- | Efficient index for comment lookups.
--
-- The index uses a two-level structure:
-- 1. Map from line number to comments that span that line
-- 2. Sorted list of comments for range queries
data CommentIndex = CommentIndex
  { ciLineIndex    :: Map Int [CommentInfo]  -- ^ Line -> comments on that line
  , ciAllComments  :: [CommentInfo]          -- ^ All comments sorted by position
  , ciStringRanges :: [Range]                -- ^ String literal ranges (to exclude)
  }
  deriving stock (Eq, Show)

-- | Empty comment index.
emptyCommentIndex :: CommentIndex
emptyCommentIndex = CommentIndex Map.empty [] []

-- | Build an efficient comment index from a list of comments.
buildCommentIndex :: [CommentInfo] -> CommentIndex
buildCommentIndex comments = CommentIndex
  { ciLineIndex = buildLineIndex sortedComments
  , ciAllComments = sortedComments
  , ciStringRanges = []  -- Can be populated separately
  }
  where
    sortedComments = sortBy (comparing commentStartLine <> comparing commentStartCol) comments

    buildLineIndex :: [CommentInfo] -> Map Int [CommentInfo]
    buildLineIndex = foldl' addToLine Map.empty

    addToLine :: Map Int [CommentInfo] -> CommentInfo -> Map Int [CommentInfo]
    addToLine idx ci =
      let lines' = [commentStartLine ci .. commentEndLine ci]
      in foldl' (\m l -> Map.insertWith (++) l [ci] m) idx lines'

--------------------------------------------------------------------------------
-- Comment Extraction
--------------------------------------------------------------------------------

-- | Configuration for comment extraction.
data CommentConfig = CommentConfig
  { ccTrackStrings    :: Bool  -- ^ Track string literals to avoid false positives
  , ccTrackPragmas    :: Bool  -- ^ Track pragma comments
  , ccTrackHaddock    :: Bool  -- ^ Distinguish Haddock comments
  , ccMaxNestingDepth :: Int   -- ^ Maximum block comment nesting depth
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default comment configuration.
defaultCommentConfig :: CommentConfig
defaultCommentConfig = CommentConfig
  { ccTrackStrings = True
  , ccTrackPragmas = True
  , ccTrackHaddock = True
  , ccMaxNestingDepth = 100
  }

-- | Extract all comments from source text.
extractComments :: Text -> [CommentInfo]
extractComments = extractCommentsWithStrings defaultCommentConfig

-- | Extract comments with string tracking for accuracy.
extractCommentsWithStrings :: CommentConfig -> Text -> [CommentInfo]
extractCommentsWithStrings config content =
  let lines' = T.lines content
      indexed = zip [1..] lines'
      (comments, _) = foldl' (processLine config) ([], initialState) indexed
  in reverse comments

-- | Parser state for tracking multi-line constructs.
data ParseState = ParseState
  { psInBlockComment :: Bool          -- ^ Currently inside block comment
  , psBlockNesting   :: Int           -- ^ Block comment nesting depth
  , psBlockStart     :: Maybe (Int, Int)  -- ^ Start position of current block
  , psBlockContent   :: Text          -- ^ Accumulated block content
  , psInString       :: Bool          -- ^ Currently inside string literal
  , psStringType     :: Maybe Char    -- ^ Type of string (" or ')
  }

initialState :: ParseState
initialState = ParseState
  { psInBlockComment = False
  , psBlockNesting = 0
  , psBlockStart = Nothing
  , psBlockContent = ""
  , psInString = False
  , psStringType = Nothing
  }

-- | Process a single line of source code.
processLine :: CommentConfig
            -> ([CommentInfo], ParseState)
            -> (Int, Text)
            -> ([CommentInfo], ParseState)
processLine config (comments, state) (lineNum, lineText) =
  if psInBlockComment state
    then processBlockContinuation config comments state lineNum lineText
    else processNewLine config comments state lineNum lineText

-- | Process a line that continues a block comment.
processBlockContinuation :: CommentConfig
                         -> [CommentInfo]
                         -> ParseState
                         -> Int
                         -> Text
                         -> ([CommentInfo], ParseState)
processBlockContinuation config comments state lineNum lineText =
  let (newComments, newState) = scanBlockComment config comments state lineNum lineText 1
  in (newComments, newState)

-- | Scan for block comment end in a line.
scanBlockComment :: CommentConfig
                 -> [CommentInfo]
                 -> ParseState
                 -> Int
                 -> Text
                 -> Int
                 -> ([CommentInfo], ParseState)
scanBlockComment config comments state lineNum lineText col
  | col > T.length lineText =
      -- End of line, block continues
      let newContent = psBlockContent state <> lineText <> "\n"
      in (comments, state { psBlockContent = newContent })
  | otherwise =
      let remaining = T.drop (col - 1) lineText
      in case T.uncons remaining of
           Nothing -> (comments, state { psBlockContent = psBlockContent state <> lineText <> "\n" })
           Just (c, rest) ->
             case (c, T.take 1 rest) of
               -- Nested block comment start
               ('{', "-") | not (T.isPrefixOf "#" (T.drop 1 rest)) ->
                 scanBlockComment config comments
                   (state { psBlockNesting = psBlockNesting state + 1 })
                   lineNum lineText (col + 2)

               -- Block comment end
               ('-', "}") ->
                 if psBlockNesting state > 1
                   then scanBlockComment config comments
                          (state { psBlockNesting = psBlockNesting state - 1 })
                          lineNum lineText (col + 2)
                   else
                     -- Close the block comment
                     let endCol = col + 1
                         Just (startLine, startCol) = psBlockStart state
                         commentContent' = psBlockContent state <> T.take (col - 1) lineText
                         commentType' = classifyBlockComment (psBlockContent state)
                         newComment = CommentInfo
                           { commentType = if ccTrackHaddock config then commentType' else BlockComment
                           , commentStartLine = startLine
                           , commentStartCol = startCol
                           , commentEndLine = lineNum
                           , commentEndCol = endCol
                           , commentContent = commentContent'
                           , commentNesting = 0
                           }
                         newState = state
                           { psInBlockComment = False
                           , psBlockNesting = 0
                           , psBlockStart = Nothing
                           , psBlockContent = ""
                           }
                         -- Continue scanning the rest of the line
                         restLine = T.drop (col + 1) lineText
                     in if T.null restLine
                          then (newComment : comments, newState)
                          else processNewLine config (newComment : comments) newState lineNum (T.replicate (col + 1) " " <> restLine)

               -- Continue scanning
               _ -> scanBlockComment config comments
                      (state { psBlockContent = psBlockContent state <> T.singleton c })
                      lineNum lineText (col + 1)

-- | Process a line that's not in a block comment.
processNewLine :: CommentConfig
               -> [CommentInfo]
               -> ParseState
               -> Int
               -> Text
               -> ([CommentInfo], ParseState)
processNewLine config comments state lineNum lineText =
  scanLine config comments state lineNum lineText 1 False

-- | Scan a line for comments, tracking string literals.
scanLine :: CommentConfig
         -> [CommentInfo]
         -> ParseState
         -> Int
         -> Text
         -> Int
         -> Bool  -- ^ In string
         -> ([CommentInfo], ParseState)
scanLine config comments state lineNum lineText col inStr
  | col > T.length lineText = (comments, state)
  | otherwise =
      let c = T.index lineText (col - 1)
          remaining = T.drop (col - 1) lineText
      in
        -- Handle string literals if tracking
        if ccTrackStrings config && (c == '"' || c == '\'') && not inStr
          then scanLine config comments state lineNum lineText (col + 1) True
        else if ccTrackStrings config && inStr
          then
            let c' = T.index lineText (col - 1)
            in if c' == '"' || c' == '\''
                 then scanLine config comments state lineNum lineText (col + 1) False
                 else if c' == '\\' && col < T.length lineText
                   then scanLine config comments state lineNum lineText (col + 2) True  -- Skip escaped char
                   else scanLine config comments state lineNum lineText (col + 1) True
        -- Not in string, check for comments
        else case T.take 3 remaining of
          -- Pragma comment
          "{-#" | ccTrackPragmas config ->
            let endIdx = findPragmaEnd remaining
                pragmaContent = T.take endIdx remaining
                newComment = CommentInfo
                  { commentType = PragmaComment
                  , commentStartLine = lineNum
                  , commentStartCol = col
                  , commentEndLine = lineNum
                  , commentEndCol = col + endIdx - 1
                  , commentContent = pragmaContent
                  , commentNesting = 0
                  }
            in scanLine config (newComment : comments) state lineNum lineText (col + endIdx) inStr
          _ -> case T.take 2 remaining of
            -- Block comment start
            "{-" ->
              let isHaddock = T.isPrefixOf "|" (T.drop 2 remaining) || T.isPrefixOf "^" (T.drop 2 remaining)
                  newState = state
                    { psInBlockComment = True
                    , psBlockNesting = 1
                    , psBlockStart = Just (lineNum, col)
                    , psBlockContent = ""
                    }
              in scanBlockComment config comments newState lineNum lineText (col + 2)

            -- Line comment
            "--" ->
              let commentContent' = T.drop 2 remaining
                  commentType' = if ccTrackHaddock config
                                   then classifyLineComment commentContent'
                                   else LineComment
                  newComment = CommentInfo
                    { commentType = commentType'
                    , commentStartLine = lineNum
                    , commentStartCol = col
                    , commentEndLine = lineNum
                    , commentEndCol = T.length lineText
                    , commentContent = T.strip commentContent'
                    , commentNesting = 0
                    }
              in (newComment : comments, state)

            -- No comment, continue
            _ -> scanLine config comments state lineNum lineText (col + 1) inStr

-- | Find the end of a pragma comment.
findPragmaEnd :: Text -> Int
findPragmaEnd txt =
  case T.breakOn "#-}" txt of
    (before, after) | not (T.null after) -> T.length before + 3
    _ -> T.length txt  -- Unclosed pragma

-- | Classify a line comment based on its content.
classifyLineComment :: Text -> CommentType
classifyLineComment content =
  case T.uncons (T.stripStart content) of
    Just ('|', _) -> HaddockLine
    Just ('^', _) -> HaddockLine
    _ -> LineComment

-- | Classify a block comment based on its content.
classifyBlockComment :: Text -> CommentType
classifyBlockComment content =
  case T.uncons (T.stripStart content) of
    Just ('|', _) -> HaddockBlock
    Just ('^', _) -> HaddockBlock
    _ -> BlockComment

--------------------------------------------------------------------------------
-- Querying
--------------------------------------------------------------------------------

-- | Check if a position is inside any comment.
isInComment :: CommentIndex -> Int -> Int -> Bool
isInComment idx line col = isInAnyComment idx (Position line col)

-- | Check if a position is inside a specific comment type.
isInCommentType :: CommentIndex -> Position -> CommentType -> Bool
isInCommentType idx pos ctype =
  any (\ci -> commentType ci == ctype && positionInComment pos ci)
      (getCommentsOnLine (ciLineIndex idx) (posLine pos))

-- | Check if a position is inside any comment.
isInAnyComment :: CommentIndex -> Position -> Bool
isInAnyComment idx pos =
  any (positionInComment pos) (getCommentsOnLine (ciLineIndex idx) (posLine pos))

-- | Check if a position is inside a specific comment.
positionInComment :: Position -> CommentInfo -> Bool
positionInComment (Position line col) CommentInfo{..} =
  (line > commentStartLine || (line == commentStartLine && col >= commentStartCol)) &&
  (line < commentEndLine || (line == commentEndLine && col <= commentEndCol))

-- | Get comments on a specific line.
getCommentsOnLine :: Map Int [CommentInfo] -> Int -> [CommentInfo]
getCommentsOnLine idx line = Map.findWithDefault [] line idx

-- | Get the comment at a specific position (if any).
getCommentAt :: CommentIndex -> Position -> Maybe CommentInfo
getCommentAt idx pos =
  listToMaybe $ filter (positionInComment pos) (getCommentsOnLine (ciLineIndex idx) (posLine pos))

-- | Get all comments in a range.
getCommentsInRange :: CommentIndex -> Range -> [CommentInfo]
getCommentsInRange idx range =
  filter (rangeOverlaps range . commentToRange) (ciAllComments idx)

-- | Get all comments in the index.
getAllComments :: CommentIndex -> [CommentInfo]
getAllComments = ciAllComments

--------------------------------------------------------------------------------
-- Suppression Directives
--------------------------------------------------------------------------------

-- | Scope of a suppression directive.
data SuppressionScope
  = SuppressLine       -- ^ Suppress on this line only
  | SuppressNextLine   -- ^ Suppress on the next line
  | SuppressBlock      -- ^ Suppress in a block (until end marker)
  | SuppressFile       -- ^ Suppress in the entire file
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A suppression directive found in a comment.
data Suppression = Suppression
  { suppressionScope   :: SuppressionScope
  , suppressionRules   :: Set Text        -- ^ Rule IDs to suppress (empty = all)
  , suppressionLine    :: Int             -- ^ Line where directive was found
  , suppressionMessage :: Maybe Text      -- ^ Optional justification
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Extract suppression directives from comments.
--
-- Recognized formats:
-- * @argus:ignore@ - ignore all rules on this line
-- * @argus:ignore rule-id@ - ignore specific rule
-- * @argus:ignore-next-line@ - ignore all on next line
-- * @argus:ignore-file@ - ignore in entire file
extractSuppressions :: [CommentInfo] -> [Suppression]
extractSuppressions = mapMaybe parseSuppressionDirective

-- | Parse a single comment for suppression directives.
parseSuppressionDirective :: CommentInfo -> Maybe Suppression
parseSuppressionDirective ci =
  let content = T.toLower $ T.strip $ commentContent ci
  in if "argus:ignore" `T.isPrefixOf` content ||
        "argus-ignore" `T.isPrefixOf` content ||
        "lint:ignore" `T.isPrefixOf` content ||
        "hlint:ignore" `T.isPrefixOf` content  -- Compatibility
     then Just $ parseDirective (commentStartLine ci) content
     else Nothing

-- | Parse the content of a suppression directive.
parseDirective :: Int -> Text -> Suppression
parseDirective line content =
  let parts = T.words content
      (scope, rulesPart) = case parts of
        (_:"-file":rest) -> (SuppressFile, rest)
        (_:"-next-line":rest) -> (SuppressNextLine, rest)
        (_:"-block":rest) -> (SuppressBlock, rest)
        (_:rest) -> (SuppressLine, rest)
        [] -> (SuppressLine, [])
      rules = Set.fromList $ filter (not . T.isPrefixOf "--") rulesPart
  in Suppression
       { suppressionScope = scope
       , suppressionRules = rules
       , suppressionLine = line
       , suppressionMessage = Nothing
       }
