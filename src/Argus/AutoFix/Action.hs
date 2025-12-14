{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Argus.AutoFix.Action
-- Description : AST and compiler for fix actions
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides a domain-specific language for expressing fix
-- transformations as an abstract syntax tree (AST). The AST can be:
--
-- * Analyzed for safety before execution
-- * Composed and transformed
-- * Compiled into actual text transformations
-- * Serialized for display or storage
--
-- == Architecture
--
-- The 'FixAction' type represents atomic fix operations. These can be
-- combined using 'FixSequence' for sequential operations or 'FixChoice'
-- for alternatives. The 'compileAction' function transforms actions
-- into executable text transformations.
--
-- == Usage
--
-- @
-- -- Define a fix action
-- let action = Replace
--       { raSpan = SrcSpan 1 1 1 10
--       , raOldText = "head xs"
--       , raNewText = "listToMaybe xs"
--       }
--
-- -- Compile and apply
-- case compileAction action content of
--   Right newContent -> putStrLn newContent
--   Left err -> putStrLn $ "Error: " <> err
-- @
module Argus.AutoFix.Action
  ( -- * Core Action Types
    FixAction (..)
  , ActionSpan (..)
  , spanFromSrcSpan
  , spanToSrcSpan

    -- * Compound Actions
  , FixSequence (..)
  , FixChoice (..)
  , FixConditional (..)
  , ActionCondition (..)

    -- * Action Metadata
  , ActionMeta (..)
  , defaultActionMeta
  , withDescription
  , withSafety
  , withReversible

    -- * Action Compilation
  , compileAction
  , compileSequence
  , compileChoice
  , ActionResult (..)
  , actionResultContent
  , actionResultDelta
  , CompileError (..)

    -- * Action Accessors
  , actionSpan
  , actionMeta

    -- * Action Analysis
  , analyzeAction
  , ActionAnalysis (..)
  , checkOverlaps
  , sortActions

    -- * Action Builders
  , replace
  , insert
  , delete
  , insertBefore
  , insertAfter
  , wrapWith
  , unwrap

    -- * Text Operations
  , TextOp (..)
  , applyTextOp
  , composeTextOps
  ) where

import Data.Aeson (ToJSON (..), FromJSON (..))
import Data.List (sortBy)
import Data.Ord (comparing)
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Argus.Types (SrcSpan (..), Line (..), Column (..))
import Argus.Rules.Types (SafetyLevel (..))

--------------------------------------------------------------------------------
-- Core Action Types
--------------------------------------------------------------------------------

-- | A span within text, using 0-based byte offsets for efficiency
data ActionSpan = ActionSpan
  { asStartOffset :: Int
    -- ^ Start byte offset (0-based, inclusive)
  , asEndOffset   :: Int
    -- ^ End byte offset (0-based, exclusive)
  , asStartLine   :: Int
    -- ^ Start line (1-based, for display)
  , asStartCol    :: Int
    -- ^ Start column (1-based, for display)
  , asEndLine     :: Int
    -- ^ End line (1-based, for display)
  , asEndCol      :: Int
    -- ^ End column (1-based, for display)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Convert a SrcSpan to an ActionSpan given file content
spanFromSrcSpan :: Text -> SrcSpan -> ActionSpan
spanFromSrcSpan content SrcSpan{..} =
  let startOffset = lineColToOffset content (unLine srcSpanStartLine) (unColumn srcSpanStartCol)
      endOffset = lineColToOffset content (unLine srcSpanEndLine) (unColumn srcSpanEndCol)
  in ActionSpan
    { asStartOffset = startOffset
    , asEndOffset = endOffset
    , asStartLine = unLine srcSpanStartLine
    , asStartCol = unColumn srcSpanStartCol
    , asEndLine = unLine srcSpanEndLine
    , asEndCol = unColumn srcSpanEndCol
    }

-- | Convert an ActionSpan back to a SrcSpan
spanToSrcSpan :: FilePath -> ActionSpan -> SrcSpan
spanToSrcSpan path ActionSpan{..} = SrcSpan
  { srcSpanFile = path
  , srcSpanStartLine = Line asStartLine
  , srcSpanStartCol = Column asStartCol
  , srcSpanEndLine = Line asEndLine
  , srcSpanEndCol = Column asEndCol
  }

-- | Convert line/column to byte offset
lineColToOffset :: Text -> Int -> Int -> Int
lineColToOffset content line col =
  let linesList = T.lines content
      precedingLines = take (line - 1) linesList
      lineOffset = sum (map ((+1) . T.length) precedingLines)  -- +1 for newlines
  in lineOffset + col - 1

-- | An atomic fix action
data FixAction
  = Replace ActionSpan Text Text ActionMeta
    -- ^ Replace span: old text, new text, metadata
  | Insert Int Text ActionMeta
    -- ^ Insert: offset, text to insert, metadata
  | Delete ActionSpan Text ActionMeta
    -- ^ Delete span: expected text, metadata
  | InsertLine Int Text ActionMeta
    -- ^ Insert line: line number (1-based), text, metadata
  | DeleteLine Int ActionMeta
    -- ^ Delete line: line number (1-based), metadata
  | Indent ActionSpan Int ActionMeta
    -- ^ Indent span: amount (negative to dedent), metadata
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Get the span of an action (if applicable)
actionSpan :: FixAction -> Maybe ActionSpan
actionSpan (Replace span' _ _ _) = Just span'
actionSpan (Insert _ _ _) = Nothing
actionSpan (Delete span' _ _) = Just span'
actionSpan (InsertLine _ _ _) = Nothing
actionSpan (DeleteLine _ _) = Nothing
actionSpan (Indent span' _ _) = Just span'

-- | Get metadata from an action
actionMeta :: FixAction -> ActionMeta
actionMeta (Replace _ _ _ m) = m
actionMeta (Insert _ _ m) = m
actionMeta (Delete _ _ m) = m
actionMeta (InsertLine _ _ m) = m
actionMeta (DeleteLine _ m) = m
actionMeta (Indent _ _ m) = m

--------------------------------------------------------------------------------
-- Compound Actions
--------------------------------------------------------------------------------

-- | A sequence of actions to be applied in order
data FixSequence = FixSequence
  { fsActions :: [FixAction]
    -- ^ Actions in application order
  , fsDescription :: Text
    -- ^ Human-readable description
  , fsStopOnError :: Bool
    -- ^ Stop if any action fails
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A choice between alternative actions
data FixChoice = FixChoice
  { fcAlternatives :: [FixAction]
    -- ^ Alternative actions (try in order)
  , fcDescription  :: Text
    -- ^ Human-readable description
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A conditional action
data FixConditional = FixConditional
  { fcCondition :: ActionCondition
    -- ^ Condition to check
  , fcThenAction :: FixAction
    -- ^ Action if condition is true
  , fcElseAction :: Maybe FixAction
    -- ^ Optional action if condition is false
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Conditions for conditional actions
data ActionCondition
  = TextMatches ActionSpan Text
    -- ^ Text at span matches exactly
  | TextContains Text
    -- ^ Content contains text anywhere
  | LineContains Int Text
    -- ^ Line contains text
  | SpanEmpty ActionSpan
    -- ^ Span contains only whitespace
  | CondAnd ActionCondition ActionCondition
    -- ^ Both conditions must hold
  | CondOr ActionCondition ActionCondition
    -- ^ Either condition must hold
  | CondNot ActionCondition
    -- ^ Negation
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Action Metadata
--------------------------------------------------------------------------------

-- | Metadata for an action
data ActionMeta = ActionMeta
  { amDescription :: Text
    -- ^ Human-readable description
  , amSafety      :: SafetyLevel
    -- ^ Safety level of this action
  , amReversible  :: Bool
    -- ^ Can this action be undone
  , amPreserveFormatting :: Bool
    -- ^ Should preserve surrounding formatting
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default action metadata
defaultActionMeta :: ActionMeta
defaultActionMeta = ActionMeta
  { amDescription = ""
  , amSafety = NeedsReview
  , amReversible = True
  , amPreserveFormatting = True
  }

-- | Set description in metadata
withDescription :: Text -> ActionMeta -> ActionMeta
withDescription desc meta = meta { amDescription = desc }

-- | Set safety level in metadata
withSafety :: SafetyLevel -> ActionMeta -> ActionMeta
withSafety safety meta = meta { amSafety = safety }

-- | Set reversibility in metadata
withReversible :: Bool -> ActionMeta -> ActionMeta
withReversible rev meta = meta { amReversible = rev }

--------------------------------------------------------------------------------
-- Action Compilation
--------------------------------------------------------------------------------

-- | Result of compiling and applying an action
data ActionResult
  = ActionSuccess Text Int
    -- ^ Success: new content, delta (change in length)
  | ActionVerificationFailed Text Text
    -- ^ Verification failed: expected text, actual text
  | ActionOutOfBounds Int Int
    -- ^ Out of bounds: invalid offset, content length
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Get new content from a successful action result
actionResultContent :: ActionResult -> Maybe Text
actionResultContent (ActionSuccess c _) = Just c
actionResultContent _ = Nothing

-- | Get delta from a successful action result
actionResultDelta :: ActionResult -> Maybe Int
actionResultDelta (ActionSuccess _ d) = Just d
actionResultDelta _ = Nothing

-- | Errors during compilation
data CompileError
  = VerificationFailed Text Text  -- ^ Expected, Actual
  | OutOfBounds Int Int           -- ^ Offset, Length
  | OverlappingActions ActionSpan ActionSpan
  | EmptySequence
  | NoValidAlternative
  | ConditionFailed Text
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Compile and apply a single action
compileAction :: FixAction -> Text -> Either CompileError ActionResult
compileAction action content = case action of
  Replace raSpan raOldText raNewText _ ->
    let start = asStartOffset raSpan
        end = asEndOffset raSpan
        len = T.length content
    in if start < 0 || end > len || start > end
         then Left $ OutOfBounds start len
         else
           let actual = T.take (end - start) (T.drop start content)
           in if actual /= raOldText
                then Left $ VerificationFailed raOldText actual
                else
                  let before = T.take start content
                      after = T.drop end content
                      newContent = before <> raNewText <> after
                      theDelta = T.length raNewText - T.length raOldText
                  in Right $ ActionSuccess newContent theDelta

  Insert iaOffset iaText _ ->
    let len = T.length content
    in if iaOffset < 0 || iaOffset > len
         then Left $ OutOfBounds iaOffset len
         else
           let before = T.take iaOffset content
               after = T.drop iaOffset content
               newContent = before <> iaText <> after
           in Right $ ActionSuccess newContent (T.length iaText)

  Delete daSpan daExpectedText _ ->
    let start = asStartOffset daSpan
        end = asEndOffset daSpan
        len = T.length content
    in if start < 0 || end > len || start > end
         then Left $ OutOfBounds start len
         else
           let actual = T.take (end - start) (T.drop start content)
           in if actual /= daExpectedText
                then Left $ VerificationFailed daExpectedText actual
                else
                  let before = T.take start content
                      after = T.drop end content
                      newContent = before <> after
                  in Right $ ActionSuccess newContent (negate (T.length daExpectedText))

  InsertLine ilLineNum ilText _ ->
    let linesList = T.lines content
        numLines = length linesList
    in if ilLineNum < 0 || ilLineNum > numLines
         then Left $ OutOfBounds ilLineNum numLines
         else
           let (before, after) = splitAt ilLineNum linesList
               newLines = before ++ [ilText] ++ after
               newContent = T.intercalate "\n" newLines
           in Right $ ActionSuccess newContent (T.length ilText + 1)

  DeleteLine dlLineNum _ ->
    let linesList = T.lines content
        numLines = length linesList
    in if dlLineNum < 1 || dlLineNum > numLines
         then Left $ OutOfBounds dlLineNum numLines
         else
           let (before, rest) = splitAt (dlLineNum - 1) linesList
               after = drop 1 rest
               deletedLen = case rest of
                 (l:_) -> T.length l + 1
                 []    -> 0
               newContent = T.intercalate "\n" (before ++ after)
           in Right $ ActionSuccess newContent (negate deletedLen)

  Indent idSpan idAmount _ ->
    let start = asStartOffset idSpan
        end = asEndOffset idSpan
        len = T.length content
    in if start < 0 || end > len || start > end
         then Left $ OutOfBounds start len
         else
           let textToIndent = T.take (end - start) (T.drop start content)
               indented = indentText idAmount textToIndent
               before = T.take start content
               after = T.drop end content
               newContent = before <> indented <> after
           in Right $ ActionSuccess newContent (T.length indented - T.length textToIndent)

-- | Indent/dedent text by given amount
indentText :: Int -> Text -> Text
indentText amount text
  | amount >= 0 =
      let indent = T.replicate amount " "
      in T.intercalate "\n" $ map (indent <>) (T.lines text)
  | otherwise =
      let dedent = abs amount
      in T.intercalate "\n" $ map (T.drop dedent) (T.lines text)

-- | Compile and apply a sequence of actions
compileSequence :: FixSequence -> Text -> Either CompileError Text
compileSequence FixSequence{..} content
  | null fsActions = Left EmptySequence
  | otherwise = applyActions fsActions content 0
  where
    applyActions :: [FixAction] -> Text -> Int -> Either CompileError Text
    applyActions [] c _ = Right c
    applyActions (a:as) c theDelta = do
      let adjusted = adjustAction theDelta a
      case compileAction adjusted c of
        Left err
          | fsStopOnError -> Left err
          | otherwise -> applyActions as c theDelta
        Right (ActionSuccess newContent resultDelta) ->
          applyActions as newContent (theDelta + resultDelta)
        Right (ActionVerificationFailed expected actual) ->
          if fsStopOnError
            then Left $ VerificationFailed expected actual
            else applyActions as c theDelta
        Right (ActionOutOfBounds offset len) ->
          if fsStopOnError
            then Left $ OutOfBounds offset len
            else applyActions as c theDelta

-- | Adjust action offsets by delta
adjustAction :: Int -> FixAction -> FixAction
adjustAction delta = \case
  Replace span' old new meta ->
    Replace (adjustSpan delta span') old new meta
  Insert offset text meta ->
    Insert (offset + delta) text meta
  Delete span' expected meta ->
    Delete (adjustSpan delta span') expected meta
  InsertLine lineNum text meta ->
    InsertLine lineNum text meta  -- Line-based, no adjustment
  DeleteLine lineNum meta ->
    DeleteLine lineNum meta  -- Line-based, no adjustment
  Indent span' amount meta ->
    Indent (adjustSpan delta span') amount meta

-- | Adjust span by delta
adjustSpan :: Int -> ActionSpan -> ActionSpan
adjustSpan delta span' = span'
  { asStartOffset = asStartOffset span' + delta
  , asEndOffset = asEndOffset span' + delta
  }

-- | Compile a choice by trying alternatives in order
compileChoice :: FixChoice -> Text -> Either CompileError Text
compileChoice FixChoice{..} content = tryAlternatives fcAlternatives
  where
    tryAlternatives [] = Left NoValidAlternative
    tryAlternatives (a:as) = case compileAction a content of
      Right (ActionSuccess newContent _) -> Right newContent
      _ -> tryAlternatives as

--------------------------------------------------------------------------------
-- Action Analysis
--------------------------------------------------------------------------------

-- | Analysis results for an action
data ActionAnalysis = ActionAnalysis
  { aaSpansAffected :: [ActionSpan]
    -- ^ All spans that will be modified
  , aaOverlaps      :: [(ActionSpan, ActionSpan)]
    -- ^ Pairs of overlapping spans
  , aaSafetyLevel   :: SafetyLevel
    -- ^ Overall safety level
  , aaIsReversible  :: Bool
    -- ^ Can all actions be reversed
  , aaEstimatedDelta :: Int
    -- ^ Estimated change in content length
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Analyze an action or sequence
analyzeAction :: FixAction -> ActionAnalysis
analyzeAction action = ActionAnalysis
  { aaSpansAffected = maybeToList (actionSpan action)
  , aaOverlaps = []
  , aaSafetyLevel = amSafety (actionMeta action)
  , aaIsReversible = amReversible (actionMeta action)
  , aaEstimatedDelta = estimateDelta action
  }
  where
    maybeToList Nothing = []
    maybeToList (Just x) = [x]

    estimateDelta (Replace _ old new _) =
      T.length new - T.length old
    estimateDelta (Insert _ text _) = T.length text
    estimateDelta (Delete _ expected _) = negate (T.length expected)
    estimateDelta (InsertLine _ text _) = T.length text + 1
    estimateDelta (DeleteLine _ _) = -1  -- Approximate
    estimateDelta (Indent _ _ _) = 0     -- Varies by content

-- | Check for overlapping actions
checkOverlaps :: [FixAction] -> [(FixAction, FixAction)]
checkOverlaps actions =
  let withSpans = [(a, actionSpan a) | a <- actions]
  in [(a1, a2) | (a1, Just s1) <- withSpans
               , (a2, Just s2) <- withSpans
               , a1 /= a2
               , spansOverlap s1 s2]
  where
    spansOverlap s1 s2 =
      asStartOffset s1 < asEndOffset s2 &&
      asEndOffset s1 > asStartOffset s2

-- | Sort actions by span position (for proper sequential application)
sortActions :: [FixAction] -> [FixAction]
sortActions = sortBy (comparing getOffset)
  where
    getOffset (Replace span' _ _ _) = asStartOffset span'
    getOffset (Insert offset _ _) = offset
    getOffset (Delete span' _ _) = asStartOffset span'
    getOffset (InsertLine lineNum _ _) = lineNum * 1000000  -- Approximate
    getOffset (DeleteLine lineNum _) = lineNum * 1000000
    getOffset (Indent span' _ _) = asStartOffset span'

--------------------------------------------------------------------------------
-- Action Builders
--------------------------------------------------------------------------------

-- | Create a replace action
replace :: ActionSpan -> Text -> Text -> FixAction
replace span' old new = Replace span' old new defaultActionMeta

-- | Create an insert action
insert :: Int -> Text -> FixAction
insert offset text = Insert offset text defaultActionMeta

-- | Create a delete action
delete :: ActionSpan -> Text -> FixAction
delete span' expected = Delete span' expected defaultActionMeta

-- | Insert before a span
insertBefore :: ActionSpan -> Text -> FixAction
insertBefore span' text = Insert (asStartOffset span') text defaultActionMeta

-- | Insert after a span
insertAfter :: ActionSpan -> Text -> FixAction
insertAfter span' text = Insert (asEndOffset span') text defaultActionMeta

-- | Wrap text in a span with prefix and suffix
wrapWith :: ActionSpan -> Text -> Text -> Text -> FixAction
wrapWith span' old prefix suffix =
  Replace span' old (prefix <> old <> suffix) defaultActionMeta

-- | Unwrap text by removing prefix and suffix
unwrap :: ActionSpan -> Text -> Text -> Text -> FixAction
unwrap span' wrapped prefix suffix =
  Replace span' wrapped (T.drop (T.length prefix) $ T.dropEnd (T.length suffix) wrapped) defaultActionMeta

--------------------------------------------------------------------------------
-- Text Operations
--------------------------------------------------------------------------------

-- | Low-level text operation
data TextOp
  = OpReplace Int Int Text  -- ^ Replace from offset to offset with text
  | OpInsert Int Text       -- ^ Insert at offset
  | OpDelete Int Int        -- ^ Delete from offset to offset
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Apply a text operation
applyTextOp :: TextOp -> Text -> Either Text Text
applyTextOp op content = case op of
  OpReplace start end new
    | start < 0 || end > T.length content || start > end ->
        Left "Invalid range for replace"
    | otherwise ->
        Right $ T.take start content <> new <> T.drop end content

  OpInsert offset text
    | offset < 0 || offset > T.length content ->
        Left "Invalid offset for insert"
    | otherwise ->
        Right $ T.take offset content <> text <> T.drop offset content

  OpDelete start end
    | start < 0 || end > T.length content || start > end ->
        Left "Invalid range for delete"
    | otherwise ->
        Right $ T.take start content <> T.drop end content

-- | Compose multiple text operations (adjusting offsets)
composeTextOps :: [TextOp] -> Text -> Either Text Text
composeTextOps ops content = foldlM applyWithDelta (content, 0) ops >>= \(c, _) -> Right c
  where
    foldlM :: (Monad m) => (b -> a -> m b) -> b -> [a] -> m b
    foldlM _ z [] = pure z
    foldlM f z (x:xs) = f z x >>= \z' -> foldlM f z' xs

    applyWithDelta (c, delta) op = do
      let adjusted = adjustOp delta op
      newC <- applyTextOp adjusted c
      let newDelta = delta + opDelta adjusted (T.length c)
      Right (newC, newDelta)

    adjustOp delta (OpReplace s e t) = OpReplace (s + delta) (e + delta) t
    adjustOp delta (OpInsert o t) = OpInsert (o + delta) t
    adjustOp delta (OpDelete s e) = OpDelete (s + delta) (e + delta)

    opDelta (OpReplace s e t) _ = T.length t - (e - s)
    opDelta (OpInsert _ t) _ = T.length t
    opDelta (OpDelete s e) _ = s - e
