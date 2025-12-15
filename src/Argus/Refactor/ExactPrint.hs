{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.Refactor.ExactPrint
-- Description : Format-preserving code transformations
-- Copyright   : (c) 2024
-- License     : MIT
-- Stability   : stable
-- Portability : GHC
--
-- = Overview
--
-- This module provides format-preserving code transformations.
-- It ensures that modifications to the AST preserve the original
-- formatting, comments, and whitespace as much as possible.
--
-- = Architecture
--
-- The module uses a text-based approach for applying edits, which is
-- simpler and more robust than AST-based transformations. Edits are
-- sorted and applied in reverse order (end-to-start) so that earlier
-- edits don't affect the positions of later ones.
--
-- @
-- Original Text → Sort Edits (reverse) → Apply Each Edit → Result Text
-- @
--
-- = Key Functions
--
-- * 'applyFix': Apply a single fix to source text
-- * 'applyEdits': Apply multiple edits to source text
-- * 'replaceExpr': Replace text at a source span
-- * 'insertImport': Insert an import statement
--
-- = Thread Safety
--
-- All functions in this module are pure and thread-safe.
--
-- @since 1.0.0
module Argus.Refactor.ExactPrint
  ( -- * Transformation context
    TransformT
  , TransformState (..)
  , runTransform
  , runTransformIO

    -- * Basic operations
  , replaceExpr
  , replaceType
  , replaceName
  , insertImport
  , removeDecl

    -- * Source manipulation
  , applyEdits
  , applyFix
  , applyFixes

    -- * Utilities
  , parseAndTransform
  , printExactly
  ) where

import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.State.Strict (StateT, runStateT, modify)
import Data.ByteString qualified as BS
import Data.Functor.Identity (Identity, runIdentity)
import Data.List (sortBy)
import Data.Ord (comparing, Down (..))
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE

import Argus.Types

--------------------------------------------------------------------------------
-- Transformation Monad
--------------------------------------------------------------------------------

-- | Transformation state (text-based, no AST required for basic edits)
data TransformState = TransformState
  { tsSource     :: Text
  , tsModified   :: Bool
  }

-- | Transformation monad
type TransformT m = StateT TransformState m

-- | Run a transformation
runTransform :: TransformT Identity a -> TransformState -> (a, TransformState)
runTransform t s = runIdentity $ runStateT t s

-- | Run a transformation in IO
runTransformIO :: TransformT m a -> TransformState -> m (a, TransformState)
runTransformIO = runStateT

--------------------------------------------------------------------------------
-- Basic Operations
--------------------------------------------------------------------------------

-- | Replace an expression at a given location
replaceExpr :: Monad m => SrcSpan -> Text -> TransformT m ()
replaceExpr srcSpan newText = modify $ \s ->
  s { tsSource = replaceInSource (tsSource s) srcSpan newText
    , tsModified = True
    }

-- | Replace a type at a given location
replaceType :: Monad m => SrcSpan -> Text -> TransformT m ()
replaceType = replaceExpr  -- Same implementation for now

-- | Replace a name at a given location
replaceName :: Monad m => SrcSpan -> Text -> TransformT m ()
replaceName = replaceExpr  -- Same implementation for now

-- | Insert an import
insertImport :: Monad m => Text -> TransformT m ()
insertImport importText = modify $ \s ->
  s { tsSource = insertImportInSource (tsSource s) importText
    , tsModified = True
    }

-- | Remove a declaration
removeDecl :: Monad m => SrcSpan -> TransformT m ()
removeDecl srcSpan = modify $ \s ->
  s { tsSource = removeFromSource (tsSource s) srcSpan
    , tsModified = True
    }

--------------------------------------------------------------------------------
-- Source Manipulation
--------------------------------------------------------------------------------

-- | Apply a list of edits to source text
applyEdits :: Text -> [FixEdit] -> Text
applyEdits source edits =
  -- Sort edits by position, in reverse order (end to start)
  -- so that earlier edits don't affect positions of later ones
  let sortedEdits = sortBy (comparing (Down . srcSpanStartLine . fixEditSpan)) edits
  in foldl' applyEdit source sortedEdits

-- | Apply a single edit
applyEdit :: Text -> FixEdit -> Text
applyEdit source (FixEdit srcSpan newText) =
  replaceInSource source srcSpan newText

-- | Apply a fix to source text
applyFix :: Text -> Fix -> Text
applyFix source fix = applyEdits source (fixEdits fix)

-- | Apply multiple fixes
applyFixes :: Text -> [Fix] -> Text
applyFixes = foldl' applyFix

-- | Replace text at a source span
replaceInSource :: Text -> SrcSpan -> Text -> Text
replaceInSource source srcSpan newText =
  let sourceLines = T.lines source
      startLineRaw = srcSpanStartLineRaw srcSpan
      endLineRaw = srcSpanEndLineRaw srcSpan
      startColRaw = srcSpanStartColRaw srcSpan
      endColRaw = srcSpanEndColRaw srcSpan

      beforeLines = take (startLineRaw - 1) sourceLines
      afterLines = drop endLineRaw sourceLines

      -- Handle the lines that are partially affected
      startLine = if startLineRaw > 0 && startLineRaw <= length sourceLines
                  then sourceLines !! (startLineRaw - 1)
                  else ""
      endLine = if endLineRaw > 0 && endLineRaw <= length sourceLines
                then sourceLines !! (endLineRaw - 1)
                else ""

      beforeCol = T.take (startColRaw - 1) startLine
      afterCol = T.drop (endColRaw - 1) endLine

      -- Construct the new content
      newLines = T.lines (beforeCol <> newText <> afterCol)
      resultLines = beforeLines ++ newLines ++ afterLines

  in T.intercalate "\n" resultLines

-- | Insert an import statement in the source
insertImportInSource :: Text -> Text -> Text
insertImportInSource source importText =
  let sourceLines = T.lines source
      -- Find where imports end (after module decl, before first non-import decl)
      importEnd = findImportInsertPoint sourceLines
      (before, after) = splitAt importEnd sourceLines
  in T.intercalate "\n" (before ++ [importText] ++ after)

-- | Find the right place to insert an import
findImportInsertPoint :: [Text] -> Int
findImportInsertPoint ls = go 0 ls False
  where
    go n [] _ = n
    go n (l:rest) seenModule
      | "module " `T.isPrefixOf` T.stripStart l = go (n + 1) rest True
      | "import " `T.isPrefixOf` T.stripStart l = go (n + 1) rest seenModule
      | seenModule && not (T.null (T.stripStart l)) && not ("import " `T.isPrefixOf` T.stripStart l) = n
      | otherwise = go (n + 1) rest seenModule

-- | Remove text at a source span
removeFromSource :: Text -> SrcSpan -> Text
removeFromSource source srcSpan = replaceInSource source srcSpan ""

--------------------------------------------------------------------------------
-- Parsing and Printing
--------------------------------------------------------------------------------

-- | Read a file and apply transformations
parseAndTransform :: MonadIO m => FilePath -> (TransformState -> m TransformState) -> m (Either Text Text)
parseAndTransform path transform = do
  content <- liftIO $ TE.decodeUtf8 <$> BS.readFile path
  let initialState = TransformState
        { tsSource = content
        , tsModified = False
        }
  finalState <- transform initialState
  pure $ Right $ tsSource finalState

-- | Get the final source text
printExactly :: TransformState -> Text
printExactly = tsSource
