{-# LANGUAGE StrictData #-}

-- |
-- Module      : Linter.Refactor.Substitution
-- Description : Safe AST substitution
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides safe substitution of AST nodes, handling
-- variable capture, scope, and other edge cases.
module Linter.Refactor.Substitution
  ( -- * Substitution
    substitute
  , substituteExpr
  , substituteType
  , substituteName

    -- * Scope handling
  , Scope (..)
  , emptyScope
  , inScope
  , addBinding

    -- * Capture avoidance
  , freshenName
  , avoidCapture
  ) where

import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T

import Linter.Rules.Types
import Linter.Types
import Linter.Utils qualified as Utils

--------------------------------------------------------------------------------
-- Scope
--------------------------------------------------------------------------------

-- | Variable scope for substitution
data Scope = Scope
  { scopeBindings :: Set Text     -- ^ Currently bound names
  , scopeFree     :: Set Text     -- ^ Free variables in replacement
  , scopeCounter  :: Int          -- ^ Counter for fresh names
  }
  deriving stock (Eq, Show)

-- | Empty scope
emptyScope :: Scope
emptyScope = Scope Set.empty Set.empty 0

-- | Check if a name is in scope
inScope :: Text -> Scope -> Bool
inScope name = Set.member name . scopeBindings

-- | Add a binding to scope
addBinding :: Text -> Scope -> Scope
addBinding name s = s { scopeBindings = Set.insert name (scopeBindings s) }

--------------------------------------------------------------------------------
-- Substitution
--------------------------------------------------------------------------------

-- | Substitute bindings into a pattern
substitute :: Bindings -> Pattern -> Text
substitute bindings = go
  where
    go pat = case pat of
      PWildcard -> "_"
      PVar pv -> case Map.lookup (pvName pv) bindings of
        Just b  -> bindingValue b
        Nothing -> pvName pv
      PLiteral t -> t
      PConstructor name pats ->
        name <> " " <> T.intercalate " " (map go pats)
      PApplication f x ->
        wrapIfNeeded (go f) <> " " <> wrapIfNeeded (go x)
      PInfix l op r ->
        go l <> " " <> op <> " " <> go r
      PTuple pats ->
        "(" <> T.intercalate ", " (map go pats) <> ")"
      PList pats ->
        "[" <> T.intercalate ", " (map go pats) <> "]"
      PParens p ->
        "(" <> go p <> ")"
      PTyped p _ ->
        go p

    wrapIfNeeded t
      | T.any (== ' ') t = "(" <> t <> ")"
      | otherwise = t

-- | Substitute in an expression with word-boundary awareness
-- This properly handles identifiers so "head" doesn't replace "headMay"
-- Uses replaceWordBoundaryPreserve to skip over strings and comments
substituteExpr :: Map Text Text -> Text -> Text
substituteExpr subs expr = foldr replaceOne expr (Map.toList subs)
  where
    replaceOne (from, to) text = Utils.replaceWordBoundaryPreserve from to text

-- | Substitute in a type (uses same word-boundary logic)
substituteType :: Map Text Text -> Text -> Text
substituteType = substituteExpr

-- | Substitute a name (exact match only)
substituteName :: Text -> Text -> Text -> Text
substituteName from to text =
  if text == from then to else text

--------------------------------------------------------------------------------
-- Capture Avoidance
--------------------------------------------------------------------------------

-- | Generate a fresh name to avoid capture
freshenName :: Text -> Scope -> (Text, Scope)
freshenName base s =
  let newName = base <> T.pack (show (scopeCounter s))
      newScope = s { scopeCounter = scopeCounter s + 1
                   , scopeBindings = Set.insert newName (scopeBindings s)
                   }
  in (newName, newScope)

-- | Avoid variable capture during substitution
avoidCapture :: Set Text -> Set Text -> Bindings -> (Bindings, Map Text Text)
avoidCapture bound free bindings =
  let captured = Set.intersection bound free
      renames = foldr mkRename Map.empty (Set.toList captured)
      newBindings = Map.map (renameBinding renames) bindings
  in (newBindings, renames)
  where
    mkRename name acc =
      let newName = findFreshName name (Set.union bound free)
      in Map.insert name newName acc

    renameBinding renames b =
      let newValue = foldr (\(from, to) t -> T.replace from to t)
                           (bindingValue b)
                           (Map.toList renames)
      in b { bindingValue = newValue }

    findFreshName base existing = go 0
      where
        go n =
          let candidate = base <> T.pack (show n)
          in if Set.member candidate existing
             then go (n + 1)
             else candidate
