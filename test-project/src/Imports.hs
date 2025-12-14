{-# LANGUAGE OverloadedStrings #-}
-- | Examples of import issues that Argus should detect
module Imports where

-- =============================================================================
-- Unused imports
-- =============================================================================

import Data.List (sort, nub, group)        -- Only 'sort' used
import Data.Maybe (fromMaybe, catMaybes, mapMaybe)  -- None used!
import qualified Data.Map as M              -- Not used at all
import qualified Data.Set as S              -- Used
import Data.Char (toUpper, toLower, isAlpha, isDigit)  -- Mixed usage
import Control.Monad (when, unless, forM_, forever)  -- Only 'when' used

-- =============================================================================
-- Wildcard imports (should be explicit)
-- =============================================================================

import qualified Data.Text as T  -- Qualified import (but still wildcard)
import Prelude hiding (head) -- OK to hide

-- =============================================================================
-- Missing qualified imports (name collisions)
-- =============================================================================

-- These could cause confusion:
-- import Data.Text (pack)        -- Conflicts with ByteString.pack
-- import Data.ByteString (pack)  -- Conflicts with Text.pack

-- =============================================================================
-- Functions that use only some imports
-- =============================================================================

-- | Uses 'sort' from Data.List, 'S.fromList' from Data.Set
sortUnique :: Ord a => [a] -> [a]
sortUnique = S.toList . S.fromList . sort

-- | Uses 'toUpper' and 'isAlpha' from Data.Char
processText :: String -> String
processText s = map toUpper $ filter isAlpha s

-- | Uses 'when' from Control.Monad
conditionalPrint :: Bool -> IO ()
conditionalPrint flag = when flag (putStrLn "Flag is true")

-- | Uses T.pack from Data.Text
makeText :: String -> T.Text
makeText = T.pack

-- =============================================================================
-- Redundant imports (already in Prelude)
-- =============================================================================

-- Some projects do this unnecessarily:
-- import Data.Bool (Bool(..))  -- Already in Prelude
-- import Data.Int (Int)        -- Already in Prelude
-- import Data.Eq ((==))        -- Already in Prelude

-- =============================================================================
-- Import ordering issues
-- =============================================================================

-- Conventional ordering:
-- 1. Standard library imports
-- 2. Third-party library imports
-- 3. Local module imports
--
-- The imports at the top of this file are somewhat mixed

-- =============================================================================
-- This module doesn't export anything explicitly (implicit export all)
-- =============================================================================
-- Missing explicit export list - should have:
-- module Imports (sortUnique, processText, conditionalPrint, makeText) where
