{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ImportQualifiedPost #-}

-- | Simulation of Yesod's widgetFile and related Template Haskell patterns
--
-- This module demonstrates how Argus should handle functions that appear
-- "unused" in source code but are actually used by TH-generated code.
--
-- Key patterns tested:
-- 1. Functions used only within TH splices (like Hamlet interpolation)
-- 2. Functions used via widgetFile/hamletFile patterns
-- 3. Helper functions called by TH-generated code
module YesodTHSimulation
  ( -- * Widget and Template Types
    Widget
  , Html

    -- * Template Functions (simulate widgetFile behavior)
  , homepageWidget
  , userProfileWidget

    -- * Functions used within templates
    -- These appear "unused" but are referenced by TH code
  , formatUserName
  , formatDate
  , getDefaultTitle
  , userGreeting
  , calculateAge

    -- * Genuinely unused functions (for comparison)
  , actuallyUnusedFunction
  ) where

import Data.Text (Text)
import Data.Text qualified as T
import Language.Haskell.TH (Q, Exp)
import Language.Haskell.TH.Quote (QuasiQuoter(..))

--------------------------------------------------------------------------------
-- Type Aliases (simulating Yesod types)
--------------------------------------------------------------------------------

-- | Widget monad (simplified - in Yesod this would be a full monad)
type Widget = IO ()

-- | HTML type (simplified)
type Html = Text

--------------------------------------------------------------------------------
-- Simulated widgetFile QuasiQuoter
--
-- In real Yesod:
--   $(widgetFile "homepage")
-- expands to code that references functions used in templates via #{...}
--------------------------------------------------------------------------------

-- | Simulate a Hamlet-style quasiquoter that generates code referencing our functions
--
-- In real Hamlet templates, #{formatUserName user} becomes actual Haskell code
-- that calls formatUserName. This is why the function appears "used" in HIE
-- but "unused" in source analysis.
hamlet :: QuasiQuoter
hamlet = QuasiQuoter
  { quoteExp = hamletExp
  , quotePat = error "hamlet: patterns not supported"
  , quoteType = error "hamlet: types not supported"
  , quoteDec = error "hamlet: declarations not supported"
  }

hamletExp :: String -> Q Exp
hamletExp _template = do
  -- In real Hamlet, this parses the template and generates code
  -- The generated code would contain calls to interpolated functions
  [| pure () :: IO () |]

--------------------------------------------------------------------------------
-- Simulated Widget Functions (like $(widgetFile "homepage"))
--------------------------------------------------------------------------------

-- | Simulate $(widgetFile "homepage")
-- The TH expansion internally references: formatUserName, getDefaultTitle
homepageWidget :: Text -> Widget
homepageWidget userName = do
  -- Simulates what the TH-generated code does:
  let _title = getDefaultTitle
      _greeting = userGreeting userName
  pure ()

-- | Simulate $(widgetFile "user-profile")
-- The TH expansion internally references: formatUserName, formatDate, calculateAge
userProfileWidget :: Text -> Int -> Int -> Widget
userProfileWidget name birthYear currentYear = do
  -- Simulates TH-generated template code
  let _formattedName = formatUserName name
      _age = calculateAge birthYear currentYear
  pure ()

--------------------------------------------------------------------------------
-- Functions Used Within Templates (via #{...} interpolation)
--
-- CRITICAL: These functions appear to have no direct callers in source code,
-- but they ARE used by TH-generated code. The HIE file (generated after TH
-- expansion) shows them as used, while source-only analysis would miss this.
--------------------------------------------------------------------------------

-- | Format a user's name for display
-- Used in templates via: #{formatUserName user}
formatUserName :: Text -> Text
formatUserName name = T.toTitle name

-- | Format a date for display (ISO format simulation)
-- Used in templates via: #{formatDate year month day}
formatDate :: Int -> Int -> Int -> Text
formatDate year month day =
  T.pack $ show year <> "-" <> padTwo month <> "-" <> padTwo day
  where
    padTwo n = let s = show n in if length s == 1 then "0" <> s else s

-- | Get the default page title
-- Used in templates via: <title>#{getDefaultTitle}</title>
getDefaultTitle :: Text
getDefaultTitle = "Welcome to Our Site"

-- | Generate a greeting for a user
-- Used in templates via: <h1>#{userGreeting name}</h1>
userGreeting :: Text -> Text
userGreeting name = "Hello, " <> formatUserName name <> "!"

-- | Calculate age from birth year
-- Used in templates via: #{calculateAge birthYear currentYear}
calculateAge :: Int -> Int -> Int
calculateAge birthYear currentYear = currentYear - birthYear

--------------------------------------------------------------------------------
-- Actually Unused Functions (should be flagged by unused detection)
--------------------------------------------------------------------------------

-- | This function is genuinely unused - not called anywhere
-- Argus should flag this as unused
actuallyUnusedFunction :: Int -> Int
actuallyUnusedFunction x = x * x + 42

-- | This helper is also unused
_internalUnusedHelper :: Text -> Text
_internalUnusedHelper = T.reverse
