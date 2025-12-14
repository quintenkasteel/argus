{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveFunctor #-}
-- | Examples of LANGUAGE pragma usage/non-usage that Argus should detect
--
-- This module has many pragmas enabled but only uses some of them.
-- Argus should detect which ones are unused.
module Pragmas where

import GHC.Generics (Generic)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Aeson (ToJSON, FromJSON)

-- =============================================================================
-- Used pragmas
-- =============================================================================

-- | Uses OverloadedStrings
textValue :: Text
textValue = "Hello"  -- Uses OverloadedStrings

-- | Uses BangPatterns
strictSum :: [Int] -> Int
strictSum = go 0
  where
    go !acc []     = acc  -- Uses BangPatterns
    go !acc (x:xs) = go (acc + x) xs

-- | Uses LambdaCase
describeMaybe :: Maybe Int -> String
describeMaybe = \case  -- Uses LambdaCase
  Nothing -> "nothing"
  Just n  -> "just " ++ show n

-- | Uses TupleSections
pairWith :: a -> b -> (a, b)
pairWith x = (x,)  -- Uses TupleSections

-- | Uses RecordWildCards
data Person = Person { name :: String, age :: Int }
  deriving (Show, Generic)  -- Uses DeriveGeneric

showPerson :: Person -> String
showPerson Person{..} = name ++ " is " ++ show age  -- Uses RecordWildCards

-- | Uses NamedFieldPuns
getAge :: Person -> Int
getAge Person{age} = age  -- Uses NamedFieldPuns

-- | Uses DeriveGeneric, DeriveAnyClass via ToJSON/FromJSON
data Config = Config
  { cfgHost :: String
  , cfgPort :: Int
  } deriving (Show, Generic, ToJSON, FromJSON)  -- Uses DeriveGeneric, DeriveAnyClass

-- | Uses DeriveFunctor
data Box a = Box a
  deriving (Show, Functor)  -- Uses DeriveFunctor

-- | Uses MultiWayIf
classifyAge :: Int -> String
classifyAge n = if
  | n < 0     -> "invalid"  -- Uses MultiWayIf
  | n < 13    -> "child"
  | n < 20    -> "teenager"
  | n < 60    -> "adult"
  | otherwise -> "senior"

-- =============================================================================
-- UNUSED pragmas (Argus should flag these)
-- =============================================================================

-- FlexibleContexts    - Not used (no unusual contexts)
-- FlexibleInstances   - Not used (no instances defined)
-- TypeFamilies        - Not used (no type families)
-- GADTs               - Not used (no GADTs)
-- RankNTypes          - Not used (no rank-n types)
-- ScopedTypeVariables - Not used (no scoped type vars with explicit forall)

-- =============================================================================
-- Functions that COULD use extensions but don't need them
-- =============================================================================

-- | This could use ScopedTypeVariables but doesn't need to
simpleFunc :: Int -> Int
simpleFunc x = x + 1

-- | This could use FlexibleContexts but doesn't need to
normalContext :: (Show a, Eq a) => a -> String
normalContext x = show x
