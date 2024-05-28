{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Function (Function (..), FunctionArgument (..)) where

import ClassyPrelude

data Function = Function
  { name :: Text,
    arguments :: [FunctionArgument],
    body :: [(Int, Text)]
  }
  deriving (Show)

data FunctionArgument = FunctionArgument
  { type_ :: Text,
    typeStartPos :: (Int, Int),
    arg :: Text,
    argStartPos :: (Int, Int)
  }
  deriving (Show)
