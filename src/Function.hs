module Function (Function (..), FunctionArgument (..)) where

import ClassyPrelude

data Function = Function
  { funcName :: Text,
    typeSignatureString :: Text,
    functionArguments :: [FunctionArgument],
    functionArgumentsString :: Text,
    functionLineNumbers :: [Int]
  }
  deriving (Show)

data FunctionArgument = FunctionArgument
  { type_ :: Text,
    arg :: Text,
    startPos :: Maybe Int
  }
  deriving (Show)
