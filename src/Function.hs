module Function (Function (..), FunctionArgument (..)) where

import ClassyPrelude

data Function = Function
  { funcName :: Text,
    typeSignatureString :: Text,
    functionArguments :: [FunctionArgument],
    functionArgumentsString :: Text,
    -- FIXME add in arguments type
    functionLineNumbers :: [Int],
    -- FIXME add lineNumber
    functionBodyLines :: [Text]
  }
  deriving (Show)

data FunctionArgument = FunctionArgument
  --FIXME add line / column number per argument
  { type_ :: Text,
    arg :: Text,
    -- This is the column number of arg
    startPos :: Maybe Int
  }
  deriving (Show)
