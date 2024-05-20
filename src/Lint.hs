module Lint (Lint (..), LintMap) where

import ClassyPrelude

data Lint = Lint
  { from :: Text,
    to :: Text,
    msg :: Text,
    lineNumber :: Int,
    functionName :: Text,
    filePath :: FilePath
  }
  deriving (Show)

type LintMap = Map Text [Lint]
