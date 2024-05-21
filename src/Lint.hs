{-# LANGUAGE QuasiQuotes #-}

module Lint (Lint (..), LintMap) where

import ClassyPrelude
import Data.String.Interpolate (i)

data Lint = Lint
  { from :: Text,
    to :: Text,
    msg :: Text,
    lineNumber :: Int,
    columnNumber :: Maybe Int,
    functionName :: Text,
    filePath :: FilePath
  }

instance Show Lint where
  show (Lint {from, to, msg, lineNumber, columnNumber, functionName, filePath}) =
    [i| #{filePath}:#{lineNumber}:#{fromMaybe "" (fmap show columnNumber)} in #{functionName} - #{msg}:
        Found: #{from}
        Perhaps: #{to}
    |]

type LintMap = Map Text [Lint]
