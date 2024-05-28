{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -fno-warn-ambiguous-fields #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Lint (Lint (..), LintType (..), Var (..), Signature (..), LintMap) where

import ClassyPrelude
import Data.String.Interpolate (i)
import qualified Data.Text as Text

data Lint = Lint
  { type_ :: LintType,
    functionName :: Text,
    lineNumber :: Int,
    columnNumber :: Int,
    msg :: Text,
    filePath :: FilePath
  }

instance Show Lint where
  show (Lint {msg, type_, lineNumber, columnNumber, functionName, filePath}) =
    [i| #{filePath}:#{show lineNumber}:#{show columnNumber} in #{functionName} - #{msg}:
        #{show type_}
    |]

data LintType
  = Variable Var
  | TypeSignature Signature

instance Show LintType where
  show = \case
    Variable variable -> show variable
    TypeSignature typeSignature -> show typeSignature

data Var = Var
  { from :: Text,
    to :: Text,
    usedAt :: [(Int, Text)]
  }

instance Show Var where
  show (Var {from, to, usedAt}) =
    [i| Found: #{from}
        Perhaps: #{to}
        Used at lines: #{toLineNumbers usedAt}
    |]

toLineNumbers :: [(Int, Text)] -> Text
toLineNumbers = Text.intercalate "," . fmap (tshow . fst)

data Signature = Signature
  { from :: Text,
    to :: Text
  }

instance Show Signature where
  show Signature {from, to} =
    [i| Found: #{from}
        Perhaps: #{to}
    |]

type LintMap = Map Text [Lint]
