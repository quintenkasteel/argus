{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-incomplete-uni-patterns #-}
{-# OPTIONS_GHC -fplugin=RecordDotPreprocessor #-}

module Linter (checkFile, checkLints) where

import ClassyPrelude
import Data.Char (isAlphaNum)
import Config (Config (..), Signature, Variable)
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Map as Map
import qualified Extra.Function as Function
import Function (Function (..), FunctionArgument (..))
import qualified Language.Haskell.Exts as Haskell
import Lint (Lint, LintMap)
import qualified Lint
import qualified Util

-- Check individual declarations for type signatures
checkTypeSignature :: FilePath -> Config -> LintMap -> [Function] -> LintMap
checkTypeSignature _ _ acc [] = acc
checkTypeSignature filePath conf acc (fn : fns) =
  checkTypeSignature filePath conf updatedAcc fns
  where
    updatedAcc = foldr (processSignature filePath fn) acc (signatures conf)

processSignature :: FilePath -> Function -> Signature -> LintMap -> LintMap
processSignature
  filePath
  function
  signature
  acc =
    case Function.getArgByType signature.from function.arguments of
      Just functionArg ->
        Map.insertWith (++) function.name
          [ Lint.Lint
              { Lint.type_ =
                  Lint.TypeSignature
                    ( Lint.Signature
                        { Lint.from = signature.from,
                          Lint.to = signature.to
                        }
                    ),
                Lint.msg =
                  fromMaybe
                    "Found incorrect naming convention in type signature"
                    signature.msg,
                Lint.lineNumber = fst functionArg.typeStartPos,
                Lint.columnNumber = snd functionArg.typeStartPos,
                Lint.functionName = function.name,
                Lint.filePath = filePath
              }
          ]
          acc
      Nothing -> acc

checkVariables :: FilePath -> Config -> LintMap -> [Function] -> LintMap
checkVariables _ _ acc [] = acc
checkVariables filePath conf acc (fn : fns) =
  checkVariables filePath conf updatedAcc fns
  where
    updatedAcc = foldr (processVariable filePath fn) acc (variables conf)

processVariable ::
  FilePath ->
  Function ->
  Variable ->
  LintMap ->
  LintMap
processVariable
  filePath
  function
  variable
  acc =
    case Function.getArgByType variable.type_ function.arguments of
      Just functionArg ->
        if shouldInsertLint functionArg variable
          then
            Map.insertWith (++) function.name
              [ Lint.Lint
                  { Lint.type_ =
                      Lint.Variable
                        ( Lint.Var
                            { Lint.from = functionArg.arg,
                              Lint.to = variable.to,
                              Lint.usedAt =
                                filter
                                  ( \(_, v) ->
                                      Util.replacerIgnoreUnderscore functionArg.arg variable.to v /= v
                                  )
                                  function.body
                            }
                        ),
                    Lint.msg =
                      fromMaybe
                        "Found incorrect naming convention in variable"
                        variable.msg,
                    Lint.lineNumber = fst functionArg.argStartPos,
                    Lint.columnNumber = snd functionArg.argStartPos,
                    Lint.functionName = function.name,
                    Lint.filePath = filePath
                  }
              ]
              acc
          else acc
      Nothing -> acc

shouldInsertLint :: FunctionArgument -> Variable -> Bool
shouldInsertLint functionArg variable =
  not (functionArg.arg == variable.to)
    && maybe True (`Util.match` Util.trimParens functionArg.arg) variable.from

-- Check the type signatures in the Haskell files
checkFile :: Config -> FilePath -> IO (Maybe (String, [Lint]))
checkFile conf file =
  Haskell.parseFile file >>= \case
    Haskell.ParseOk (Haskell.Module _ _ _ _ decls) -> do
      let functions = Function.extract file decls
          signatures = checkTypeSignature file conf mempty functions
          rules = checkVariables file conf signatures functions
          lints = concatMap snd (Map.toList rules)
      res <- checkLints file lints
      pure (Just res)
    _ ->
      pure Nothing

-- Function to replace a specific part of a line in a file
checkLints :: FilePath -> [Lint] -> IO (String, [Lint])
checkLints filename lints = do
  contents <- readFile filename
  pure (foldr lintReplacer (ByteString.unpack contents) lints, lints)

lintReplacer :: Lint -> String -> String
lintReplacer lint content =
  case lint.type_ of
    Lint.Variable variable ->
      let newContent = replaceLintInContent variable.from variable.to lint.lineNumber lint.columnNumber content
       in foldr (\(i, _) acc -> replaceLintInContent variable.from variable.to i 0 acc) newContent variable.usedAt
    Lint.TypeSignature signature ->
      replaceLintInContent
        signature.from
        signature.to
        lint.lineNumber
        lint.columnNumber
        content

replaceLintInContent :: Text -> Text -> Int -> Int -> String -> String
replaceLintInContent from to lineNumber columnNumber acc =
  let ls = lines acc
      (before, line:after) = splitAt (lineNumber - 1) ls
      (beforeArg, argRest) = splitAt (columnNumber - 1) line
      candidate = unpack from
      candidateLength = length candidate
      (matchText, restText) = splitAt candidateLength argRest
      isWordChar c = isAlphaNum c || c == '_'
      validReplacement = matchText == candidate && (null restText || not (isWordChar (Prelude.head restText)))
      newLine = if validReplacement
                then beforeArg ++ unpack to ++ restText
                else line
      modifiedLines = before ++ [newLine] ++ after
   in unlines modifiedLines
