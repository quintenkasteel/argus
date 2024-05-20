{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Linter (checkFile, checkLints) where

import ClassyPrelude
import Config (Config (..), Signature (..), Variable (..))
import qualified Config
import qualified Data.Map as Map
import Data.String.Interpolate (i)
import qualified Extra.Function as Function
import Function (Function (..), FunctionArgument (..))
import Language.Haskell.Exts
import Lint (Lint (..), LintMap)
import qualified System.Environment as Environment
import Util

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
  Function {funcName, functionLineNumbers, functionArguments}
  Signature {msg, to, from}
  acc =
    if from `elem` fmap type_ functionArguments
      then
        insertLint
          filePath
          funcName
          from
          to
          ( fromMaybe
              "Found incorrect naming convention in type signature"
              msg
          )
          functionLineNumbers
          acc
      else acc

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
  Function {funcName, functionLineNumbers, functionArguments}
  var
  acc =
    case Function.getArgByType (varType var) functionArguments of
      Just FunctionArgument {arg} ->
        if shouldInsertLint arg var
          then
            insertLint
              filePath
              funcName
              arg
              (varTo var)
              ( fromMaybe
                  "Found incorrect naming convention in variable"
                  (varMsg var)
              )
              functionLineNumbers
              acc
          else acc
      Nothing -> acc

shouldInsertLint :: Text -> Variable -> Bool
shouldInsertLint arg Variable {varFrom, varTo} =
  not (arg == varTo) && maybe True (`match` trimParens arg) varFrom

insertLint :: FilePath -> Text -> Text -> Text -> Text -> [Int] -> LintMap -> LintMap
insertLint filePath funcName arg to msg lineNumbers acc =
  Map.insertWith (++) funcName newLints acc
  where
    toText = Util.replacerIgnoreUnderscore arg to arg
    newLints = map (createLint filePath funcName arg toText msg) lineNumbers

createLint :: FilePath -> Text -> Text -> Text -> Text -> Int -> Lint
createLint filePath funcName from to msg lineNumber =
  Lint
    { from = from,
      to = to,
      msg = msg,
      lineNumber = lineNumber,
      functionName = funcName,
      filePath = filePath
    }

-- Check the type signatures in the Haskell files
checkFile :: Config -> FilePath -> IO (Maybe (String, [Lint]))
checkFile conf file = do
  result <- parseFile file
  case result of
    ParseOk (Module _ _ _ _ decls) -> do
      let functions = Function.extract file decls
          signatures = checkTypeSignature file conf mempty functions
          rules = checkVariables file conf signatures functions
      res <- checkLints file (concatMap snd (Map.toList rules))
      pure (Just res)
    _ ->
      pure Nothing
