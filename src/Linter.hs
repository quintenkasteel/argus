{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Linter (checkFile, checkLints) where

import ClassyPrelude
import Config (Config (..), Signature (..), Variable (..))
import qualified Data.ByteString.Char8 as ByteString
import qualified Data.Map as Map
import qualified Extra.Function as Function
import Function (Function (..), FunctionArgument (..))
import qualified Language.Haskell.Exts as Haskell
import Lint (Lint (..), LintMap)
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
  Function {funcName, functionLineNumbers, functionArguments}
  Signature {msg, to, from}
  acc =
    if from `elem` fmap type_ functionArguments
      then
        insertLint
          filePath
          funcName
          from
          Nothing
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
      Just fArg@(FunctionArgument {arg, startPos}) ->
        if shouldInsertLint fArg var
          then
            insertLint
              filePath
              funcName
              arg
              startPos
              (varTo var)
              ( fromMaybe
                  "Found incorrect naming convention in variable"
                  (varMsg var)
              )
              functionLineNumbers
              acc
          else acc
      Nothing -> acc

shouldInsertLint :: FunctionArgument -> Variable -> Bool
shouldInsertLint FunctionArgument {arg} Variable {varFrom, varTo} =
  not (arg == varTo) && maybe True (`Util.match` Util.trimParens arg) varFrom

insertLint :: FilePath -> Text -> Text -> Maybe Int -> Text -> Text -> [Int] -> LintMap -> LintMap
insertLint filePath funcName arg startPos to msg lineNumbers acc =
  Map.insertWith (++) funcName newLints acc
  where
    toText = Util.replacerIgnoreUnderscore arg to arg
    newLints = map (createLint filePath funcName arg startPos toText msg) lineNumbers

createLint :: FilePath -> Text -> Text -> Maybe Int -> Text -> Text -> Int -> Lint
createLint filePath funcName arg startPos to msg lineNumber =
  Lint
    { from = arg,
      to = to,
      msg = msg,
      lineNumber = lineNumber,
      columnNumber = startPos,
      functionName = funcName,
      filePath = filePath
    }

-- Check the type signatures in the Haskell files
checkFile :: Config -> FilePath -> IO (Maybe (String, [Lint]))
checkFile conf file =
  Haskell.parseFile file >>= \case
    Haskell.ParseOk (Haskell.Module _ _ _ _ decls) -> do
      let functions = Function.extract file decls
          signatures = checkTypeSignature file conf mempty functions
          rules = checkVariables file conf signatures functions
      res <- checkLints file (concatMap snd (Map.toList rules))
      pure (Just res)
    _ ->
      pure Nothing

-- Function to replace a specific part of a line in a file
checkLints :: FilePath -> [Lint] -> IO (String, [Lint])
checkLints filename lints = do
  contents <- readFile filename
  -- Util.pPrint lints
  let (newContent, checkedLints) =
        foldr
          ( \l@(Lint {lineNumber, from, to}) acc ->
              Util.replaceInFile acc lineNumber from to l
          )
          (ByteString.unpack contents, [])
          lints
  pure (newContent, checkedLints)
