{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE NoImplicitPrelude #-}

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
checkTypeSignature filePath conf acc (Function {funcName, functionLineNumbers, functionArguments} : xs) =
  checkTypeSignature
    filePath
    conf
    ( foldr
        ( \(Signature {from, to, msg}) newAcc ->
            if from `elem` fmap type_ functionArguments
              then
                let toVal = Util.replacerIgnoreUnderscore from to from
                    toMsg :: Text = fromMaybe [i|Change #{from} to #{toVal}|] msg
                 in Map.insertWith
                      ((++))
                      funcName
                      ( fmap
                          ( \lineNumber ->
                              Lint
                                { from = from,
                                  to = toVal,
                                  msg = toMsg,
                                  lineNumber = lineNumber,
                                  functionName = funcName,
                                  filePath = filePath
                                }
                          )
                          functionLineNumbers
                      )
                      newAcc
              else newAcc
        )
        acc
        (signatures conf)
    )
    xs

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
          then insertLint filePath funcName arg (varTo var) (varMsg var) functionLineNumbers acc
          else acc
      Nothing -> acc

shouldInsertLint :: Text -> Variable -> Bool
shouldInsertLint arg Variable {varFrom, varTo} =
  not (arg == varTo) && maybe True (`match` trimParens arg) varFrom

insertLint :: FilePath -> Text -> Text -> Text -> Maybe Text -> [Int] -> LintMap -> LintMap
insertLint filePath funcName arg to varMsg lineNumbers acc =
  Map.insertWith (++) funcName newLints acc
  where
    toText = Util.replacerIgnoreUnderscore arg to arg
    msg = fromMaybe [i|Change #{arg} to #{toText}|] varMsg
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

-- Main function to apply variable naming checks
main :: IO ()
main = do
  args <- Environment.getArgs
  config <- Config.parse
  let dirs = if null args then directoriesToCheck else args
  files <- concat <$> mapM listHaskellFiles dirs
  traverse_ (checkFile config) files

-- Directories to check
directoriesToCheck :: [FilePath]
directoriesToCheck = ["check"]

-- Check the type signatures in the Haskell files
checkFile :: Config -> FilePath -> IO ()
checkFile conf file = do
  result <- parseFile file
  case result of
    ParseOk (Module _ _ _ _ decls) -> do
      let functions = Function.extract file decls
          signatures = checkTypeSignature file conf mempty functions
          rules = checkVariables file conf signatures functions
      (newContent, lints) <- checkLints file (concatMap snd (Map.toList rules))
      Util.pPrint lints
      Util.pPrint newContent
    _ ->
      pure ()
