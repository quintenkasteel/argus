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

-- Check individual declarations for type signatures
checkVariables :: FilePath -> Config -> LintMap -> [Function] -> LintMap
checkVariables _ _ acc [] = acc
checkVariables filePath conf acc (Function {funcName, functionLineNumbers, functionArguments} : xs) =
  checkVariables
    filePath
    conf
    ( foldr
        ( \(Variable {varType, varFrom, varTo, varMsg}) newAcc ->
            case Function.getArgByType varType functionArguments of
              Just (FunctionArgument {arg}) ->
                if all
                  ((==) True)
                  [ (not (arg == varTo)),
                    maybe True (\f -> match f (trimParens arg)) varFrom
                  ]
                  then
                    let to =
                          Util.replacerIgnoreUnderscore
                            arg
                            varTo
                            arg

                        toMsg :: Text = fromMaybe [i|Change #{arg} to #{to}|] varMsg
                     in Map.insertWith
                          ((++))
                          funcName
                          ( fmap
                              ( \lineNumber ->
                                  Lint
                                    { from = arg,
                                      to = to,
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
              Nothing -> newAcc
        )
        acc
        (variables conf)
    )
    xs

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
