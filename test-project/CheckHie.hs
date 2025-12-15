#!/usr/bin/env stack
-- stack script --resolver lts-22.43 --package ghc --package containers --package filepath

{-# LANGUAGE PackageImports #-}

-- | Check HIE file content to verify TH expansion
module Main where

import "ghc" GHC.Iface.Ext.Binary (readHieFile, hie_file_result)
import "ghc" GHC.Iface.Ext.Types (HieFile(..), HieASTs(..), getAsts)
import "ghc" GHC.Iface.Ext.Utils (generateReferencesMap)
import "ghc" GHC.Types.Name (nameOccName)
import "ghc" GHC.Types.Name.Occurrence (occNameString)

import HieDb.Utils (makeNc)

import qualified Data.Map as Map
import Data.List (nub, sort)

main :: IO ()
main = do
  putStrLn "Reading THRealTest.hie..."
  nc <- makeNc
  hieResult <- readHieFile nc ".hie/THRealTest.hie"
  let hieFile = hie_file_result hieResult
      asts = getAsts $ hie_asts hieFile
      refMap = generateReferencesMap (Map.elems asts)

      -- Extract names
      names = nub $ sort $ concatMap extractName $ Map.keys refMap

  putStrLn "\nAll identifier names in THRealTest.hie:"
  mapM_ putStrLn names

  putStrLn "\n=== KEY CHECK ==="
  putStrLn $ "onlyCalledFromTH present: " ++ show ("onlyCalledFromTH" `elem` names)
  putStrLn $ "reallyUnused present: " ++ show ("reallyUnused" `elem` names)
  putStrLn $ "runGenerated present: " ++ show ("runGenerated" `elem` names)

extractName :: Either a b -> [String]
extractName (Left _) = []
extractName (Right name) = [occNameString $ nameOccName name]
