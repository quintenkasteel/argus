{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.CLI.Pack
-- Description : Pack management command
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module implements the pack management command for handling rule packs.
module Argus.CLI.Pack
  ( runPack
  ) where

import Control.Monad (forM, forM_, unless)
import Data.Aeson qualified as AE
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import System.Exit (exitWith, ExitCode(ExitFailure), exitFailure)
import System.IO (hPutStrLn, stderr)

import Argus.CLI.Types
import Argus.Rules.Pack qualified as Pack

-- | Run pack management commands
runPack :: GlobalOptions -> PackOptions -> IO ()
runPack _ opts = do
  registry <- Pack.newPackRegistry
  case poAction opts of
    PackList -> do
      packs <- Pack.listPacks registry
      if poOutputJson opts
        then BL.putStr (AE.encode packs)
        else do
          putStrLn "Available Rule Packs:"
          putStrLn (replicate 60 '-')
          forM_ packs $ \pack -> do
            let enabled = if Pack.rpEnabled pack then "[enabled]" else "[disabled]"
            putStrLn $ "  " ++ T.unpack (Pack.rpName pack) ++ " " ++ enabled
            putStrLn $ "    " ++ T.unpack (Pack.rpDescription pack)
            putStrLn $ "    Category: " ++ T.unpack (Pack.rpCategory pack)
            putStrLn $ "    Rules: " ++ show (length (Pack.rpRuleIds pack))
            unless (null (Pack.rpIncludes pack)) $
              putStrLn $ "    Includes: " ++ T.unpack (T.intercalate ", " (Pack.rpIncludes pack))
            putStrLn ""

    PackShow packName -> do
      packInfo <- Pack.getPackInfo registry packName
      case packInfo of
        Nothing -> do
          hPutStrLn stderr $ "Pack not found: " ++ T.unpack packName
          exitFailure
        Just (pack, ruleCount) -> do
          if poOutputJson opts
            then BL.putStr (AE.encode pack)
            else do
              putStrLn $ "Pack: " ++ T.unpack (Pack.rpName pack)
              putStrLn $ "Description: " ++ T.unpack (Pack.rpDescription pack)
              putStrLn $ "Category: " ++ T.unpack (Pack.rpCategory pack)
              putStrLn $ "Priority: " ++ show (Pack.rpPriority pack)
              putStrLn $ "Enabled by default: " ++ show (Pack.rpEnabled pack)
              putStrLn $ "Tags: " ++ T.unpack (T.intercalate ", " (Pack.rpTags pack))
              putStrLn ""
              putStrLn "Direct rules:"
              forM_ (Pack.rpRuleIds pack) $ \ruleId ->
                putStrLn $ "  - " ++ T.unpack ruleId
              unless (null (Pack.rpIncludes pack)) $ do
                putStrLn ""
                putStrLn "Includes packs:"
                forM_ (Pack.rpIncludes pack) $ \inc ->
                  putStrLn $ "  - " ++ T.unpack inc
              unless (null (Pack.rpExcludes pack)) $ do
                putStrLn ""
                putStrLn "Excludes rules:"
                forM_ (Pack.rpExcludes pack) $ \exc ->
                  putStrLn $ "  - " ++ T.unpack exc
              putStrLn ""
              putStrLn $ "Total rules (expanded): " ++ show ruleCount

    PackValidate maybeFile -> do
      case maybeFile of
        Nothing -> do
          -- Validate all builtin packs
          packs <- Pack.listPacks registry
          allWarnings <- forM packs $ \pack -> do
            warnings <- Pack.validatePack registry pack
            unless (null warnings) $ do
              putStrLn $ "Warnings for pack '" ++ T.unpack (Pack.rpName pack) ++ "':"
              forM_ warnings $ \w -> putStrLn $ "  - " ++ T.unpack w
            pure warnings
          if all null allWarnings
            then putStrLn "All packs valid."
            else exitWith (ExitFailure 1)
        Just file -> do
          result <- Pack.importPackFromFile file
          case result of
            Left err -> do
              hPutStrLn stderr $ "Failed to load pack: " ++ T.unpack err
              exitFailure
            Right vpack -> do
              warnings <- Pack.validatePack registry (Pack.vpPack vpack)
              if null warnings
                then putStrLn $ "Pack '" ++ T.unpack (Pack.pmName (Pack.vpManifest vpack)) ++ "' is valid."
                else do
                  putStrLn "Validation warnings:"
                  forM_ warnings $ \w -> putStrLn $ "  - " ++ T.unpack w
                  exitWith (ExitFailure 1)

    PackExport packName outputFile -> do
      mPack <- Pack.getPackByName registry packName
      case mPack of
        Nothing -> do
          hPutStrLn stderr $ "Pack not found: " ++ T.unpack packName
          exitFailure
        Just pack -> do
          let version = case poVersion opts >>= Pack.parseVersion of
                Just v -> v
                Nothing -> Pack.PackVersion 1 0 0 Nothing
          vpack <- Pack.createVersionedPack pack version (poAuthor opts)
          result <- Pack.exportPackToFile outputFile vpack
          case result of
            Left err -> do
              hPutStrLn stderr $ "Failed to export pack: " ++ T.unpack err
              exitFailure
            Right () -> putStrLn $ "Exported pack '" ++ T.unpack packName ++ "' to " ++ outputFile

    PackImport file -> do
      result <- Pack.importPackFromFile file
      case result of
        Left err -> do
          hPutStrLn stderr $ "Failed to import pack: " ++ T.unpack err
          exitFailure
        Right vpack -> do
          let pack = Pack.vpPack vpack
              manifest = Pack.vpManifest vpack
          Pack.registerPack registry pack
          putStrLn $ "Imported pack '" ++ T.unpack (Pack.pmName manifest) ++ "'"
          putStrLn $ "  Version: " ++ T.unpack (Pack.formatVersion (Pack.pmVersion manifest))
          case Pack.pmAuthor manifest of
            Just author -> putStrLn $ "  Author: " ++ T.unpack author
            Nothing -> pure ()

    PackCreate packName -> do
      let newPack = Pack.RulePack
            { Pack.rpName = packName
            , Pack.rpDescription = "Custom rule pack"
            , Pack.rpRuleIds = []
            , Pack.rpIncludes = []
            , Pack.rpExcludes = []
            , Pack.rpPriority = 50
            , Pack.rpCategory = "custom"
            , Pack.rpTags = ["custom"]
            , Pack.rpEnabled = True
            }
      Pack.registerPack registry newPack
      putStrLn $ "Created pack '" ++ T.unpack packName ++ "'"
      putStrLn "Add rules using your configuration file or export and edit the pack file."
