{-# LANGUAGE StrictData #-}

-- |
-- Module      : Argus.CLI.Index
-- Description : HIE indexing command
-- Copyright   : (c) 2024
-- License     : MIT
--
-- = Overview
--
-- This module implements the @argus index@ command, which builds a project
-- with HIE file generation enabled and creates an hiedb database for
-- semantic analysis.
--
-- = Execution Flow
--
-- @
-- ┌─────────────────────────────────────────────────────────────────────┐
-- │                          runIndex                                    │
-- │                                                                      │
-- │  1. Detect Project ──► 2. Detect GHC ──► 3. Build with HIE         │
-- │         │                    │                   │                  │
-- │         ▼                    ▼                   ▼                  │
-- │    Stack/Cabal          Version            .hie files               │
-- │                                                  │                  │
-- │                                    4. Find/Install hiedb            │
-- │                                                  │                  │
-- │                                                  ▼                  │
-- │                                    5. Index HIE ──► .hiedb          │
-- │                                                  │                  │
-- │                                    6. Create symlink (optional)     │
-- └─────────────────────────────────────────────────────────────────────┘
-- @
--
-- = Project Detection
--
-- Automatically detects project type by checking for:
-- * Stack: @stack.yaml@
-- * Cabal: @cabal.project@ or @*.cabal@ files
--
-- = hiedb Management
--
-- If hiedb is not found:
-- 1. Check system PATH
-- 2. Prompt to install (unless @--yes@ provided)
-- 3. Install via @stack install hiedb@ or @cabal install hiedb@
--
-- @since 1.0.0
module Argus.CLI.Index
  ( -- * Command Entry Point
    runIndex
    -- * Types
  , ProjectType(..)
  ) where

import Control.Exception (try, SomeException)
import Control.Monad (when, unless)
import Data.List (isInfixOf, isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import System.Directory (doesFileExist, doesDirectoryExist, listDirectory,
                         createDirectoryLink, removeFile, getCurrentDirectory,
                         getSymbolicLinkTarget, pathIsSymbolicLink)
import System.Exit (exitFailure, ExitCode(ExitFailure))
import System.Exit qualified as Exit
import System.FilePath ((</>), makeRelative)
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import System.Process (readProcessWithExitCode, readCreateProcess, createProcess, proc, cwd, std_out, std_err, waitForProcess, StdStream(..))
import GHC.IO.Handle (hGetContents)

import Argus.CLI.Types

-- | Detected Haskell project build system.
--
-- @since 1.0.0
data ProjectType
  = StackProject
    -- ^ Project using Stack (has stack.yaml)
  | CabalProject
    -- ^ Project using Cabal (has *.cabal or cabal.project)
  deriving stock (Eq, Show)

-- | Run the index command.
--
-- Builds the project with HIE file generation enabled, then indexes
-- the HIE files with hiedb for use by semantic analysis.
--
-- = Side Effects
--
-- * Builds the project (unless @--no-build@)
-- * Creates @.hiedb@ database
-- * Optionally creates @.hie@ symlink
-- * May install hiedb if not present
--
-- @since 1.0.0
runIndex :: GlobalOptions -> IndexOptions -> IO ()
runIndex global opts = do
  let verbose = goVerbose global
      projectDir = fromMaybe "." (ixTargetDir opts)

  -- Change to project directory
  curDir <- getCurrentDirectory
  let workDir = if projectDir == "." then curDir else projectDir

  logInfo verbose $ "Working in: " <> workDir

  -- 1. Detect project type (Stack or Cabal)
  projectType <- detectProjectType workDir
  logInfo verbose $ "Detected project type: " <> show projectType

  -- 2. Detect GHC version for this project
  ghcVersion <- detectGhcVersion workDir projectType
  logInfo verbose $ "Project GHC version: " <> ghcVersion

  -- 3. Build the project with HIE file generation (if requested)
  hieDir <- if ixBuild opts
    then do
      logInfo True "Building project with HIE file generation..."
      when (ixClean opts) $ do
        logInfo verbose "Cleaning build artifacts..."
        cleanProject workDir projectType
      buildWithHie workDir projectType
    else do
      logInfo verbose "Skipping build (--no-build)"
      findExistingHieDir workDir projectType

  logInfo verbose $ "HIE directory: " <> hieDir

  -- 4. Check if hiedb is available (matching GHC version), install if needed
  hiedbCmd <- findOrInstallHiedb workDir projectType ghcVersion verbose (ixYes opts)
  logInfo verbose $ "Using hiedb: " <> hiedbCmd

  -- 5. Index the HIE files
  let dbPath = fromMaybe (workDir </> ".hiedb") (ixDbPath opts)
  logInfo True $ "Indexing HIE files into: " <> dbPath
  indexHieFiles hiedbCmd workDir dbPath hieDir verbose

  -- 6. Create .hie symlink (if requested)
  when (ixSymlink opts) $ do
    createHieSymlink workDir hieDir verbose

  logInfo True "✓ Indexing complete!"
  logInfo True $ "  Database: " <> dbPath
  logInfo True $ "  HIE files: " <> hieDir
  logInfo True ""
  logInfo True "You can now run argus with accurate Template Haskell support."

--------------------------------------------------------------------------------
-- Project Type Detection
--------------------------------------------------------------------------------

-- | Detect project type by checking for stack.yaml or cabal.project
detectProjectType :: FilePath -> IO ProjectType
detectProjectType dir = do
  hasStack <- doesFileExist (dir </> "stack.yaml")
  hasCabalProject <- doesFileExist (dir </> "cabal.project")

  -- Also check for .cabal files
  files <- listDirectory dir
  let hasCabalFile = any (".cabal" `isInfixOf`) files

  if hasStack
    then pure StackProject
    else if hasCabalProject || hasCabalFile
      then pure CabalProject
      else do
        hPutStrLn stderr "Could not detect project type. No stack.yaml or *.cabal file found."
        exitFailure

-- | Detect GHC version for the project
detectGhcVersion :: FilePath -> ProjectType -> IO String
detectGhcVersion dir projectType = do
  result <- try @SomeException $ case projectType of
    StackProject -> do
      -- Use stack to get GHC version (run from project directory)
      out <- readCreateProcess ((proc "stack" ["ghc", "--", "--numeric-version"]) { cwd = Just dir }) ""
      pure $ filter (/= '\n') out
    CabalProject -> do
      -- Use cabal/ghc to get version
      (_, out, _) <- readProcessWithExitCode "ghc" ["--numeric-version"] ""
      pure $ filter (/= '\n') out

  case result of
    Right v | not (null v) -> pure v
    _ -> do
      -- Fallback: try ghc directly
      (_, out, _) <- readProcessWithExitCode "ghc" ["--numeric-version"] ""
      let v = filter (/= '\n') out
      if null v
        then do
          hPutStrLn stderr "Could not detect GHC version"
          exitFailure
        else pure v

--------------------------------------------------------------------------------
-- Project Building
--------------------------------------------------------------------------------

-- | Clean project build artifacts
cleanProject :: FilePath -> ProjectType -> IO ()
cleanProject dir projectType = do
  _ <- case projectType of
    StackProject -> readCreateProcess ((proc "stack" ["clean"]) { cwd = Just dir }) ""
    CabalProject -> readCreateProcess ((proc "cabal" ["clean"]) { cwd = Just dir }) ""
  pure ()

-- | Build project with HIE file generation enabled
buildWithHie :: FilePath -> ProjectType -> IO FilePath
buildWithHie dir projectType = do
  case projectType of
    StackProject -> do
      -- Stack: build with -fwrite-ide-info (run from project directory with relative paths)
      let processSpec = (proc "stack"
            [ "build"
            , "--ghc-options=-fwrite-ide-info -hiedir=.hie"
            ]) { cwd = Just dir, std_err = CreatePipe }
      (_, _, Just errH, ph) <- createProcess processSpec
      exitCode <- waitForProcess ph
      err <- hGetContents errH
      case exitCode of
        Exit.ExitSuccess -> pure $ dir </> ".hie"
        ExitFailure _ -> do
          hPutStrLn stderr $ "Build failed:\n" <> err
          exitFailure

    CabalProject -> do
      -- Cabal: build with -fwrite-ide-info (run from project directory)
      let processSpec = (proc "cabal"
            [ "build"
            , "all"
            , "--ghc-options=-fwrite-ide-info -hiedir=.hie"
            ]) { cwd = Just dir, std_err = CreatePipe }
      (_, _, Just errH, ph) <- createProcess processSpec
      exitCode <- waitForProcess ph
      err <- hGetContents errH
      case exitCode of
        Exit.ExitSuccess -> pure $ dir </> ".hie"
        ExitFailure _ -> do
          hPutStrLn stderr $ "Build failed:\n" <> err
          exitFailure

-- | Find existing HIE directory if not building
findExistingHieDir :: FilePath -> ProjectType -> IO FilePath
findExistingHieDir dir projectType = do
  -- Check common locations
  let candidates = case projectType of
        StackProject ->
          [ dir </> ".hie"
          , dir </> ".stack-work" </> "dist"
          ]
        CabalProject ->
          [ dir </> ".hie"
          , dir </> "dist-newstyle"
          ]

  existing <- filterM doesDirectoryExist candidates
  case existing of
    (d:_) -> pure d
    [] -> do
      -- Try to find HIE files recursively in build directories
      mHieDir <- findHieDirRecursive dir projectType
      case mHieDir of
        Just d -> pure d
        Nothing -> do
          hPutStrLn stderr "Could not find HIE directory. Try running with --build or specify --hie-dir"
          exitFailure
  where
    filterM p = foldr (\x acc -> do
      b <- p x
      rest <- acc
      pure $ if b then x:rest else rest) (pure [])

-- | Find HIE directory recursively in build directories
findHieDirRecursive :: FilePath -> ProjectType -> IO (Maybe FilePath)
findHieDirRecursive dir projectType = do
  let searchRoot = case projectType of
        StackProject -> dir </> ".stack-work"
        CabalProject -> dir </> "dist-newstyle"

  exists <- doesDirectoryExist searchRoot
  if not exists
    then pure Nothing
    else findHieInDir searchRoot
  where
    findHieInDir d = do
      contents <- listDirectory d
      -- Check if any .hie files exist in this directory
      let hieFiles = filter (".hie" `isInfixOf`) contents
      if not (null hieFiles)
        then pure $ Just d
        else do
          -- Search subdirectories
          subdirs <- filterM (doesDirectoryExist . (d </>)) contents
          results <- mapM (findHieInDir . (d </>)) subdirs
          pure $ listToMaybe $ concatMap (maybe [] pure) results

    filterM p = foldr (\x acc -> do
      b <- p x
      rest <- acc
      pure $ if b then x:rest else rest) (pure [])

--------------------------------------------------------------------------------
-- HIEdb Management
--------------------------------------------------------------------------------

-- | Find or install hiedb matching the project's GHC version
findOrInstallHiedb :: FilePath -> ProjectType -> String -> Bool -> Bool -> IO String
findOrInstallHiedb dir projectType ghcVersion verbose autoYes = do
  -- First, try to use hiedb from the project's environment
  mHiedb <- findHiedbInEnv dir projectType

  case mHiedb of
    Just cmd -> pure cmd
    Nothing -> do
      -- hiedb not found - try system hiedb
      (sysExitCode, _, _) <- readProcessWithExitCode "which" ["hiedb"] ""
      case sysExitCode of
        Exit.ExitSuccess -> do
          logInfo verbose "Found system hiedb"
          -- Check if system hiedb matches GHC version
          (_, sysVersion, _) <- readProcessWithExitCode "hiedb" ["--version"] ""
          logInfo verbose $ "System hiedb version: " <> filter (/= '\n') sysVersion
          logInfo True "Using system hiedb (note: if GHC versions don't match, indexing may fail)"
          pure "hiedb"

        ExitFailure _ -> do
          -- No hiedb at all - offer to install
          logInfo True ""
          logInfo True "⚠ hiedb not found."
          logInfo True ""

          shouldInstall <- if autoYes
            then do
              logInfo True "Auto-installing hiedb (--yes flag provided)..."
              pure True
            else askYesNo $ "Would you like to install hiedb for GHC " <> ghcVersion <> "?"

          if shouldInstall
            then installHiedb dir projectType ghcVersion verbose
            else do
              hPutStrLn stderr "hiedb is required but not found. Please install it manually:"
              case projectType of
                StackProject -> do
                  hPutStrLn stderr $ "  stack install hiedb"
                CabalProject -> do
                  hPutStrLn stderr "  cabal install hiedb"
              exitFailure

-- | Find hiedb in the project's environment
findHiedbInEnv :: FilePath -> ProjectType -> IO (Maybe String)
findHiedbInEnv dir projectType = case projectType of
  StackProject -> do
    -- Try stack exec hiedb (run from project directory)
    result <- try @SomeException $
      readCreateProcess ((proc "stack" ["exec", "which", "--", "hiedb"]) { cwd = Just dir }) ""
    case result of
      Right out | not (null (filter (/= '\n') out)) -> pure $ Just "stack exec hiedb --"
      _ -> pure Nothing

  CabalProject -> do
    -- Try which hiedb
    (exitCode, _, _) <- readProcessWithExitCode "which" ["hiedb"] ""
    case exitCode of
      Exit.ExitSuccess -> pure $ Just "hiedb"
      ExitFailure _ -> pure Nothing

-- | Ask user a yes/no question
askYesNo :: String -> IO Bool
askYesNo question = do
  putStr $ question <> " [y/N] "
  hFlush stdout
  answer <- getLine
  pure $ answer `elem` ["y", "Y", "yes", "Yes", "YES"]

-- | Install hiedb using the appropriate package manager
installHiedb :: FilePath -> ProjectType -> String -> Bool -> IO String
installHiedb dir projectType ghcVersion verbose = do
  logInfo True $ "Installing hiedb for GHC " <> ghcVersion <> "..."
  logInfo True ""

  case projectType of
    StackProject -> do
      -- Use stack install
      logInfo True "Running: stack install hiedb"
      logInfo True "(This may take a few minutes...)"
      logInfo True ""

      -- Run stack install from project directory
      let processSpec = (proc "stack" ["install", "hiedb"]) { cwd = Just dir, std_err = CreatePipe, std_out = CreatePipe }
      (_, Just outH, Just errH, ph) <- createProcess processSpec
      exitCode <- waitForProcess ph
      out <- hGetContents outH
      err <- hGetContents errH

      case exitCode of
        Exit.ExitSuccess -> do
          logInfo verbose out
          logInfo True "✓ hiedb installed successfully!"
          -- After installation, it should be available via stack exec
          pure "stack exec hiedb --"

        ExitFailure _ -> do
          hPutStrLn stderr $ "Failed to install hiedb:\n" <> err
          hPutStrLn stderr ""
          hPutStrLn stderr "You may need to install it manually:"
          hPutStrLn stderr "  stack install hiedb"
          exitFailure

    CabalProject -> do
      -- Use cabal install
      logInfo True "Running: cabal install hiedb"
      logInfo True "(This may take a few minutes...)"
      logInfo True ""

      (exitCode, out, err) <- readProcessWithExitCode "cabal"
        ["install", "hiedb", "--install-method=copy", "--overwrite-policy=always"] ""

      case exitCode of
        Exit.ExitSuccess -> do
          logInfo verbose out
          logInfo True "✓ hiedb installed successfully!"
          pure "hiedb"

        ExitFailure _ -> do
          hPutStrLn stderr $ "Failed to install hiedb:\n" <> err
          hPutStrLn stderr ""
          hPutStrLn stderr "You may need to install it manually:"
          hPutStrLn stderr "  cabal install hiedb"
          exitFailure

--------------------------------------------------------------------------------
-- HIE Indexing
--------------------------------------------------------------------------------

-- | Index HIE files using hiedb
indexHieFiles :: String -> FilePath -> FilePath -> FilePath -> Bool -> IO ()
indexHieFiles hiedbCmd workDir dbPath hieDir verbose = do
  -- Parse the hiedb command (may include "stack exec hiedb --")
  let cmdParts = words hiedbCmd
      (cmd, baseArgs) = case cmdParts of
        (c:rest) -> (c, rest)
        [] -> ("hiedb", [])

      -- Use relative paths when running from workDir
      relDbPath = makeRelative workDir dbPath
      relHieDir = makeRelative workDir hieDir
      args = baseArgs ++ ["-D", relDbPath, "index", relHieDir]

  logInfo verbose $ "Running: " <> unwords (cmd : args)

  -- Run from the project directory
  let processSpec = (proc cmd args) { cwd = Just workDir, std_out = CreatePipe, std_err = CreatePipe }
  (_, Just outH, Just errH, ph) <- createProcess processSpec
  exitCode <- waitForProcess ph
  out <- hGetContents outH
  err <- hGetContents errH

  case exitCode of
    Exit.ExitSuccess -> do
      unless (null out) $ logInfo verbose out
      -- Check for warnings in stderr
      when (not (null err) && "Warning" `isPrefixOf` err) $
        logInfo True err
    ExitFailure _ -> do
      hPutStrLn stderr $ "hiedb indexing failed:\n" <> err
      hPutStrLn stderr ""
      hPutStrLn stderr "This may be due to a GHC version mismatch."
      hPutStrLn stderr "Make sure hiedb was built with the same GHC version as your project."
      exitFailure

--------------------------------------------------------------------------------
-- Symlink Management
--------------------------------------------------------------------------------

-- | Create a .hie symlink in the project root pointing to the actual HIE directory
createHieSymlink :: FilePath -> FilePath -> Bool -> IO ()
createHieSymlink projectDir hieDir verbose = do
  let symlinkPath = projectDir </> ".hie"

  -- Check if .hie already exists
  symlinkResult <- try @SomeException $ pathIsSymbolicLink symlinkPath
  let isSymlink = case symlinkResult of
        Right True -> True
        _ -> False
  isDir <- doesDirectoryExist symlinkPath

  if symlinkPath == hieDir
    then logInfo verbose $ ".hie is already the HIE directory"
    else if isSymlink
      then do
        -- Check if it points to the right place
        target <- getSymbolicLinkTarget symlinkPath
        if target == hieDir || target == makeRelative projectDir hieDir
          then logInfo verbose $ ".hie symlink already points to " <> hieDir
          else do
            logInfo verbose $ "Updating .hie symlink: " <> target <> " -> " <> hieDir
            removeFile symlinkPath
            createSymlink projectDir hieDir symlinkPath
      else if isDir
        then logInfo verbose $ ".hie directory exists, not creating symlink"
        else do
          logInfo verbose $ "Creating .hie symlink -> " <> hieDir
          createSymlink projectDir hieDir symlinkPath

-- | Create a symlink, using relative path if possible
createSymlink :: FilePath -> FilePath -> FilePath -> IO ()
createSymlink projectDir target symlinkPath = do
  let relTarget = makeRelative projectDir target
  result <- try @SomeException $ createDirectoryLink relTarget symlinkPath
  case result of
    Right () -> pure ()
    Left err -> do
      hPutStrLn stderr $ "Warning: Could not create symlink: " <> show err
      hPutStrLn stderr $ "You may need to manually create: ln -s " <> relTarget <> " " <> symlinkPath

--------------------------------------------------------------------------------
-- Utilities
--------------------------------------------------------------------------------

-- | Log info message
logInfo :: Bool -> String -> IO ()
logInfo enabled msg = when enabled $ do
  putStrLn msg
  hFlush stdout
