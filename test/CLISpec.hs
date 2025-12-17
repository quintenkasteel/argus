{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : CLISpec
-- Description : Tests for Argus.CLI
--
-- Comprehensive tests for the command-line interface including
-- argument parsing, option validation, and command execution paths.
module CLISpec (spec) where

import Test.Hspec
import Data.Text (Text)
import Data.Text qualified as T
import Options.Applicative
import Options.Applicative.Types (ParserResult(..))

import Argus.CLI
import Argus.Types (Verbosity(..))
import Argus.Refactor.Validation (ValidationLevel(..))
import Argus.Refactor.FixGraph (ConflictStrategy(..))

spec :: Spec
spec = do
  describe "Argus.CLI" $ do
    parseGlobalOptionsSpec
    parseCheckOptionsSpec
    parseFixOptionsSpec
    parseUnusedOptionsSpec
    parseInitOptionsSpec
    parseIndexOptionsSpec
    parseWatchOptionsSpec
    parseDiffOptionsSpec
    parseBaselineOptionsSpec
    parseStatsOptionsSpec
    parseDaemonOptionsSpec
    parseLspOptionsSpec
    parseArchitectureOptionsSpec
    parsePackOptionsSpec
    parseCIModeOptionsSpec
    parseCommandSpec

--------------------------------------------------------------------------------
-- Helper function to parse arguments
--------------------------------------------------------------------------------

-- | Parse arguments with the full command parser
parseArgs :: [String] -> ParserResult Command
parseArgs args = execParserPure defaultPrefs parseCommandWithInfo args
  where
    parseCommandWithInfo = info (parseCommand <**> helper)
      ( fullDesc
      <> progDesc "The All-Seeing Haskell Static Analyzer"
      <> header "argus - Comprehensive Haskell static analysis and refactoring"
      )

-- | Check if parsing succeeded
parseSuccess :: [String] -> Bool
parseSuccess args = case parseArgs args of
  Success _ -> True
  _ -> False

-- | Extract parsed command on success
getCommand :: [String] -> Maybe Command
getCommand args = case parseArgs args of
  Success cmd -> Just cmd
  _ -> Nothing

--------------------------------------------------------------------------------
-- Global Options
--------------------------------------------------------------------------------

parseGlobalOptionsSpec :: Spec
parseGlobalOptionsSpec = describe "parseGlobalOptions" $ do
  describe "default values" $ do
    it "has no config file by default" $ do
      case getCommand ["check", "."] of
        Just (CmdCheck global _) -> goConfigFile global `shouldBe` Nothing
        _ -> expectationFailure "Expected CmdCheck"

    it "has normal verbosity by default" $ do
      case getCommand ["check", "."] of
        Just (CmdCheck global _) -> goVerbosity global `shouldBe` Normal
        _ -> expectationFailure "Expected CmdCheck"

    it "has color enabled by default" $ do
      case getCommand ["check", "."] of
        Just (CmdCheck global _) -> goNoColor global `shouldBe` False
        _ -> expectationFailure "Expected CmdCheck"

    it "has 4 parallel jobs by default" $ do
      case getCommand ["check", "."] of
        Just (CmdCheck global _) -> goParallel global `shouldBe` 4
        _ -> expectationFailure "Expected CmdCheck"

  describe "custom values" $ do
    it "parses config file with -c" $ do
      case getCommand ["check", "-c", "custom.toml", "."] of
        Just (CmdCheck global _) -> goConfigFile global `shouldBe` Just "custom.toml"
        _ -> expectationFailure "Expected CmdCheck"

    it "parses config file with --config" $ do
      case getCommand ["check", "--config", "my-config.toml", "."] of
        Just (CmdCheck global _) -> goConfigFile global `shouldBe` Just "my-config.toml"
        _ -> expectationFailure "Expected CmdCheck"

    it "enables verbose with -v" $ do
      case getCommand ["check", "-v", "."] of
        Just (CmdCheck global _) -> goVerbosity global `shouldBe` Verbose
        _ -> expectationFailure "Expected CmdCheck"

    it "enables verbose with --verbose" $ do
      case getCommand ["check", "--verbose", "."] of
        Just (CmdCheck global _) -> goVerbosity global `shouldBe` Verbose
        _ -> expectationFailure "Expected CmdCheck"

    it "disables color with --no-color" $ do
      case getCommand ["check", "--no-color", "."] of
        Just (CmdCheck global _) -> goNoColor global `shouldBe` True
        _ -> expectationFailure "Expected CmdCheck"

    it "sets parallel jobs with -j" $ do
      case getCommand ["check", "-j", "8", "."] of
        Just (CmdCheck global _) -> goParallel global `shouldBe` 8
        _ -> expectationFailure "Expected CmdCheck"

    it "sets parallel jobs with --parallel" $ do
      case getCommand ["check", "--parallel", "16", "."] of
        Just (CmdCheck global _) -> goParallel global `shouldBe` 16
        _ -> expectationFailure "Expected CmdCheck"

--------------------------------------------------------------------------------
-- Check Command
--------------------------------------------------------------------------------

parseCheckOptionsSpec :: Spec
parseCheckOptionsSpec = describe "parseCheckOptions" $ do
  describe "default values" $ do
    it "uses current directory when no targets specified" $ do
      parseSuccess ["check"] `shouldBe` True

    it "defaults to quick mode" $ do
      case getCommand ["check", "."] of
        Just (CmdCheck _ opts) -> coMode opts `shouldBe` "quick"
        _ -> expectationFailure "Expected CmdCheck"

    it "defaults to terminal format" $ do
      case getCommand ["check", "."] of
        Just (CmdCheck _ opts) -> coOutputFormat opts `shouldBe` "terminal"
        _ -> expectationFailure "Expected CmdCheck"

    it "defaults to grouping by file" $ do
      case getCommand ["check", "."] of
        Just (CmdCheck _ opts) -> coGroupBy opts `shouldBe` "file"
        _ -> expectationFailure "Expected CmdCheck"

    it "defaults to 2 context lines" $ do
      case getCommand ["check", "."] of
        Just (CmdCheck _ opts) -> coContextLines opts `shouldBe` 2
        _ -> expectationFailure "Expected CmdCheck"

  describe "targets" $ do
    it "parses single target" $ do
      case getCommand ["check", "src/"] of
        Just (CmdCheck _ opts) -> coTargets opts `shouldBe` ["src/"]
        _ -> expectationFailure "Expected CmdCheck"

    it "parses multiple targets" $ do
      case getCommand ["check", "src/", "app/", "lib/"] of
        Just (CmdCheck _ opts) -> coTargets opts `shouldBe` ["src/", "app/", "lib/"]
        _ -> expectationFailure "Expected CmdCheck"

    it "parses file targets" $ do
      case getCommand ["check", "Main.hs", "Lib.hs"] of
        Just (CmdCheck _ opts) -> coTargets opts `shouldBe` ["Main.hs", "Lib.hs"]
        _ -> expectationFailure "Expected CmdCheck"

  describe "mode option" $ do
    it "parses quick mode" $ do
      case getCommand ["check", "--mode", "quick", "."] of
        Just (CmdCheck _ opts) -> coMode opts `shouldBe` "quick"
        _ -> expectationFailure "Expected CmdCheck"

    it "parses full mode" $ do
      case getCommand ["check", "--mode", "full", "."] of
        Just (CmdCheck _ opts) -> coMode opts `shouldBe` "full"
        _ -> expectationFailure "Expected CmdCheck"

    it "parses plugin mode" $ do
      case getCommand ["check", "--mode", "plugin", "."] of
        Just (CmdCheck _ opts) -> coMode opts `shouldBe` "plugin"
        _ -> expectationFailure "Expected CmdCheck"

    it "parses mode with short flag" $ do
      case getCommand ["check", "-m", "full", "."] of
        Just (CmdCheck _ opts) -> coMode opts `shouldBe` "full"
        _ -> expectationFailure "Expected CmdCheck"

  describe "output format option" $ do
    it "parses terminal format" $ do
      case getCommand ["check", "--format", "terminal", "."] of
        Just (CmdCheck _ opts) -> coOutputFormat opts `shouldBe` "terminal"
        _ -> expectationFailure "Expected CmdCheck"

    it "parses json format" $ do
      case getCommand ["check", "--format", "json", "."] of
        Just (CmdCheck _ opts) -> coOutputFormat opts `shouldBe` "json"
        _ -> expectationFailure "Expected CmdCheck"

    it "parses sarif format" $ do
      case getCommand ["check", "--format", "sarif", "."] of
        Just (CmdCheck _ opts) -> coOutputFormat opts `shouldBe` "sarif"
        _ -> expectationFailure "Expected CmdCheck"

    it "parses html format" $ do
      case getCommand ["check", "--format", "html", "."] of
        Just (CmdCheck _ opts) -> coOutputFormat opts `shouldBe` "html"
        _ -> expectationFailure "Expected CmdCheck"

    it "parses junit format" $ do
      case getCommand ["check", "--format", "junit", "."] of
        Just (CmdCheck _ opts) -> coOutputFormat opts `shouldBe` "junit"
        _ -> expectationFailure "Expected CmdCheck"

    it "parses codeclimate format" $ do
      case getCommand ["check", "--format", "codeclimate", "."] of
        Just (CmdCheck _ opts) -> coOutputFormat opts `shouldBe` "codeclimate"
        _ -> expectationFailure "Expected CmdCheck"

    it "parses checkstyle format" $ do
      case getCommand ["check", "--format", "checkstyle", "."] of
        Just (CmdCheck _ opts) -> coOutputFormat opts `shouldBe` "checkstyle"
        _ -> expectationFailure "Expected CmdCheck"

    it "parses format with short flag" $ do
      case getCommand ["check", "-f", "json", "."] of
        Just (CmdCheck _ opts) -> coOutputFormat opts `shouldBe` "json"
        _ -> expectationFailure "Expected CmdCheck"

  describe "hie directory option" $ do
    it "defaults to Nothing" $ do
      case getCommand ["check", "."] of
        Just (CmdCheck _ opts) -> coHieDir opts `shouldBe` Nothing
        _ -> expectationFailure "Expected CmdCheck"

    it "parses hie directory" $ do
      case getCommand ["check", "--hie-dir", ".hie", "."] of
        Just (CmdCheck _ opts) -> coHieDir opts `shouldBe` Just ".hie"
        _ -> expectationFailure "Expected CmdCheck"

  describe "context options" $ do
    it "parses context flag" $ do
      case getCommand ["check", "--context", "."] of
        Just (CmdCheck _ opts) -> coShowContext opts `shouldBe` True
        _ -> expectationFailure "Expected CmdCheck"

    it "parses context lines" $ do
      case getCommand ["check", "--context-lines", "5", "."] of
        Just (CmdCheck _ opts) -> coContextLines opts `shouldBe` 5
        _ -> expectationFailure "Expected CmdCheck"

--------------------------------------------------------------------------------
-- Fix Command
--------------------------------------------------------------------------------

parseFixOptionsSpec :: Spec
parseFixOptionsSpec = describe "parseFixOptions" $ do
  describe "default values" $ do
    it "defaults to non-interactive" $ do
      case getCommand ["fix", "."] of
        Just (CmdFix _ opts) -> foInteractive opts `shouldBe` False
        _ -> expectationFailure "Expected CmdFix"

    it "defaults to safe-only" $ do
      case getCommand ["fix", "."] of
        Just (CmdFix _ opts) -> foSafeOnly opts `shouldBe` True
        _ -> expectationFailure "Expected CmdFix"

    it "defaults to backup enabled" $ do
      case getCommand ["fix", "."] of
        Just (CmdFix _ opts) -> foBackup opts `shouldBe` True
        _ -> expectationFailure "Expected CmdFix"

    it "defaults to validation enabled" $ do
      case getCommand ["fix", "."] of
        Just (CmdFix _ opts) -> foValidate opts `shouldBe` True
        _ -> expectationFailure "Expected CmdFix"

    it "defaults to transactional mode" $ do
      case getCommand ["fix", "."] of
        Just (CmdFix _ opts) -> foTransactional opts `shouldBe` True
        _ -> expectationFailure "Expected CmdFix"

    it "defaults to SyntaxValidation level" $ do
      case getCommand ["fix", "."] of
        Just (CmdFix _ opts) -> foValidateLevel opts `shouldBe` SyntaxValidation
        _ -> expectationFailure "Expected CmdFix"

    it "defaults to PreferPreferred conflict strategy" $ do
      case getCommand ["fix", "."] of
        Just (CmdFix _ opts) -> foConflictStrategy opts `shouldBe` PreferPreferred
        _ -> expectationFailure "Expected CmdFix"

    it "defaults to showing diff" $ do
      case getCommand ["fix", "."] of
        Just (CmdFix _ opts) -> foShowDiff opts `shouldBe` True
        _ -> expectationFailure "Expected CmdFix"

  describe "interactive mode" $ do
    it "enables interactive with -i" $ do
      case getCommand ["fix", "-i", "."] of
        Just (CmdFix _ opts) -> foInteractive opts `shouldBe` True
        _ -> expectationFailure "Expected CmdFix"

    it "enables interactive with --interactive" $ do
      case getCommand ["fix", "--interactive", "."] of
        Just (CmdFix _ opts) -> foInteractive opts `shouldBe` True
        _ -> expectationFailure "Expected CmdFix"

  describe "preview/dry-run modes" $ do
    it "enables preview with -p" $ do
      case getCommand ["fix", "-p", "."] of
        Just (CmdFix _ opts) -> foPreview opts `shouldBe` True
        _ -> expectationFailure "Expected CmdFix"

    it "enables preview with --preview" $ do
      case getCommand ["fix", "--preview", "."] of
        Just (CmdFix _ opts) -> foPreview opts `shouldBe` True
        _ -> expectationFailure "Expected CmdFix"

    it "enables dry-run with -n" $ do
      case getCommand ["fix", "-n", "."] of
        Just (CmdFix _ opts) -> foDryRun opts `shouldBe` True
        _ -> expectationFailure "Expected CmdFix"

    it "enables dry-run with --dry-run" $ do
      case getCommand ["fix", "--dry-run", "."] of
        Just (CmdFix _ opts) -> foDryRun opts `shouldBe` True
        _ -> expectationFailure "Expected CmdFix"

  describe "safety options" $ do
    it "allows unsafe with --unsafe" $ do
      case getCommand ["fix", "--unsafe", "."] of
        Just (CmdFix _ opts) -> foSafeOnly opts `shouldBe` False
        _ -> expectationFailure "Expected CmdFix"

    it "disables backup with --no-backup" $ do
      case getCommand ["fix", "--no-backup", "."] of
        Just (CmdFix _ opts) -> foBackup opts `shouldBe` False
        _ -> expectationFailure "Expected CmdFix"

    it "disables validation with --no-validate" $ do
      case getCommand ["fix", "--no-validate", "."] of
        Just (CmdFix _ opts) -> foValidate opts `shouldBe` False
        _ -> expectationFailure "Expected CmdFix"

    it "disables transactional with --no-transactional" $ do
      case getCommand ["fix", "--no-transactional", "."] of
        Just (CmdFix _ opts) -> foTransactional opts `shouldBe` False
        _ -> expectationFailure "Expected CmdFix"

  describe "validation level" $ do
    it "parses none validation level" $ do
      case getCommand ["fix", "--validate-level", "none", "."] of
        Just (CmdFix _ opts) -> foValidateLevel opts `shouldBe` NoValidation
        _ -> expectationFailure "Expected CmdFix"

    it "parses structural validation level" $ do
      case getCommand ["fix", "--validate-level", "structural", "."] of
        Just (CmdFix _ opts) -> foValidateLevel opts `shouldBe` StructuralOnly
        _ -> expectationFailure "Expected CmdFix"

    it "parses syntax validation level" $ do
      case getCommand ["fix", "--validate-level", "syntax", "."] of
        Just (CmdFix _ opts) -> foValidateLevel opts `shouldBe` SyntaxValidation
        _ -> expectationFailure "Expected CmdFix"

    it "parses semantic validation level" $ do
      case getCommand ["fix", "--validate-level", "semantic", "."] of
        Just (CmdFix _ opts) -> foValidateLevel opts `shouldBe` SemanticValidation
        _ -> expectationFailure "Expected CmdFix"

  describe "conflict strategy" $ do
    it "parses skip conflict strategy" $ do
      case getCommand ["fix", "--conflict-strategy", "skip", "."] of
        Just (CmdFix _ opts) -> foConflictStrategy opts `shouldBe` SkipAll
        _ -> expectationFailure "Expected CmdFix"

    it "parses preferred conflict strategy" $ do
      case getCommand ["fix", "--conflict-strategy", "preferred", "."] of
        Just (CmdFix _ opts) -> foConflictStrategy opts `shouldBe` PreferPreferred
        _ -> expectationFailure "Expected CmdFix"

    it "parses first conflict strategy" $ do
      case getCommand ["fix", "--conflict-strategy", "first", "."] of
        Just (CmdFix _ opts) -> foConflictStrategy opts `shouldBe` SkipSecond
        _ -> expectationFailure "Expected CmdFix"

    it "parses severity conflict strategy" $ do
      case getCommand ["fix", "--conflict-strategy", "severity", "."] of
        Just (CmdFix _ opts) -> foConflictStrategy opts `shouldBe` PreferSeverity
        _ -> expectationFailure "Expected CmdFix"

    it "parses smaller conflict strategy" $ do
      case getCommand ["fix", "--conflict-strategy", "smaller", "."] of
        Just (CmdFix _ opts) -> foConflictStrategy opts `shouldBe` PreferSmaller
        _ -> expectationFailure "Expected CmdFix"

  describe "rule filter" $ do
    it "parses rule filter with -r" $ do
      case getCommand ["fix", "-r", "partial-head", "."] of
        Just (CmdFix _ opts) -> foRule opts `shouldBe` Just "partial-head"
        _ -> expectationFailure "Expected CmdFix"

    it "parses rule filter with --rule" $ do
      case getCommand ["fix", "--rule", "foldl-strict", "."] of
        Just (CmdFix _ opts) -> foRule opts `shouldBe` Just "foldl-strict"
        _ -> expectationFailure "Expected CmdFix"

--------------------------------------------------------------------------------
-- Unused Command
--------------------------------------------------------------------------------

parseUnusedOptionsSpec :: Spec
parseUnusedOptionsSpec = describe "parseUnusedOptions" $ do
  describe "default values" $ do
    it "defaults to full mode" $ do
      case getCommand ["unused", "."] of
        Just (CmdUnused _ opts) -> uoMode opts `shouldBe` "full"
        _ -> expectationFailure "Expected CmdUnused"

    it "defaults to terminal format" $ do
      case getCommand ["unused", "."] of
        Just (CmdUnused _ opts) -> uoOutputFormat opts `shouldBe` "terminal"
        _ -> expectationFailure "Expected CmdUnused"

    it "defaults to no roots" $ do
      case getCommand ["unused", "."] of
        Just (CmdUnused _ opts) -> uoRoots opts `shouldBe` []
        _ -> expectationFailure "Expected CmdUnused"

  describe "roots" $ do
    it "parses single root" $ do
      case getCommand ["unused", "--root", "^Main.main$", "."] of
        Just (CmdUnused _ opts) -> uoRoots opts `shouldBe` ["^Main.main$"]
        _ -> expectationFailure "Expected CmdUnused"

    it "parses multiple roots" $ do
      case getCommand ["unused", "--root", "^Main.main$", "--root", "^Paths_", "."] of
        Just (CmdUnused _ opts) -> uoRoots opts `shouldBe` ["^Main.main$", "^Paths_"]
        _ -> expectationFailure "Expected CmdUnused"

--------------------------------------------------------------------------------
-- Init Command
--------------------------------------------------------------------------------

parseInitOptionsSpec :: Spec
parseInitOptionsSpec = describe "parseInitOptions" $ do
  it "defaults to not forcing" $ do
    case getCommand ["init"] of
      Just (CmdInit _ opts) -> ioForce opts `shouldBe` False
      _ -> expectationFailure "Expected CmdInit"

  it "enables force with -f" $ do
    case getCommand ["init", "-f"] of
      Just (CmdInit _ opts) -> ioForce opts `shouldBe` True
      _ -> expectationFailure "Expected CmdInit"

  it "enables force with --force" $ do
    case getCommand ["init", "--force"] of
      Just (CmdInit _ opts) -> ioForce opts `shouldBe` True
      _ -> expectationFailure "Expected CmdInit"

--------------------------------------------------------------------------------
-- Index Command
--------------------------------------------------------------------------------

parseIndexOptionsSpec :: Spec
parseIndexOptionsSpec = describe "parseIndexOptions" $ do
  describe "default values" $ do
    it "defaults to no target directory" $ do
      case getCommand ["index"] of
        Just (CmdIndex _ opts) -> ixTargetDir opts `shouldBe` Nothing
        _ -> expectationFailure "Expected CmdIndex"

    it "defaults to building" $ do
      case getCommand ["index"] of
        Just (CmdIndex _ opts) -> ixBuild opts `shouldBe` True
        _ -> expectationFailure "Expected CmdIndex"

    it "defaults to not cleaning" $ do
      case getCommand ["index"] of
        Just (CmdIndex _ opts) -> ixClean opts `shouldBe` False
        _ -> expectationFailure "Expected CmdIndex"

    it "defaults to creating symlink" $ do
      case getCommand ["index"] of
        Just (CmdIndex _ opts) -> ixSymlink opts `shouldBe` True
        _ -> expectationFailure "Expected CmdIndex"

  describe "custom values" $ do
    it "parses target directory" $ do
      case getCommand ["index", "myproject/"] of
        Just (CmdIndex _ opts) -> ixTargetDir opts `shouldBe` Just "myproject/"
        _ -> expectationFailure "Expected CmdIndex"

    it "disables build with --no-build" $ do
      case getCommand ["index", "--no-build"] of
        Just (CmdIndex _ opts) -> ixBuild opts `shouldBe` False
        _ -> expectationFailure "Expected CmdIndex"

    it "enables clean with --clean" $ do
      case getCommand ["index", "--clean"] of
        Just (CmdIndex _ opts) -> ixClean opts `shouldBe` True
        _ -> expectationFailure "Expected CmdIndex"

    it "parses custom db path" $ do
      case getCommand ["index", "--db", "custom.hiedb"] of
        Just (CmdIndex _ opts) -> ixDbPath opts `shouldBe` Just "custom.hiedb"
        _ -> expectationFailure "Expected CmdIndex"

    it "disables symlink with --no-symlink" $ do
      case getCommand ["index", "--no-symlink"] of
        Just (CmdIndex _ opts) -> ixSymlink opts `shouldBe` False
        _ -> expectationFailure "Expected CmdIndex"

    it "enables auto-accept with -y" $ do
      case getCommand ["index", "-y"] of
        Just (CmdIndex _ opts) -> ixYes opts `shouldBe` True
        _ -> expectationFailure "Expected CmdIndex"

--------------------------------------------------------------------------------
-- Watch Command
--------------------------------------------------------------------------------

parseWatchOptionsSpec :: Spec
parseWatchOptionsSpec = describe "parseWatchOptions" $ do
  describe "default values" $ do
    it "defaults to 500ms debounce" $ do
      case getCommand ["watch"] of
        Just (CmdWatch _ opts) -> woDebounceMs opts `shouldBe` 500
        _ -> expectationFailure "Expected CmdWatch"

    it "defaults to 1000ms poll interval" $ do
      case getCommand ["watch"] of
        Just (CmdWatch _ opts) -> woPollIntervalMs opts `shouldBe` 1000
        _ -> expectationFailure "Expected CmdWatch"

    it "defaults to clearing screen" $ do
      case getCommand ["watch"] of
        Just (CmdWatch _ opts) -> woClearScreen opts `shouldBe` True
        _ -> expectationFailure "Expected CmdWatch"

    it "defaults to showing timestamp" $ do
      case getCommand ["watch"] of
        Just (CmdWatch _ opts) -> woShowTimestamp opts `shouldBe` True
        _ -> expectationFailure "Expected CmdWatch"

  describe "custom values" $ do
    it "parses target directories" $ do
      case getCommand ["watch", "src/", "app/"] of
        Just (CmdWatch _ opts) -> woTargets opts `shouldBe` ["src/", "app/"]
        _ -> expectationFailure "Expected CmdWatch"

    it "parses custom debounce" $ do
      case getCommand ["watch", "--debounce", "1000"] of
        Just (CmdWatch _ opts) -> woDebounceMs opts `shouldBe` 1000
        _ -> expectationFailure "Expected CmdWatch"

    it "parses custom poll interval" $ do
      case getCommand ["watch", "--poll-interval", "2000"] of
        Just (CmdWatch _ opts) -> woPollIntervalMs opts `shouldBe` 2000
        _ -> expectationFailure "Expected CmdWatch"

    it "disables clear screen with --no-clear" $ do
      case getCommand ["watch", "--no-clear"] of
        Just (CmdWatch _ opts) -> woClearScreen opts `shouldBe` False
        _ -> expectationFailure "Expected CmdWatch"

    it "disables timestamp with --no-timestamp" $ do
      case getCommand ["watch", "--no-timestamp"] of
        Just (CmdWatch _ opts) -> woShowTimestamp opts `shouldBe` False
        _ -> expectationFailure "Expected CmdWatch"

--------------------------------------------------------------------------------
-- Diff Command
--------------------------------------------------------------------------------

parseDiffOptionsSpec :: Spec
parseDiffOptionsSpec = describe "parseDiffOptions" $ do
  describe "default values" $ do
    it "defaults to terminal format" $ do
      case getCommand ["diff", "."] of
        Just (CmdDiff _ opts) -> dfOutputFormat opts `shouldBe` "terminal"
        _ -> expectationFailure "Expected CmdDiff"

    it "defaults to showing all issues" $ do
      case getCommand ["diff", "."] of
        Just (CmdDiff _ opts) -> do
          dfShowNew opts `shouldBe` False
          dfShowFixed opts `shouldBe` False
        _ -> expectationFailure "Expected CmdDiff"

  describe "baseline comparison" $ do
    it "parses baseline file with -b" $ do
      case getCommand ["diff", "-b", "baseline.json", "."] of
        Just (CmdDiff _ opts) -> dfBaseline opts `shouldBe` Just "baseline.json"
        _ -> expectationFailure "Expected CmdDiff"

    it "parses baseline file with --baseline" $ do
      case getCommand ["diff", "--baseline", "old.json", "."] of
        Just (CmdDiff _ opts) -> dfBaseline opts `shouldBe` Just "old.json"
        _ -> expectationFailure "Expected CmdDiff"

  describe "git comparison" $ do
    it "parses git ref with -g" $ do
      case getCommand ["diff", "-g", "HEAD~1", "."] of
        Just (CmdDiff _ opts) -> dfGitRef opts `shouldBe` Just "HEAD~1"
        _ -> expectationFailure "Expected CmdDiff"

    it "parses git ref with --git-ref" $ do
      case getCommand ["diff", "--git-ref", "main", "."] of
        Just (CmdDiff _ opts) -> dfGitRef opts `shouldBe` Just "main"
        _ -> expectationFailure "Expected CmdDiff"

  describe "filter options" $ do
    it "enables new-only filter" $ do
      case getCommand ["diff", "--new-only", "."] of
        Just (CmdDiff _ opts) -> dfShowNew opts `shouldBe` True
        _ -> expectationFailure "Expected CmdDiff"

    it "enables fixed-only filter" $ do
      case getCommand ["diff", "--fixed-only", "."] of
        Just (CmdDiff _ opts) -> dfShowFixed opts `shouldBe` True
        _ -> expectationFailure "Expected CmdDiff"

--------------------------------------------------------------------------------
-- Baseline Command
--------------------------------------------------------------------------------

parseBaselineOptionsSpec :: Spec
parseBaselineOptionsSpec = describe "parseBaselineOptions" $ do
  describe "default values" $ do
    it "defaults to .argus-baseline.json output file" $ do
      case getCommand ["baseline", "."] of
        Just (CmdBaseline _ opts) -> blOutputFile opts `shouldBe` ".argus-baseline.json"
        _ -> expectationFailure "Expected CmdBaseline"

    it "defaults to not forcing overwrite" $ do
      case getCommand ["baseline", "."] of
        Just (CmdBaseline _ opts) -> blForce opts `shouldBe` False
        _ -> expectationFailure "Expected CmdBaseline"

  describe "custom values" $ do
    it "parses custom output file with -o" $ do
      case getCommand ["baseline", "-o", "my-baseline.json", "."] of
        Just (CmdBaseline _ opts) -> blOutputFile opts `shouldBe` "my-baseline.json"
        _ -> expectationFailure "Expected CmdBaseline"

    it "parses custom output file with --output" $ do
      case getCommand ["baseline", "--output", "custom.json", "."] of
        Just (CmdBaseline _ opts) -> blOutputFile opts `shouldBe` "custom.json"
        _ -> expectationFailure "Expected CmdBaseline"

    it "enables force with -f" $ do
      case getCommand ["baseline", "-f", "."] of
        Just (CmdBaseline _ opts) -> blForce opts `shouldBe` True
        _ -> expectationFailure "Expected CmdBaseline"

--------------------------------------------------------------------------------
-- Stats Command
--------------------------------------------------------------------------------

parseStatsOptionsSpec :: Spec
parseStatsOptionsSpec = describe "parseStatsOptions" $ do
  describe "default values" $ do
    it "defaults to terminal format" $ do
      case getCommand ["stats", "."] of
        Just (CmdStats _ opts) -> stOutputFormat opts `shouldBe` "terminal"
        _ -> expectationFailure "Expected CmdStats"

    it "defaults to no grouping" $ do
      case getCommand ["stats", "."] of
        Just (CmdStats _ opts) -> do
          stByRule opts `shouldBe` False
          stByFile opts `shouldBe` False
          stBySeverity opts `shouldBe` False
        _ -> expectationFailure "Expected CmdStats"

  describe "grouping options" $ do
    it "enables by-rule grouping" $ do
      case getCommand ["stats", "--by-rule", "."] of
        Just (CmdStats _ opts) -> stByRule opts `shouldBe` True
        _ -> expectationFailure "Expected CmdStats"

    it "enables by-file grouping" $ do
      case getCommand ["stats", "--by-file", "."] of
        Just (CmdStats _ opts) -> stByFile opts `shouldBe` True
        _ -> expectationFailure "Expected CmdStats"

    it "enables by-severity grouping" $ do
      case getCommand ["stats", "--by-severity", "."] of
        Just (CmdStats _ opts) -> stBySeverity opts `shouldBe` True
        _ -> expectationFailure "Expected CmdStats"

--------------------------------------------------------------------------------
-- Daemon Command
--------------------------------------------------------------------------------

parseDaemonOptionsSpec :: Spec
parseDaemonOptionsSpec = describe "parseDaemonOptions" $ do
  describe "actions" $ do
    it "parses start action" $ do
      case getCommand ["daemon", "start"] of
        Just (CmdDaemon _ opts) -> daAction opts `shouldBe` DaemonStart
        _ -> expectationFailure "Expected CmdDaemon"

    it "parses stop action" $ do
      case getCommand ["daemon", "stop"] of
        Just (CmdDaemon _ opts) -> daAction opts `shouldBe` DaemonStop
        _ -> expectationFailure "Expected CmdDaemon"

    it "parses status action" $ do
      case getCommand ["daemon", "status"] of
        Just (CmdDaemon _ opts) -> daAction opts `shouldBe` DaemonStatus
        _ -> expectationFailure "Expected CmdDaemon"

    it "parses reload action" $ do
      case getCommand ["daemon", "reload"] of
        Just (CmdDaemon _ opts) -> daAction opts `shouldBe` DaemonReload
        _ -> expectationFailure "Expected CmdDaemon"

    it "parses check action with files" $ do
      case getCommand ["daemon", "check", "src/Main.hs", "src/Lib.hs"] of
        Just (CmdDaemon _ opts) -> daAction opts `shouldBe` DaemonCheck ["src/Main.hs", "src/Lib.hs"]
        _ -> expectationFailure "Expected CmdDaemon"

  describe "options" $ do
    it "parses socket path" $ do
      case getCommand ["daemon", "--socket", "/tmp/argus.sock", "start"] of
        Just (CmdDaemon _ opts) -> daSocketPath opts `shouldBe` Just "/tmp/argus.sock"
        _ -> expectationFailure "Expected CmdDaemon"

    it "parses port" $ do
      case getCommand ["daemon", "--port", "9999", "start"] of
        Just (CmdDaemon _ opts) -> daPort opts `shouldBe` Just 9999
        _ -> expectationFailure "Expected CmdDaemon"

    it "parses idle timeout" $ do
      case getCommand ["daemon", "--idle-timeout", "300", "start"] of
        Just (CmdDaemon _ opts) -> daIdleTimeout opts `shouldBe` Just 300
        _ -> expectationFailure "Expected CmdDaemon"

    it "global -v before subcommand sets global verbose" $ do
      -- -v before subcommand is captured by global options (daemon has own --verbose
      -- but it must come before subcommand where it conflicts with global -v)
      case getCommand ["daemon", "-v", "start"] of
        Just (CmdDaemon globalOpts _) -> goVerbosity globalOpts `shouldBe` Verbose
        _ -> expectationFailure "Expected CmdDaemon"

--------------------------------------------------------------------------------
-- LSP Command
--------------------------------------------------------------------------------

parseLspOptionsSpec :: Spec
parseLspOptionsSpec = describe "parseLspOptions" $ do
  describe "default values" $ do
    it "defaults to no debug log" $ do
      case getCommand ["lsp"] of
        Just (CmdLsp _ opts) -> loDebugLog opts `shouldBe` Nothing
        _ -> expectationFailure "Expected CmdLsp"

    it "defaults to not analyzing on change" $ do
      case getCommand ["lsp"] of
        Just (CmdLsp _ opts) -> loAnalyzeOnChange opts `shouldBe` False
        _ -> expectationFailure "Expected CmdLsp"

    it "defaults to 500ms debounce" $ do
      case getCommand ["lsp"] of
        Just (CmdLsp _ opts) -> loDebounceMs opts `shouldBe` 500
        _ -> expectationFailure "Expected CmdLsp"

    it "defaults to progress reporting enabled" $ do
      case getCommand ["lsp"] of
        Just (CmdLsp _ opts) -> loProgressReporting opts `shouldBe` True
        _ -> expectationFailure "Expected CmdLsp"

  describe "custom values" $ do
    it "parses debug log file" $ do
      case getCommand ["lsp", "--debug-log", "/tmp/argus-lsp.log"] of
        Just (CmdLsp _ opts) -> loDebugLog opts `shouldBe` Just "/tmp/argus-lsp.log"
        _ -> expectationFailure "Expected CmdLsp"

    it "enables analyze on change" $ do
      case getCommand ["lsp", "--analyze-on-change"] of
        Just (CmdLsp _ opts) -> loAnalyzeOnChange opts `shouldBe` True
        _ -> expectationFailure "Expected CmdLsp"

    it "parses custom debounce" $ do
      case getCommand ["lsp", "--debounce", "1000"] of
        Just (CmdLsp _ opts) -> loDebounceMs opts `shouldBe` 1000
        _ -> expectationFailure "Expected CmdLsp"

    it "disables progress reporting" $ do
      case getCommand ["lsp", "--no-progress"] of
        Just (CmdLsp _ opts) -> loProgressReporting opts `shouldBe` False
        _ -> expectationFailure "Expected CmdLsp"

--------------------------------------------------------------------------------
-- Architecture Command
--------------------------------------------------------------------------------

parseArchitectureOptionsSpec :: Spec
parseArchitectureOptionsSpec = describe "parseArchitectureOptions" $ do
  describe "default values" $ do
    it "defaults to terminal format" $ do
      case getCommand ["architecture", "."] of
        Just (CmdArchitecture _ opts) -> aoOutputFormat opts `shouldBe` "terminal"
        _ -> expectationFailure "Expected CmdArchitecture"

    it "defaults to not showing graph" $ do
      case getCommand ["architecture", "."] of
        Just (CmdArchitecture _ opts) -> aoShowGraph opts `shouldBe` False
        _ -> expectationFailure "Expected CmdArchitecture"

    it "defaults to showing metrics" $ do
      case getCommand ["architecture", "."] of
        Just (CmdArchitecture _ opts) -> aoShowMetrics opts `shouldBe` True
        _ -> expectationFailure "Expected CmdArchitecture"

    it "defaults to showing violations" $ do
      case getCommand ["architecture", "."] of
        Just (CmdArchitecture _ opts) -> aoShowViolations opts `shouldBe` True
        _ -> expectationFailure "Expected CmdArchitecture"

    it "defaults to showing cycles" $ do
      case getCommand ["architecture", "."] of
        Just (CmdArchitecture _ opts) -> aoShowCycles opts `shouldBe` True
        _ -> expectationFailure "Expected CmdArchitecture"

  describe "custom values" $ do
    it "parses dot format" $ do
      case getCommand ["architecture", "--format", "dot", "."] of
        Just (CmdArchitecture _ opts) -> aoOutputFormat opts `shouldBe` "dot"
        _ -> expectationFailure "Expected CmdArchitecture"

    it "parses json format" $ do
      case getCommand ["architecture", "--format", "json", "."] of
        Just (CmdArchitecture _ opts) -> aoOutputFormat opts `shouldBe` "json"
        _ -> expectationFailure "Expected CmdArchitecture"

    it "enables graph with -g" $ do
      case getCommand ["architecture", "-g", "."] of
        Just (CmdArchitecture _ opts) -> aoShowGraph opts `shouldBe` True
        _ -> expectationFailure "Expected CmdArchitecture"

    it "enables graph with --graph" $ do
      case getCommand ["architecture", "--graph", "."] of
        Just (CmdArchitecture _ opts) -> aoShowGraph opts `shouldBe` True
        _ -> expectationFailure "Expected CmdArchitecture"

    it "disables metrics with --no-metrics" $ do
      case getCommand ["architecture", "--no-metrics", "."] of
        Just (CmdArchitecture _ opts) -> aoShowMetrics opts `shouldBe` False
        _ -> expectationFailure "Expected CmdArchitecture"

    it "disables violations with --no-violations" $ do
      case getCommand ["architecture", "--no-violations", "."] of
        Just (CmdArchitecture _ opts) -> aoShowViolations opts `shouldBe` False
        _ -> expectationFailure "Expected CmdArchitecture"

    it "disables cycles with --no-cycles" $ do
      case getCommand ["architecture", "--no-cycles", "."] of
        Just (CmdArchitecture _ opts) -> aoShowCycles opts `shouldBe` False
        _ -> expectationFailure "Expected CmdArchitecture"

    it "parses graph output file with -o" $ do
      case getCommand ["architecture", "-o", "deps.dot", "."] of
        Just (CmdArchitecture _ opts) -> aoGraphOutput opts `shouldBe` Just "deps.dot"
        _ -> expectationFailure "Expected CmdArchitecture"

--------------------------------------------------------------------------------
-- Full Command Parsing
--------------------------------------------------------------------------------

parseCommandSpec :: Spec
parseCommandSpec = describe "parseCommand" $ do
  describe "command recognition" $ do
    it "recognizes check command" $ do
      case getCommand ["check", "."] of
        Just (CmdCheck _ _) -> pure ()
        _ -> expectationFailure "Expected CmdCheck"

    it "recognizes fix command" $ do
      case getCommand ["fix", "."] of
        Just (CmdFix _ _) -> pure ()
        _ -> expectationFailure "Expected CmdFix"

    it "recognizes unused command" $ do
      case getCommand ["unused", "."] of
        Just (CmdUnused _ _) -> pure ()
        _ -> expectationFailure "Expected CmdUnused"

    it "recognizes init command" $ do
      case getCommand ["init"] of
        Just (CmdInit _ _) -> pure ()
        _ -> expectationFailure "Expected CmdInit"

    it "recognizes index command" $ do
      case getCommand ["index"] of
        Just (CmdIndex _ _) -> pure ()
        _ -> expectationFailure "Expected CmdIndex"

    it "recognizes watch command" $ do
      case getCommand ["watch"] of
        Just (CmdWatch _ _) -> pure ()
        _ -> expectationFailure "Expected CmdWatch"

    it "recognizes diff command" $ do
      case getCommand ["diff", "."] of
        Just (CmdDiff _ _) -> pure ()
        _ -> expectationFailure "Expected CmdDiff"

    it "recognizes baseline command" $ do
      case getCommand ["baseline", "."] of
        Just (CmdBaseline _ _) -> pure ()
        _ -> expectationFailure "Expected CmdBaseline"

    it "recognizes stats command" $ do
      case getCommand ["stats", "."] of
        Just (CmdStats _ _) -> pure ()
        _ -> expectationFailure "Expected CmdStats"

    it "recognizes daemon command" $ do
      case getCommand ["daemon", "start"] of
        Just (CmdDaemon _ _) -> pure ()
        _ -> expectationFailure "Expected CmdDaemon"

    it "recognizes lsp command" $ do
      case getCommand ["lsp"] of
        Just (CmdLsp _ _) -> pure ()
        _ -> expectationFailure "Expected CmdLsp"

    it "recognizes architecture command" $ do
      case getCommand ["architecture", "."] of
        Just (CmdArchitecture _ _) -> pure ()
        _ -> expectationFailure "Expected CmdArchitecture"

  describe "invalid commands" $ do
    it "fails on unknown command" $ do
      parseSuccess ["unknown"] `shouldBe` False

    it "fails on missing command" $ do
      parseSuccess [] `shouldBe` False

  describe "complex command combinations" $ do
    it "parses check with all options" $ do
      case getCommand ["check", "-v", "--no-color", "-j", "8", "-c", "config.toml",
                       "--mode", "full", "--format", "sarif", "--hie-dir", ".hie",
                       "--context", "--context-lines", "5", "src/", "app/"] of
        Just (CmdCheck global opts) -> do
          goVerbosity global `shouldBe` Verbose
          goNoColor global `shouldBe` True
          goParallel global `shouldBe` 8
          goConfigFile global `shouldBe` Just "config.toml"
          coMode opts `shouldBe` "full"
          coOutputFormat opts `shouldBe` "sarif"
          coHieDir opts `shouldBe` Just ".hie"
          coShowContext opts `shouldBe` True
          coContextLines opts `shouldBe` 5
          coTargets opts `shouldBe` ["src/", "app/"]
        _ -> expectationFailure "Expected CmdCheck"

    it "parses fix with all safety options" $ do
      case getCommand ["fix", "-i", "--unsafe", "--no-backup", "--no-validate",
                       "--no-transactional", "--validate-level", "semantic",
                       "--conflict-strategy", "severity", "--dry-run", "src/"] of
        Just (CmdFix global opts) -> do
          foInteractive opts `shouldBe` True
          foSafeOnly opts `shouldBe` False
          foBackup opts `shouldBe` False
          foValidate opts `shouldBe` False
          foTransactional opts `shouldBe` False
          foValidateLevel opts `shouldBe` SemanticValidation
          foConflictStrategy opts `shouldBe` PreferSeverity
          foDryRun opts `shouldBe` True
          foTargets opts `shouldBe` ["src/"]
        _ -> expectationFailure "Expected CmdFix"

--------------------------------------------------------------------------------
-- Pack Command
--------------------------------------------------------------------------------

parsePackOptionsSpec :: Spec
parsePackOptionsSpec = describe "parsePackOptions" $ do
  describe "actions" $ do
    it "parses list action" $ do
      case getCommand ["pack", "list"] of
        Just (CmdPack _ opts) -> poAction opts `shouldBe` PackList
        _ -> expectationFailure "Expected CmdPack"

    it "parses show action with pack name" $ do
      case getCommand ["pack", "show", "core"] of
        Just (CmdPack _ opts) -> poAction opts `shouldBe` PackShow "core"
        _ -> expectationFailure "Expected CmdPack"

    it "parses validate action without file" $ do
      case getCommand ["pack", "validate"] of
        Just (CmdPack _ opts) -> poAction opts `shouldBe` PackValidate Nothing
        _ -> expectationFailure "Expected CmdPack"

    it "parses validate action with file" $ do
      case getCommand ["pack", "validate", "mypack.toml"] of
        Just (CmdPack _ opts) -> poAction opts `shouldBe` PackValidate (Just "mypack.toml")
        _ -> expectationFailure "Expected CmdPack"

    it "parses export action" $ do
      case getCommand ["pack", "export", "core", "output.toml"] of
        Just (CmdPack _ opts) -> poAction opts `shouldBe` PackExport "core" "output.toml"
        _ -> expectationFailure "Expected CmdPack"

    it "parses import action" $ do
      case getCommand ["pack", "import", "mypack.toml"] of
        Just (CmdPack _ opts) -> poAction opts `shouldBe` PackImport "mypack.toml"
        _ -> expectationFailure "Expected CmdPack"

    it "parses create action" $ do
      case getCommand ["pack", "create", "my-custom-pack"] of
        Just (CmdPack _ opts) -> poAction opts `shouldBe` PackCreate "my-custom-pack"
        _ -> expectationFailure "Expected CmdPack"

  describe "options" $ do
    it "defaults to no version" $ do
      case getCommand ["pack", "list"] of
        Just (CmdPack _ opts) -> poVersion opts `shouldBe` Nothing
        _ -> expectationFailure "Expected CmdPack"

    it "parses version with -V" $ do
      case getCommand ["pack", "-V", "1.0.0", "export", "core", "out.toml"] of
        Just (CmdPack _ opts) -> poVersion opts `shouldBe` Just "1.0.0"
        _ -> expectationFailure "Expected CmdPack"

    it "parses version with --version" $ do
      case getCommand ["pack", "--version", "2.0.0", "export", "core", "out.toml"] of
        Just (CmdPack _ opts) -> poVersion opts `shouldBe` Just "2.0.0"
        _ -> expectationFailure "Expected CmdPack"

    it "parses author with -a" $ do
      case getCommand ["pack", "-a", "John Doe", "export", "core", "out.toml"] of
        Just (CmdPack _ opts) -> poAuthor opts `shouldBe` Just "John Doe"
        _ -> expectationFailure "Expected CmdPack"

    it "parses author with --author" $ do
      case getCommand ["pack", "--author", "Jane Doe", "export", "core", "out.toml"] of
        Just (CmdPack _ opts) -> poAuthor opts `shouldBe` Just "Jane Doe"
        _ -> expectationFailure "Expected CmdPack"

    it "defaults to no JSON output" $ do
      case getCommand ["pack", "list"] of
        Just (CmdPack _ opts) -> poOutputJson opts `shouldBe` False
        _ -> expectationFailure "Expected CmdPack"

    it "enables JSON output with --json" $ do
      case getCommand ["pack", "--json", "list"] of
        Just (CmdPack _ opts) -> poOutputJson opts `shouldBe` True
        _ -> expectationFailure "Expected CmdPack"

--------------------------------------------------------------------------------
-- CI Mode Options
--------------------------------------------------------------------------------

parseCIModeOptionsSpec :: Spec
parseCIModeOptionsSpec = describe "CI mode options" $ do
  describe "baseline options" $ do
    it "defaults to no baseline" $ do
      case getCommand ["check", "."] of
        Just (CmdCheck _ opts) -> coBaselinePath opts `shouldBe` Nothing
        _ -> expectationFailure "Expected CmdCheck"

    it "parses baseline with -b" $ do
      case getCommand ["check", "-b", "baseline.json", "."] of
        Just (CmdCheck _ opts) -> coBaselinePath opts `shouldBe` Just "baseline.json"
        _ -> expectationFailure "Expected CmdCheck"

    it "parses baseline with --baseline" $ do
      case getCommand ["check", "--baseline", "ci-baseline.json", "."] of
        Just (CmdCheck _ opts) -> coBaselinePath opts `shouldBe` Just "ci-baseline.json"
        _ -> expectationFailure "Expected CmdCheck"

  describe "fail options" $ do
    it "defaults to not failing on new" $ do
      case getCommand ["check", "."] of
        Just (CmdCheck _ opts) -> coFailOnNew opts `shouldBe` False
        _ -> expectationFailure "Expected CmdCheck"

    it "enables fail-on-new" $ do
      case getCommand ["check", "--fail-on-new", "."] of
        Just (CmdCheck _ opts) -> coFailOnNew opts `shouldBe` True
        _ -> expectationFailure "Expected CmdCheck"

    it "parses fail-on-severity" $ do
      case getCommand ["check", "--fail-on-severity", "warning", "."] of
        Just (CmdCheck _ opts) -> coFailOnSeverity opts `shouldBe` Just "warning"
        _ -> expectationFailure "Expected CmdCheck"

    it "parses fail-on-count" $ do
      case getCommand ["check", "--fail-on-count", "10", "."] of
        Just (CmdCheck _ opts) -> coFailOnCount opts `shouldBe` Just 10
        _ -> expectationFailure "Expected CmdCheck"

    it "parses fail-on-delta" $ do
      case getCommand ["check", "--fail-on-delta", "5", "."] of
        Just (CmdCheck _ opts) -> coFailOnDelta opts `shouldBe` Just 5
        _ -> expectationFailure "Expected CmdCheck"

  describe "update options" $ do
    it "defaults to not updating baseline" $ do
      case getCommand ["check", "."] of
        Just (CmdCheck _ opts) -> coUpdateBaseline opts `shouldBe` False
        _ -> expectationFailure "Expected CmdCheck"

    it "enables update-baseline" $ do
      case getCommand ["check", "--update-baseline", "."] of
        Just (CmdCheck _ opts) -> coUpdateBaseline opts `shouldBe` True
        _ -> expectationFailure "Expected CmdCheck"

  describe "quiet mode" $ do
    it "defaults to not quiet" $ do
      case getCommand ["check", "."] of
        Just (CmdCheck _ opts) -> coCIQuiet opts `shouldBe` False
        _ -> expectationFailure "Expected CmdCheck"

    it "enables ci-quiet" $ do
      case getCommand ["check", "--ci-quiet", "."] of
        Just (CmdCheck _ opts) -> coCIQuiet opts `shouldBe` True
        _ -> expectationFailure "Expected CmdCheck"

  describe "combined CI options" $ do
    it "parses full CI mode configuration" $ do
      case getCommand ["check", "-b", "baseline.json", "--fail-on-new",
                       "--fail-on-severity", "error", "--fail-on-count", "100",
                       "--update-baseline", "--ci-quiet", "."] of
        Just (CmdCheck _ opts) -> do
          coBaselinePath opts `shouldBe` Just "baseline.json"
          coFailOnNew opts `shouldBe` True
          coFailOnSeverity opts `shouldBe` Just "error"
          coFailOnCount opts `shouldBe` Just 100
          coUpdateBaseline opts `shouldBe` True
          coCIQuiet opts `shouldBe` True
        _ -> expectationFailure "Expected CmdCheck"
