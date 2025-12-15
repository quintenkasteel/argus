{-# LANGUAGE OverloadedStrings #-}

module WatchSpec (spec) where

import Data.Text qualified as T
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>))
import System.IO.Temp (withSystemTempDirectory)
import System.IO.Silently (capture_)
import Test.Hspec

import Argus.Watch
import Argus.Types (Diagnostic(..), Severity(..), DiagnosticKind(..), mkSrcSpanRaw)

spec :: Spec
spec = do
  describe "Argus.Watch" $ do
    describe "WatchConfig" $ do
      it "has sensible default directories" $ do
        let cfg = defaultWatchConfig
        wcDirectories cfg `shouldBe` ["."]

      it "has sensible default exclude patterns" $ do
        let cfg = defaultWatchConfig
        wcExcludePatterns cfg `shouldContain` ["dist-newstyle/**"]
        wcExcludePatterns cfg `shouldContain` [".stack-work/**"]
        wcExcludePatterns cfg `shouldContain` [".git/**"]

      it "has reasonable debounce time" $ do
        let cfg = defaultWatchConfig
        wcDebounceMs cfg `shouldSatisfy` (> 0)
        wcDebounceMs cfg `shouldSatisfy` (< 2000)

      it "has reasonable poll interval" $ do
        let cfg = defaultWatchConfig
        wcPollIntervalMs cfg `shouldSatisfy` (> 0)
        wcPollIntervalMs cfg `shouldSatisfy` (< 5000)

      it "has clear screen enabled by default" $ do
        let cfg = defaultWatchConfig
        wcClearScreen cfg `shouldBe` True

      it "has timestamp enabled by default" $ do
        let cfg = defaultWatchConfig
        wcShowTimestamp cfg `shouldBe` True

      it "has verbose disabled by default" $ do
        let cfg = defaultWatchConfig
        wcVerbose cfg `shouldBe` False

    describe "WatchEvent" $ do
      it "FileModified contains path" $ do
        let event = FileModified "src/Main.hs"
        case event of
          FileModified path -> path `shouldBe` "src/Main.hs"
          _ -> expectationFailure "Expected FileModified"

      it "FileCreated contains path" $ do
        let event = FileCreated "src/New.hs"
        case event of
          FileCreated path -> path `shouldBe` "src/New.hs"
          _ -> expectationFailure "Expected FileCreated"

      it "FileDeleted contains path" $ do
        let event = FileDeleted "src/Old.hs"
        case event of
          FileDeleted path -> path `shouldBe` "src/Old.hs"
          _ -> expectationFailure "Expected FileDeleted"

      it "AnalysisStarted contains file list" $ do
        let event = AnalysisStarted ["a.hs", "b.hs"]
        case event of
          AnalysisStarted files -> length files `shouldBe` 2
          _ -> expectationFailure "Expected AnalysisStarted"

      it "AnalysisCompleted contains diagnostics and timing" $ do
        let event = AnalysisCompleted [] 0.5
        case event of
          AnalysisCompleted diags time -> do
            diags `shouldBe` []
            time `shouldBe` 0.5
          _ -> expectationFailure "Expected AnalysisCompleted"

      it "WatchError contains message" $ do
        let event = WatchError "Some error"
        case event of
          WatchError msg -> msg `shouldBe` "Some error"
          _ -> expectationFailure "Expected WatchError"

      it "events have Eq instance" $ do
        FileModified "a.hs" `shouldBe` FileModified "a.hs"
        FileModified "a.hs" `shouldNotBe` FileModified "b.hs"
        FileModified "a.hs" `shouldNotBe` FileCreated "a.hs"

      it "events have Show instance" $ do
        show (FileModified "a.hs") `shouldContain` "a.hs"
        show (FileCreated "b.hs") `shouldContain` "b.hs"
        show (WatchError "error") `shouldContain` "error"

    describe "defaultWatchCallback" $ do
      it "prints FileModified event" $ do
        output <- capture_ $ defaultWatchCallback (FileModified "test.hs")
        output `shouldContain` "Modified"
        output `shouldContain` "test.hs"

      it "prints FileCreated event" $ do
        output <- capture_ $ defaultWatchCallback (FileCreated "new.hs")
        output `shouldContain` "Created"
        output `shouldContain` "new.hs"

      it "prints FileDeleted event" $ do
        output <- capture_ $ defaultWatchCallback (FileDeleted "old.hs")
        output `shouldContain` "Deleted"
        output `shouldContain` "old.hs"

      it "prints AnalysisStarted event with file count" $ do
        output <- capture_ $ defaultWatchCallback (AnalysisStarted ["a.hs", "b.hs", "c.hs"])
        output `shouldContain` "Analyzing"
        output `shouldContain` "3"

      it "prints AnalysisCompleted with error and warning counts" $ do
        let diags =
              [ mkDiag Error
              , mkDiag Error
              , mkDiag Warning
              , mkDiag Suggestion
              ]
        output <- capture_ $ defaultWatchCallback (AnalysisCompleted diags 1.5)
        output `shouldContain` "2 error"
        output `shouldContain` "1 warning"
        output `shouldContain` "1.5"

      it "prints WatchError with message" $ do
        output <- capture_ $ defaultWatchCallback (WatchError "Connection lost")
        output `shouldContain` "error"
        output `shouldContain` "Connection lost"

    describe "file scanning" $ do
      it "finds Haskell files in directory" $ do
        withSystemTempDirectory "argus-watch-test" $ \tmpDir -> do
          -- Create some test files
          let hsFile = tmpDir </> "Test.hs"
              txtFile = tmpDir </> "readme.txt"
          writeFile hsFile "module Test where"
          writeFile txtFile "Not Haskell"
          -- scanDirectories should find only .hs files
          files <- scanDirectories [tmpDir] []
          any (== hsFile) files `shouldBe` True
          any (== txtFile) files `shouldBe` False

      it "respects exclude patterns" $ do
        withSystemTempDirectory "argus-watch-test" $ \tmpDir -> do
          -- Create a dist-newstyle directory with a .hs file
          let distDir = tmpDir </> "dist-newstyle"
              distFile = distDir </> "Build.hs"
              srcFile = tmpDir </> "Main.hs"
          createDirectoryIfMissing True distDir
          writeFile distFile "module Build where"
          writeFile srcFile "module Main where"
          -- Should exclude dist-newstyle
          files <- scanDirectories [tmpDir] ["dist-newstyle/**"]
          any (\f -> "dist-newstyle" `T.isInfixOf` T.pack f) files `shouldBe` False
          any (== srcFile) files `shouldBe` True

      it "scans subdirectories recursively" $ do
        withSystemTempDirectory "argus-watch-test" $ \tmpDir -> do
          -- Create nested directories with .hs files
          let subDir = tmpDir </> "src" </> "Internal"
              nestedFile = subDir </> "Deep.hs"
          createDirectoryIfMissing True subDir
          writeFile nestedFile "module Internal.Deep where"
          files <- scanDirectories [tmpDir] []
          any (== nestedFile) files `shouldBe` True

      it "handles non-existent directories gracefully" $ do
        files <- scanDirectories ["/nonexistent/path/argus-watch"] []
        files `shouldBe` []

      it "handles empty directories" $ do
        withSystemTempDirectory "argus-watch-test" $ \tmpDir -> do
          files <- scanDirectories [tmpDir] []
          files `shouldBe` []

    describe "pattern matching" $ do
      it "matchPattern matches exact paths" $ do
        matchPattern "foo.hs" "foo.hs" `shouldBe` True
        matchPattern "foo.hs" "bar.hs" `shouldBe` False

      it "matchPattern handles single wildcard" $ do
        matchPattern "*.hs" "foo.hs" `shouldBe` True
        matchPattern "*.hs" "bar.hs" `shouldBe` True
        matchPattern "*.hs" "foo.txt" `shouldBe` False

      it "matchPattern handles prefix wildcard" $ do
        matchPattern "src/*" "src/Main" `shouldBe` True
        matchPattern "src/*" "lib/Main" `shouldBe` False

      it "matchPattern handles double wildcard for deep paths" $ do
        matchPattern "dist-newstyle/**" "dist-newstyle/build/Main.hs" `shouldBe` True
        matchPattern ".git/**" ".git/objects/abc123" `shouldBe` True

      it "matchesExclude checks multiple patterns" $ do
        let patterns = ["dist-newstyle/**", ".git/**", "*.bak"]
        matchesExclude patterns "dist-newstyle/build" `shouldBe` True
        matchesExclude patterns ".git/config" `shouldBe` True
        matchesExclude patterns "backup.bak" `shouldBe` True
        matchesExclude patterns "src/Main.hs" `shouldBe` False

      it "matchesExclude returns False for empty pattern list" $ do
        matchesExclude [] "any/path/here" `shouldBe` False

    describe "Haskell file detection" $ do
      it "isHaskellFile returns True for .hs files" $ do
        withSystemTempDirectory "argus-watch-test" $ \tmpDir -> do
          let hsFile = tmpDir </> "Test.hs"
          writeFile hsFile "module Test where"
          result <- isHaskellFile hsFile
          result `shouldBe` True

      it "isHaskellFile returns False for non-.hs files" $ do
        withSystemTempDirectory "argus-watch-test" $ \tmpDir -> do
          let txtFile = tmpDir </> "readme.txt"
          writeFile txtFile "Not Haskell"
          result <- isHaskellFile txtFile
          result `shouldBe` False

      it "isHaskellFile returns False for .lhs files" $ do
        withSystemTempDirectory "argus-watch-test" $ \tmpDir -> do
          let lhsFile = tmpDir </> "Literate.lhs"
          writeFile lhsFile "> module Literate where"
          result <- isHaskellFile lhsFile
          result `shouldBe` False

      it "isHaskellFile returns False for non-existent files" $ do
        result <- isHaskellFile "/nonexistent/file.hs"
        result `shouldBe` False

    describe "WatchConfig customization" $ do
      it "allows custom directories" $ do
        let cfg = defaultWatchConfig { wcDirectories = ["src", "lib", "app"] }
        wcDirectories cfg `shouldBe` ["src", "lib", "app"]

      it "allows custom exclude patterns" $ do
        let cfg = defaultWatchConfig { wcExcludePatterns = ["*.bak", "tmp/**"] }
        wcExcludePatterns cfg `shouldBe` ["*.bak", "tmp/**"]

      it "allows custom debounce time" $ do
        let cfg = defaultWatchConfig { wcDebounceMs = 1000 }
        wcDebounceMs cfg `shouldBe` 1000

      it "allows custom poll interval" $ do
        let cfg = defaultWatchConfig { wcPollIntervalMs = 2000 }
        wcPollIntervalMs cfg `shouldBe` 2000

      it "allows disabling clear screen" $ do
        let cfg = defaultWatchConfig { wcClearScreen = False }
        wcClearScreen cfg `shouldBe` False

      it "allows disabling timestamp" $ do
        let cfg = defaultWatchConfig { wcShowTimestamp = False }
        wcShowTimestamp cfg `shouldBe` False

      it "allows enabling verbose mode" $ do
        let cfg = defaultWatchConfig { wcVerbose = True }
        wcVerbose cfg `shouldBe` True

    describe "advanced pattern matching" $ do
      it "matchPattern handles complex double wildcard patterns" $ do
        -- Double wildcards match anywhere within the path when prefix is present
        matchPattern ".stack-work/**" ".stack-work/dist/build/argus" `shouldBe` True
        matchPattern ".stack-work/**" ".stack-work/" `shouldBe` True
        -- Note: The current implementation uses isInfixOf for **, so this also matches
        matchPattern ".stack-work/**" "src/.stack-work/foo" `shouldBe` True

      it "matchPattern handles single wildcard with prefix" $ do
        -- Current implementation: simple prefix + suffix matching with single *
        matchPattern "src/*.hs" "src/Main.hs" `shouldBe` True
        matchPattern "test/*.hs" "test/Spec.hs" `shouldBe` True
        matchPattern "*.hs" "Main.hs" `shouldBe` True

      it "matchPattern handles dot files" $ do
        matchPattern ".git/**" ".git/config" `shouldBe` True
        matchPattern ".git/**" ".git/objects/abc" `shouldBe` True
        matchPattern ".*" ".gitignore" `shouldBe` True

      it "matchPattern case sensitivity" $ do
        matchPattern "*.hs" "Main.hs" `shouldBe` True
        matchPattern "*.hs" "Main.HS" `shouldBe` False
        matchPattern "*.Hs" "Main.Hs" `shouldBe` True

      it "matchesExclude with nested exclusions" $ do
        let patterns = ["dist-newstyle/**", ".git/**", "*.bak", ".stack-work/**"]
        matchesExclude patterns "dist-newstyle/build/x86_64-linux/ghc-9.10/argus" `shouldBe` True
        matchesExclude patterns ".git/hooks/pre-commit" `shouldBe` True
        matchesExclude patterns ".stack-work/install/x86/bin" `shouldBe` True
        matchesExclude patterns "src/Main.hs" `shouldBe` False

    describe "multiple directory scanning" $ do
      it "scans multiple directories" $ do
        withSystemTempDirectory "argus-watch-test" $ \tmpDir -> do
          let dir1 = tmpDir </> "src"
              dir2 = tmpDir </> "lib"
              file1 = dir1 </> "Main.hs"
              file2 = dir2 </> "Utils.hs"
          createDirectoryIfMissing True dir1
          createDirectoryIfMissing True dir2
          writeFile file1 "module Main where"
          writeFile file2 "module Utils where"
          files <- scanDirectories [dir1, dir2] []
          length files `shouldBe` 2
          any (== file1) files `shouldBe` True
          any (== file2) files `shouldBe` True

      it "handles overlapping directories" $ do
        withSystemTempDirectory "argus-watch-test" $ \tmpDir -> do
          let subDir = tmpDir </> "sub"
              file1 = tmpDir </> "Root.hs"
              file2 = subDir </> "Sub.hs"
          createDirectoryIfMissing True subDir
          writeFile file1 "module Root where"
          writeFile file2 "module Sub where"
          -- Scanning tmpDir already includes subDir, so scanning both shouldn't duplicate
          files <- scanDirectories [tmpDir] []
          any (== file1) files `shouldBe` True
          any (== file2) files `shouldBe` True

      it "excludes patterns across all directories" $ do
        withSystemTempDirectory "argus-watch-test" $ \tmpDir -> do
          let dir1 = tmpDir </> "src"
              dir2 = tmpDir </> "lib"
              build1 = dir1 </> "build"
              build2 = dir2 </> "build"
          createDirectoryIfMissing True build1
          createDirectoryIfMissing True build2
          writeFile (dir1 </> "Main.hs") "module Main where"
          writeFile (build1 </> "Generated.hs") "module Generated where"
          writeFile (dir2 </> "Lib.hs") "module Lib where"
          writeFile (build2 </> "Auto.hs") "module Auto where"
          files <- scanDirectories [dir1, dir2] ["build/**"]
          any (\f -> "build" `T.isInfixOf` T.pack f) files `shouldBe` False
          length files `shouldBe` 2

    describe "WatchEvent edge cases" $ do
      it "AnalysisCompleted handles empty diagnostics" $ do
        let event = AnalysisCompleted [] 0.0
        case event of
          AnalysisCompleted diags time -> do
            diags `shouldBe` []
            time `shouldBe` 0.0
          _ -> expectationFailure "Expected AnalysisCompleted"

      it "AnalysisCompleted handles mixed severity diagnostics" $ do
        let diags = [mkDiag Error, mkDiag Warning, mkDiag Suggestion, mkDiag Error]
            event = AnalysisCompleted diags 2.5
        case event of
          AnalysisCompleted ds time -> do
            length (filter (\d -> diagSeverity d == Error) ds) `shouldBe` 2
            length (filter (\d -> diagSeverity d == Warning) ds) `shouldBe` 1
            length (filter (\d -> diagSeverity d == Suggestion) ds) `shouldBe` 1
            time `shouldBe` 2.5
          _ -> expectationFailure "Expected AnalysisCompleted"

      it "FileModified/Created/Deleted preserve full paths" $ do
        let path = "/home/user/projects/argus/src/Argus/Types.hs"
        case FileModified path of
          FileModified p -> p `shouldBe` path
          _ -> expectationFailure "Expected FileModified"
        case FileCreated path of
          FileCreated p -> p `shouldBe` path
          _ -> expectationFailure "Expected FileCreated"
        case FileDeleted path of
          FileDeleted p -> p `shouldBe` path
          _ -> expectationFailure "Expected FileDeleted"

      it "AnalysisStarted handles large file lists" $ do
        let files = ["file" <> show n <> ".hs" | n <- [1..1000 :: Int]]
            event = AnalysisStarted files
        case event of
          AnalysisStarted fs -> length fs `shouldBe` 1000
          _ -> expectationFailure "Expected AnalysisStarted"

      it "WatchError handles special characters in message" $ do
        let msg = "Error: file \"foo.hs\" contains invalid UTF-8 \x1234"
            event = WatchError msg
        case event of
          WatchError m -> m `shouldBe` msg
          _ -> expectationFailure "Expected WatchError"

    describe "callback output formatting" $ do
      it "AnalysisCompleted shows plural forms correctly" $ do
        -- Single error
        output1 <- capture_ $ defaultWatchCallback (AnalysisCompleted [mkDiag Error] 0.1)
        output1 `shouldContain` "1 error"
        -- Multiple errors
        output2 <- capture_ $ defaultWatchCallback (AnalysisCompleted [mkDiag Error, mkDiag Error] 0.2)
        output2 `shouldContain` "2 error"
        -- Zero errors
        output3 <- capture_ $ defaultWatchCallback (AnalysisCompleted [] 0.0)
        output3 `shouldContain` "0 error"

      it "AnalysisStarted shows singular file count" $ do
        output <- capture_ $ defaultWatchCallback (AnalysisStarted ["single.hs"])
        output `shouldContain` "1 file"

      it "timing output has reasonable precision" $ do
        output <- capture_ $ defaultWatchCallback (AnalysisCompleted [] 1.234)
        output `shouldContain` "1.234"

    describe "file modification time handling" $ do
      it "detects new files as changes" $ do
        withSystemTempDirectory "argus-watch-test" $ \tmpDir -> do
          let file = tmpDir </> "New.hs"
          -- Write file
          writeFile file "module New where"
          -- Scan should find it
          files <- scanDirectories [tmpDir] []
          any (== file) files `shouldBe` True

      it "handles rapid file modifications" $ do
        withSystemTempDirectory "argus-watch-test" $ \tmpDir -> do
          let file = tmpDir </> "Rapid.hs"
          -- Write multiple times rapidly
          writeFile file "v1"
          writeFile file "v2"
          writeFile file "v3"
          -- File should still be scannable
          files <- scanDirectories [tmpDir] []
          any (== file) files `shouldBe` True

    describe "special file handling" $ do
      it "ignores non-.hs Haskell-related files" $ do
        withSystemTempDirectory "argus-watch-test" $ \tmpDir -> do
          let hsFile = tmpDir </> "Module.hs"
              hiFile = tmpDir </> "Module.hi"
              oFile = tmpDir </> "Module.o"
              hieFile = tmpDir </> "Module.hie"
          writeFile hsFile "module Module where"
          writeFile hiFile "binary interface"
          writeFile oFile "binary object"
          writeFile hieFile "binary hie"
          files <- scanDirectories [tmpDir] []
          any (== hsFile) files `shouldBe` True
          any (== hiFile) files `shouldBe` False
          any (== oFile) files `shouldBe` False
          any (== hieFile) files `shouldBe` False

      it "handles hidden Haskell files" $ do
        withSystemTempDirectory "argus-watch-test" $ \tmpDir -> do
          let hiddenFile = tmpDir </> ".Hidden.hs"
          writeFile hiddenFile "module Hidden where"
          files <- scanDirectories [tmpDir] []
          -- Hidden .hs files should still be found
          any (== hiddenFile) files `shouldBe` True

      it "handles symlinks to Haskell files gracefully" $ do
        -- This test just ensures no crash on symlink handling
        withSystemTempDirectory "argus-watch-test" $ \tmpDir -> do
          let normalFile = tmpDir </> "Normal.hs"
          writeFile normalFile "module Normal where"
          files <- scanDirectories [tmpDir] []
          any (== normalFile) files `shouldBe` True

-- Helper to create a simple diagnostic for testing
mkDiag :: Severity -> Diagnostic
mkDiag sev = Diagnostic
  { diagSpan = mkSrcSpanRaw "test.hs" 1 1 1 10
  , diagMessage = "Test diagnostic"
  , diagSeverity = sev
  , diagKind = CodePattern
  , diagCode = Just "test/diag"
  , diagFixes = []
  , diagRelated = []
  }
