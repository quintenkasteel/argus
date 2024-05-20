{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Spec where

import ClassyPrelude
import Config
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.Text as Text
import Lint (Lint (..), LintMap)
import qualified Linter
import System.Directory (listDirectory)
import System.FilePath ((</>))
import Test.Hspec
import qualified Prelude

-- Helper function to read and convert file contents to Text
readFileText :: FilePath -> IO Text
readFileText = fmap pack . Prelude.readFile

-- Helper function to write Text to a file
writeFileText :: FilePath -> Text -> IO ()
writeFileText path = Prelude.writeFile path . unpack

spec :: Spec
spec = do
  describe "checkLints" $ do
    -- Fetch test files dynamically
    config <- runIO (Config.parse "test/config.yaml")
    testFiles <- runIO $ listDirectory "test/data"
    forM_ (sort testFiles) $ \testFile -> do
      let inputFile = "test/data" </> testFile
          resultFile = "test/data-result" </> testFile

      it ("correctly transforms " ++ testFile) $ do
        -- Read input and expected result
        expected <- readFileText resultFile

        -- Apply the checkLints function
        Just (newContent, _) <- Linter.checkFile config inputFile

        -- Compare the output with expected result
        newContent `shouldBe` (unpack expected)
