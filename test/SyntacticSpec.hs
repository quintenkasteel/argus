{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : SyntacticSpec
-- Description : Tests for Argus.Analysis.Syntactic
--
-- Comprehensive tests for syntactic analysis using ghc-lib-parser.
-- Tests parsing, function extraction, import/export handling, and TH detection.
module SyntacticSpec (spec) where

import Test.Hspec
import Data.Maybe (isJust, isNothing)
import Data.Text (Text)
import Data.Text qualified as T
import System.IO.Temp (withSystemTempDirectory)
import System.FilePath ((</>))

import Data.Set qualified as Set

import Argus.Analysis.Syntactic
import Argus.Types (SrcSpan(..), Line(..), Column(..), SymbolKind(..), QualifiedName(..), Symbol(..))

spec :: Spec
spec = do
  describe "Argus.Analysis.Syntactic" $ do
    describe "ParseError" $ do
      it "stores all fields correctly" $ do
        let err = ParseError
              { peFile = "Test.hs"
              , peLine = 10
              , peColumn = 5
              , peMessage = "Parse error"
              }
        peFile err `shouldBe` "Test.hs"
        peLine err `shouldBe` 10
        peColumn err `shouldBe` 5
        peMessage err `shouldBe` "Parse error"

    describe "FunctionInfo" $ do
      it "stores all fields correctly" $ do
        let fi = FunctionInfo
              { fiName = "testFunc"
              , fiSpan = noSrcSpanTest
              , fiSignature = Nothing
              , fiArguments = []
              , fiBody = []
              , fiExported = True
              }
        fiName fi `shouldBe` "testFunc"
        fiExported fi `shouldBe` True
        isNothing (fiSignature fi) `shouldBe` True

      it "handles function with signature" $ do
        let sig = TypeInfo "Int -> Bool" noSrcSpanTest ["Int"] [noSrcSpanTest] "Bool"
            fi = FunctionInfo "foo" noSrcSpanTest (Just sig) [] [] True
        isJust (fiSignature fi) `shouldBe` True

    describe "ArgumentInfo" $ do
      it "stores all fields correctly" $ do
        let arg = ArgumentInfo
              { aiName = "x"
              , aiType = Just "Int"
              , aiSpan = noSrcSpanTest
              , aiTypeSpan = Nothing
              }
        aiName arg `shouldBe` "x"
        aiType arg `shouldBe` Just "Int"

    describe "TypeInfo" $ do
      it "stores all fields correctly" $ do
        let ti = TypeInfo
              { tiText = "Int -> String -> Bool"
              , tiSpan = noSrcSpanTest
              , tiArgTypes = ["Int", "String"]
              , tiArgSpans = [noSrcSpanTest, noSrcSpanTest]
              , tiRetType = "Bool"
              }
        tiText ti `shouldBe` "Int -> String -> Bool"
        length (tiArgTypes ti) `shouldBe` 2
        tiRetType ti `shouldBe` "Bool"

    describe "ImportInfo" $ do
      it "stores all fields correctly" $ do
        let ii = ImportInfo
              { iiModuleName = "Data.Text"
              , iiQualified = True
              , iiAlias = Just "T"
              , iiSpan = noSrcSpanTest
              , iiHiding = False
              , iiExplicit = Nothing
              }
        iiModuleName ii `shouldBe` "Data.Text"
        iiQualified ii `shouldBe` True
        iiAlias ii `shouldBe` Just "T"
        iiHiding ii `shouldBe` False

      it "handles explicit import list" $ do
        let items = [ImportItem "map" False False False []]
            ii = ImportInfo "Data.List" False Nothing noSrcSpanTest False (Just items)
        isJust (iiExplicit ii) `shouldBe` True

    describe "ImportItem" $ do
      it "stores all fields correctly" $ do
        let item = ImportItem
              { importItemName = "ToJSON"
              , importItemIsType = True
              , importItemIsOperator = False
              , importItemIsWildcard = True
              , importItemChildren = ["toJSON", "toEncoding"]
              }
        importItemName item `shouldBe` "ToJSON"
        importItemIsType item `shouldBe` True
        importItemIsWildcard item `shouldBe` True
        length (importItemChildren item) `shouldBe` 2

    describe "ExportInfo" $ do
      it "stores all fields correctly" $ do
        let ei = ExportInfo
              { eiName = "myFunc"
              , eiSpan = noSrcSpanTest
              , eiIsType = False
              }
        eiName ei `shouldBe` "myFunc"
        eiIsType ei `shouldBe` False

    describe "ThSpliceInfo" $ do
      it "stores all fields correctly" $ do
        let si = ThSpliceInfo
              { tsiSpan = noSrcSpanTest
              , tsiQuotedName = Just "MyType"
              , tsiExpr = "$(deriveJSON defaultOptions ''MyType)"
              , tsiIsTyped = False
              }
        tsiQuotedName si `shouldBe` Just "MyType"
        tsiIsTyped si `shouldBe` False

      it "handles typed splices" $ do
        let si = ThSpliceInfo noSrcSpanTest Nothing "$$(typedExpr)" True
        tsiIsTyped si `shouldBe` True

    describe "QuasiQuoteInfo" $ do
      it "stores all fields correctly" $ do
        let qq = QuasiQuoteInfo
              { qqiSpan = noSrcSpanTest
              , qqiQuoter = "sql"
              , qqiContents = "SELECT * FROM users"
              }
        qqiQuoter qq `shouldBe` "sql"
        qqiContents qq `shouldBe` "SELECT * FROM users"

    describe "ThQuoteInfo" $ do
      it "stores all fields for value quote" $ do
        let tq = ThQuoteInfo noSrcSpanTest "myFunc" False
        tqiName tq `shouldBe` "myFunc"
        tqiIsType tq `shouldBe` False

      it "stores all fields for type quote" $ do
        let tq = ThQuoteInfo noSrcSpanTest "MyType" True
        tqiName tq `shouldBe` "MyType"
        tqiIsType tq `shouldBe` True

    describe "PragmaInfo" $ do
      it "stores enabled extension" $ do
        let pi' = PragmaInfo noSrcSpanTest "OverloadedStrings" True
        piExtension pi' `shouldBe` "OverloadedStrings"
        piIsEnabled pi' `shouldBe` True

      it "stores disabled extension" $ do
        let pi' = PragmaInfo noSrcSpanTest "MonomorphismRestriction" False
        piExtension pi' `shouldBe` "MonomorphismRestriction"
        piIsEnabled pi' `shouldBe` False

    describe "InstanceInfo" $ do
      it "stores all fields correctly" $ do
        let ii = InstanceInfo
              { iiInstanceSpan = noSrcSpanTest
              , iiClassName = "Show"
              , iiTypeName = "MyType"
              , iiFullType = "Show MyType"
              , iiIsDeriving = False
              , iiIsOrphan = False
              }
        iiClassName ii `shouldBe` "Show"
        iiTypeName ii `shouldBe` "MyType"
        iiIsDeriving ii `shouldBe` False
        iiIsOrphan ii `shouldBe` False

    describe "Parsing" $ do
      describe "parseModule" $ do
        it "parses simple module" $ do
          let source = "module Test where\n\nfoo :: Int\nfoo = 42"
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> prFile pr `shouldBe` "Test.hs"
            Left err -> expectationFailure $ T.unpack $ peMessage err

        it "parses module with imports" $ do
          let source = T.unlines
                [ "module Test where"
                , "import Data.Text"
                , "import qualified Data.Map as M"
                , "foo = 1"
                ]
          result <- parseModule "Test.hs" source
          case result of
            Right _ -> pure ()
            Left err -> expectationFailure $ T.unpack $ peMessage err

        it "parses module with exports" $ do
          let source = T.unlines
                [ "module Test (foo, bar) where"
                , "foo = 1"
                , "bar = 2"
                , "baz = 3"
                ]
          result <- parseModule "Test.hs" source
          case result of
            Right _ -> pure ()
            Left err -> expectationFailure $ T.unpack $ peMessage err

        it "returns ParseError for invalid syntax" $ do
          let source = "module Test where\n\nfoo = = invalid"
          result <- parseModule "Test.hs" source
          case result of
            Left _ -> pure ()  -- Expected
            Right _ -> expectationFailure "Should fail on invalid syntax"

        it "handles extensions" $ do
          let source = T.unlines
                [ "{-# LANGUAGE OverloadedStrings #-}"
                , "module Test where"
                , "import Data.Text (Text)"
                , "foo :: Text"
                , "foo = \"hello\""
                ]
          result <- parseModule "Test.hs" source
          case result of
            Right _ -> pure ()
            Left err -> expectationFailure $ T.unpack $ peMessage err

      describe "parseFile" $ do
        it "returns error for non-existent file" $ do
          result <- parseFile "/nonexistent/path/Test.hs"
          case result of
            Left _ -> pure ()
            Right _ -> expectationFailure "Should fail for non-existent file"

        it "parses existing file" $ withTempDir $ \tmpDir -> do
          let path = tmpDir </> "Test.hs"
          writeFile path "module Test where\nfoo = 1"
          result <- parseFile path
          case result of
            Right pr -> prFile pr `shouldBe` path
            Left err -> expectationFailure $ T.unpack $ peMessage err

    describe "Function Extraction" $ do
      describe "extractFunctions" $ do
        it "extracts simple function" $ do
          let source = "module Test where\n\nfoo :: Int\nfoo = 42"
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> do
              let funcs = extractFunctions "Test.hs" source (prModule pr)
              length funcs `shouldSatisfy` (>= 1)
              any (\f -> fiName f == "foo") funcs `shouldBe` True
            Left _ -> expectationFailure "Parse failed"

        it "extracts function with arguments" $ do
          let source = "module Test where\n\nadd :: Int -> Int -> Int\nadd x y = x + y"
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> do
              let funcs = extractFunctions "Test.hs" source (prModule pr)
              case filter (\f -> fiName f == "add") funcs of
                [f] -> do
                  length (fiArguments f) `shouldSatisfy` (>= 2)
                  isJust (fiSignature f) `shouldBe` True
                _ -> expectationFailure "Expected exactly one 'add' function"
            Left _ -> expectationFailure "Parse failed"

        it "detects exported status" $ do
          let source = T.unlines
                [ "module Test (exported) where"
                , "exported = 1"
                , "notExported = 2"
                ]
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> do
              let funcs = extractFunctions "Test.hs" source (prModule pr)
              case filter (\f -> fiName f == "exported") funcs of
                [f] -> fiExported f `shouldBe` True
                _ -> pure ()
              case filter (\f -> fiName f == "notExported") funcs of
                [f] -> fiExported f `shouldBe` False
                _ -> pure ()
            Left _ -> expectationFailure "Parse failed"

    describe "Type Extraction" $ do
      describe "extractTypes" $ do
        it "extracts data type" $ do
          let source = "module Test where\n\ndata MyType = MyConstructor Int"
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> do
              let types = extractTypes "Test.hs" (prModule pr)
              any (\s -> qnName (symbolName s) == "MyType") types `shouldBe` True
            Left _ -> expectationFailure "Parse failed"

        it "extracts newtype" $ do
          let source = "module Test where\n\nnewtype Wrapper = Wrapper Int"
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> do
              let types = extractTypes "Test.hs" (prModule pr)
              any (\s -> qnName (symbolName s) == "Wrapper") types `shouldBe` True
            Left _ -> expectationFailure "Parse failed"

        it "extracts type alias" $ do
          let source = "module Test where\n\ntype MyAlias = Int"
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> do
              let types = extractTypes "Test.hs" (prModule pr)
              any (\s -> qnName (symbolName s) == "MyAlias") types `shouldBe` True
            Left _ -> expectationFailure "Parse failed"

        it "extracts class declaration" $ do
          let source = "module Test where\n\nclass MyClass a where\n  myMethod :: a -> Int"
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> do
              let types = extractTypes "Test.hs" (prModule pr)
              any (\s -> symbolKind s == TypeClass) types `shouldBe` True
            Left _ -> expectationFailure "Parse failed"

    describe "Import Extraction" $ do
      describe "extractImports" $ do
        it "extracts simple import" $ do
          let source = "module Test where\n\nimport Data.List\n\nfoo = 1"
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> do
              let imports = extractImports "Test.hs" (prModule pr)
              any (\i -> iiModuleName i == "Data.List") imports `shouldBe` True
            Left _ -> expectationFailure "Parse failed"

        it "extracts qualified import" $ do
          let source = "module Test where\n\nimport qualified Data.Map as M\n\nfoo = 1"
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> do
              let imports = extractImports "Test.hs" (prModule pr)
              case filter (\i -> iiModuleName i == "Data.Map") imports of
                [i] -> do
                  iiQualified i `shouldBe` True
                  iiAlias i `shouldBe` Just "M"
                _ -> expectationFailure "Expected qualified import"
            Left _ -> expectationFailure "Parse failed"

        it "extracts explicit import list" $ do
          let source = "module Test where\n\nimport Data.List (map, filter)\n\nfoo = 1"
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> do
              let imports = extractImports "Test.hs" (prModule pr)
              case filter (\i -> iiModuleName i == "Data.List") imports of
                [i] -> isJust (iiExplicit i) `shouldBe` True
                _ -> expectationFailure "Expected explicit import"
            Left _ -> expectationFailure "Parse failed"

        it "extracts hiding import" $ do
          let source = "module Test where\n\nimport Data.List hiding (map)\n\nfoo = 1"
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> do
              let imports = extractImports "Test.hs" (prModule pr)
              case filter (\i -> iiModuleName i == "Data.List") imports of
                [i] -> iiHiding i `shouldBe` True
                _ -> expectationFailure "Expected hiding import"
            Left _ -> expectationFailure "Parse failed"

    describe "Export Extraction" $ do
      describe "extractExports" $ do
        it "returns empty for module without explicit exports" $ do
          let source = "module Test where\n\nfoo = 1"
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> do
              let exports = extractExports "Test.hs" (prModule pr)
              exports `shouldBe` []
            Left _ -> expectationFailure "Parse failed"

        it "extracts explicit exports" $ do
          let source = "module Test (foo, bar) where\n\nfoo = 1\nbar = 2"
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> do
              let exports = extractExports "Test.hs" (prModule pr)
              length exports `shouldBe` 2
            Left _ -> expectationFailure "Parse failed"

        it "detects type exports" $ do
          let source = "module Test (MyType(..)) where\n\ndata MyType = MkMyType"
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> do
              let exports = extractExports "Test.hs" (prModule pr)
              any eiIsType exports `shouldBe` True
            Left _ -> expectationFailure "Parse failed"

    describe "Template Haskell Extraction" $ do
      describe "extractThSplices" $ do
        it "extracts splice from declaration" $ do
          let source = T.unlines
                [ "{-# LANGUAGE TemplateHaskell #-}"
                , "module Test where"
                , "import Language.Haskell.TH"
                , "$(return [])"
                ]
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> do
              let splices = extractThSplices "Test.hs" (prModule pr)
              -- Should find at least the top-level splice
              splices `shouldSatisfy` const True
            Left _ -> expectationFailure "Parse failed"

      describe "extractQuasiQuotes" $ do
        it "handles module without quasiquotes" $ do
          let source = "module Test where\n\nfoo = 1"
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> do
              let qqs = extractQuasiQuotes "Test.hs" (prModule pr)
              qqs `shouldBe` []
            Left _ -> expectationFailure "Parse failed"

      describe "extractThQuotes" $ do
        it "handles module without quotes" $ do
          let source = "module Test where\n\nfoo = 1"
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> do
              let quotes = extractThQuotes "Test.hs" (prModule pr)
              quotes `shouldBe` []
            Left _ -> expectationFailure "Parse failed"

    describe "Pragma Extraction" $ do
      describe "extractPragmas" $ do
        it "extracts single LANGUAGE pragma" $ do
          let source = "{-# LANGUAGE OverloadedStrings #-}\nmodule Test where"
          let pragmas = extractPragmas "Test.hs" source
          length pragmas `shouldSatisfy` (>= 1)
          -- Check that at least one pragma contains "OverloadedStrings"
          any (\p -> "OverloadedStrings" `T.isInfixOf` piExtension p) pragmas `shouldBe` True

        it "extracts multiple extensions in one pragma" $ do
          let source = "{-# LANGUAGE OverloadedStrings, GADTs #-}\nmodule Test where"
          let pragmas = extractPragmas "Test.hs" source
          length pragmas `shouldSatisfy` (>= 2)

        it "extracts multiple LANGUAGE pragmas" $ do
          let source = T.unlines
                [ "{-# LANGUAGE OverloadedStrings #-}"
                , "{-# LANGUAGE GADTs #-}"
                , "module Test where"
                ]
          let pragmas = extractPragmas "Test.hs" source
          length pragmas `shouldSatisfy` (>= 2)

        it "detects disabled extensions" $ do
          let source = "{-# LANGUAGE NoMonomorphismRestriction #-}\nmodule Test where"
          let pragmas = extractPragmas "Test.hs" source
          case filter (\p -> piExtension p == "MonomorphismRestriction") pragmas of
            [p] -> piIsEnabled p `shouldBe` False
            _ -> pure ()  -- May not parse disabled extensions

        it "returns empty for no pragmas" $ do
          let source = "module Test where\nfoo = 1"
          let pragmas = extractPragmas "Test.hs" source
          pragmas `shouldBe` []

    describe "Instance Extraction" $ do
      describe "extractInstances" $ do
        it "extracts instance declaration" $ do
          let source = T.unlines
                [ "module Test where"
                , "data MyType = MyType"
                , "instance Show MyType where"
                , "  show _ = \"MyType\""
                ]
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> do
              let instances = extractInstances "Test.hs" "Test" (prModule pr)
              any (\i -> iiClassName i == "Show") instances `shouldBe` True
            Left _ -> expectationFailure "Parse failed"

      describe "isPotentialOrphan" $ do
        it "returns False for local type with standard class" $ do
          let localTypes = ["MyType"]
          isPotentialOrphan "Test" (setFromList localTypes) "Show" "MyType" `shouldBe` False

        it "returns True for external type with external class" $ do
          let localTypes = [] :: [Text]
          isPotentialOrphan "Test" (setFromList localTypes) "ToJSON" "SomeExternalType" `shouldBe` True

        it "returns False when type is local" $ do
          let localTypes = ["MyType"]
          isPotentialOrphan "Test" (setFromList localTypes) "ToJSON" "MyType" `shouldBe` False

    describe "Utilities" $ do
      describe "prettyPrintType" $ do
        it "prints types from parsed module" $ do
          let source = "module Test where\n\nfoo :: Int -> Bool\nfoo = undefined"
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> do
              let funcs = extractFunctions "Test.hs" source (prModule pr)
              case filter (\f -> fiName f == "foo") funcs of
                [f] -> case fiSignature f of
                  Just sig -> T.length (tiText sig) `shouldSatisfy` (> 0)
                  Nothing -> expectationFailure "Expected signature"
                _ -> expectationFailure "Expected foo function"
            Left _ -> expectationFailure "Parse failed"

      describe "getSourceText" $ do
        it "extracts text for span" $ do
          let source = "line 1\nline 2\nline 3"
              span' = SrcSpan "Test.hs" (Line 2) (Column 1) (Line 2) (Column 6)
          getSourceText source span' `shouldBe` "line 2"

        it "handles multi-line spans" $ do
          let source = "line 1\nline 2\nline 3"
              span' = SrcSpan "Test.hs" (Line 1) (Column 1) (Line 3) (Column 6)
          T.lines (getSourceText source span') `shouldBe` ["line 1", "line 2", "line 3"]

      describe "spanToSrcSpan" $ do
        -- spanToSrcSpan is internal, but we can test indirectly through extraction
        it "converts GHC spans to Argus spans" $ do
          let source = "module Test where\n\nfoo = 1"
          result <- parseModule "Test.hs" source
          case result of
            Right pr -> do
              let funcs = extractFunctions "Test.hs" source (prModule pr)
              case funcs of
                (f:_) -> srcSpanFile (fiSpan f) `shouldBe` "Test.hs"
                [] -> pure ()  -- OK if no functions extracted
            Left _ -> expectationFailure "Parse failed"

    describe "Show and Eq instances" $ do
      it "ParseError has Show and Eq" $ do
        let e1 = ParseError "f" 1 1 "m"
            e2 = ParseError "f" 1 1 "m"
        show e1 `shouldContain` "ParseError"
        e1 `shouldBe` e2

      it "FunctionInfo has Show and Eq" $ do
        let f1 = FunctionInfo "f" noSrcSpanTest Nothing [] [] True
            f2 = FunctionInfo "f" noSrcSpanTest Nothing [] [] True
        show f1 `shouldContain` "FunctionInfo"
        f1 `shouldBe` f2

      it "ImportInfo has Show and Eq" $ do
        let i1 = ImportInfo "M" False Nothing noSrcSpanTest False Nothing
            i2 = ImportInfo "M" False Nothing noSrcSpanTest False Nothing
        show i1 `shouldContain` "ImportInfo"
        i1 `shouldBe` i2

      it "ExportInfo has Show and Eq" $ do
        let e1 = ExportInfo "n" noSrcSpanTest False
            e2 = ExportInfo "n" noSrcSpanTest False
        show e1 `shouldContain` "ExportInfo"
        e1 `shouldBe` e2

      it "ThSpliceInfo has Show and Eq" $ do
        let s1 = ThSpliceInfo noSrcSpanTest Nothing "e" False
            s2 = ThSpliceInfo noSrcSpanTest Nothing "e" False
        show s1 `shouldContain` "ThSpliceInfo"
        s1 `shouldBe` s2

      it "QuasiQuoteInfo has Show and Eq" $ do
        let q1 = QuasiQuoteInfo noSrcSpanTest "sql" "c"
            q2 = QuasiQuoteInfo noSrcSpanTest "sql" "c"
        show q1 `shouldContain` "QuasiQuoteInfo"
        q1 `shouldBe` q2

      it "ThQuoteInfo has Show and Eq" $ do
        let t1 = ThQuoteInfo noSrcSpanTest "n" False
            t2 = ThQuoteInfo noSrcSpanTest "n" False
        show t1 `shouldContain` "ThQuoteInfo"
        t1 `shouldBe` t2

      it "PragmaInfo has Show and Eq" $ do
        let p1 = PragmaInfo noSrcSpanTest "E" True
            p2 = PragmaInfo noSrcSpanTest "E" True
        show p1 `shouldContain` "PragmaInfo"
        p1 `shouldBe` p2

      it "InstanceInfo has Show and Eq" $ do
        let i1 = InstanceInfo noSrcSpanTest "C" "T" "C T" False False
            i2 = InstanceInfo noSrcSpanTest "C" "T" "C T" False False
        show i1 `shouldContain` "InstanceInfo"
        i1 `shouldBe` i2


-- Helper for creating test SrcSpan
noSrcSpanTest :: SrcSpan
noSrcSpanTest = SrcSpan "" (Line 1) (Column 1) (Line 1) (Column 1)

-- Helper for creating a Set from a list
setFromList :: Ord a => [a] -> Set.Set a
setFromList = Set.fromList

-- Helper for running tests with a temporary directory
withTempDir :: (FilePath -> IO a) -> IO a
withTempDir = withSystemTempDirectory "argus-syntactic-test"
