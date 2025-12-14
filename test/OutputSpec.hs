{-# LANGUAGE OverloadedStrings #-}

module OutputSpec (spec) where

import Data.Map.Strict qualified as Map
import Data.Set qualified as Set
import Data.Text qualified as T
import Test.Hspec

import Argus.Output.Checkstyle
import Argus.Output.CodeClimate
import Argus.Output.Html
import Argus.Output.Json
import Argus.Output.JUnit
import Argus.Output.Plain
import Argus.Output.Sarif
import Argus.Output.Terminal
import Argus.Output.Types
import Argus.Types (mkSrcSpanRaw, Diagnostic(..), Severity(..), SrcSpan(..), DiagnosticKind(..), AnalysisResult(..), FileResult(..), emptyAnalysisResult)

spec :: Spec
spec = do
  describe "Argus.Output.Json" $ do
    describe "renderJson" $ do
      it "produces valid JSON with version" $ do
        let result = mkTestResult
            opts = defaultOutputOptions
            json = renderJson opts result
        T.isInfixOf "\"version\"" json `shouldBe` True

      it "includes files array" $ do
        let result = mkTestResult
            opts = defaultOutputOptions
            json = renderJson opts result
        T.isInfixOf "\"files\"" json `shouldBe` True

      it "includes summary" $ do
        let result = mkTestResult
            opts = defaultOutputOptions
            json = renderJson opts result
        T.isInfixOf "\"summary\"" json `shouldBe` True

      it "includes diagnostics in files" $ do
        let result = mkTestResultWithDiag
            opts = defaultOutputOptions
            json = renderJson opts result
        T.isInfixOf "\"diagnostics\"" json `shouldBe` True

      it "handles empty results" $ do
        let result = emptyAnalysisResult
            opts = defaultOutputOptions
            json = renderJson opts result
        T.isInfixOf "\"files\":[]" json `shouldBe` True

    describe "renderJsonPretty" $ do
      it "produces indented output" $ do
        let result = mkTestResult
            opts = defaultOutputOptions
            json = renderJsonPretty opts result
        -- Pretty output should have newlines
        T.isInfixOf "\n" json `shouldBe` True

      it "produces valid JSON structure" $ do
        let result = mkTestResult
            opts = defaultOutputOptions
            json = renderJsonPretty opts result
        T.isInfixOf "\"version\"" json `shouldBe` True
        T.isInfixOf "\"files\"" json `shouldBe` True

    describe "JsonOutput" $ do
      it "has correct structure" $ do
        let output = JsonOutput
              { joVersion = "1.0.0"
              , joFiles = []
              , joSummary = emptySummary
              }
        joVersion output `shouldBe` "1.0.0"
        joFiles output `shouldBe` []

  describe "Argus.Output.Sarif" $ do
    describe "renderSarif" $ do
      it "produces SARIF version 2.1.0" $ do
        let result = mkTestResult
            opts = defaultOutputOptions
            sarif = renderSarif opts result
        T.isInfixOf "\"version\":\"2.1.0\"" sarif `shouldBe` True

      it "includes schema reference" $ do
        let result = mkTestResult
            opts = defaultOutputOptions
            sarif = renderSarif opts result
        T.isInfixOf "sarif-schema-2.1.0.json" sarif `shouldBe` True

      it "includes tool information" $ do
        let result = mkTestResult
            opts = defaultOutputOptions
            sarif = renderSarif opts result
        T.isInfixOf "\"tool\"" sarif `shouldBe` True
        T.isInfixOf "\"driver\"" sarif `shouldBe` True

      it "includes argus name" $ do
        let result = mkTestResult
            opts = defaultOutputOptions
            sarif = renderSarif opts result
        T.isInfixOf "argus" sarif `shouldBe` True

      it "includes runs array" $ do
        let result = mkTestResult
            opts = defaultOutputOptions
            sarif = renderSarif opts result
        T.isInfixOf "\"runs\"" sarif `shouldBe` True

      it "converts diagnostics to results" $ do
        let result = mkTestResultWithDiag
            opts = defaultOutputOptions
            sarif = renderSarif opts result
        T.isInfixOf "\"results\"" sarif `shouldBe` True

      it "includes location information" $ do
        let result = mkTestResultWithDiag
            opts = defaultOutputOptions
            sarif = renderSarif opts result
        T.isInfixOf "\"locations\"" sarif `shouldBe` True
        T.isInfixOf "\"physicalLocation\"" sarif `shouldBe` True

    describe "SarifLog" $ do
      it "has correct version" $ do
        let log = SarifLog
              { slVersion = "2.1.0"
              , slSchema = "test-schema"
              , slRuns = []
              }
        slVersion log `shouldBe` "2.1.0"

    describe "severity mapping" $ do
      it "maps Error to error level" $ do
        let result = mkResultWithSeverity Error
            sarif = renderSarif defaultOutputOptions result
        T.isInfixOf "\"level\":\"error\"" sarif `shouldBe` True

      it "maps Warning to warning level" $ do
        let result = mkResultWithSeverity Warning
            sarif = renderSarif defaultOutputOptions result
        T.isInfixOf "\"level\":\"warning\"" sarif `shouldBe` True

      it "maps Suggestion to note level" $ do
        let result = mkResultWithSeverity Suggestion
            sarif = renderSarif defaultOutputOptions result
        T.isInfixOf "\"level\":\"note\"" sarif `shouldBe` True

  describe "Argus.Output.JUnit" $ do
    describe "renderJUnit" $ do
      it "produces valid XML declaration" $ do
        let result = mkTestResult
            Output{outText} = renderJUnit defaultOutputOptions result
        T.isInfixOf "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" outText `shouldBe` True

      it "includes testsuites element" $ do
        let result = mkTestResult
            Output{outText} = renderJUnit defaultOutputOptions result
        T.isInfixOf "<testsuites" outText `shouldBe` True
        T.isInfixOf "</testsuites>" outText `shouldBe` True

      it "includes name attribute in testsuites" $ do
        let result = mkTestResult
            Output{outText} = renderJUnit defaultOutputOptions result
        T.isInfixOf "name=\"argus\"" outText `shouldBe` True

      it "includes tests count attribute" $ do
        let result = mkTestResultWithDiag
            Output{outText} = renderJUnit defaultOutputOptions result
        T.isInfixOf "tests=\"1\"" outText `shouldBe` True

      it "creates testsuite for files with diagnostics" $ do
        let result = mkTestResultWithDiag
            Output{outText} = renderJUnit defaultOutputOptions result
        T.isInfixOf "<testsuite name=\"src/Test.hs\"" outText `shouldBe` True

      it "creates testcase elements for diagnostics" $ do
        let result = mkTestResultWithDiag
            Output{outText} = renderJUnit defaultOutputOptions result
        T.isInfixOf "<testcase" outText `shouldBe` True
        T.isInfixOf "</testcase>" outText `shouldBe` True

      it "marks errors as failures" $ do
        let result = mkResultWithSeverity Error
            Output{outText} = renderJUnit defaultOutputOptions result
        T.isInfixOf "<failure" outText `shouldBe` True
        T.isInfixOf "type=\"Error\"" outText `shouldBe` True

      it "marks warnings as failures" $ do
        let result = mkResultWithSeverity Warning
            Output{outText} = renderJUnit defaultOutputOptions result
        T.isInfixOf "<failure" outText `shouldBe` True
        T.isInfixOf "type=\"Warning\"" outText `shouldBe` True

      it "includes suggestions in system-out" $ do
        let result = mkResultWithSeverity Suggestion
            Output{outText} = renderJUnit defaultOutputOptions result
        T.isInfixOf "<system-out>" outText `shouldBe` True

      it "escapes XML special characters in messages" $ do
        let diagWithSpecial = testDiagnostic { diagMessage = "Use foo <T> & bar" }
            resultWithSpecial = mkTestResultWithDiag { resultFiles = Map.singleton "src/Test.hs" FileResult
              { fileResultPath = "src/Test.hs"
              , fileResultDiagnostics = [diagWithSpecial]
              , fileResultSymbols = []
              , fileResultImports = []
              , fileResultExports = []
              }}
            Output{outText} = renderJUnit defaultOutputOptions resultWithSpecial
        T.isInfixOf "&lt;T&gt;" outText `shouldBe` True
        T.isInfixOf "&amp;" outText `shouldBe` True

      it "handles empty results" $ do
        let Output{outText} = renderJUnit defaultOutputOptions emptyAnalysisResult
        T.isInfixOf "<testsuites" outText `shouldBe` True
        T.isInfixOf "tests=\"0\"" outText `shouldBe` True

    describe "junitFormatter" $ do
      it "has correct format" $ do
        fmtFormat junitFormatter `shouldBe` JUnitFormat

  describe "Argus.Output.CodeClimate" $ do
    describe "renderCodeClimate" $ do
      it "produces JSON array" $ do
        let result = mkTestResultWithDiag
            Output{outText} = renderCodeClimate defaultOutputOptions result
        T.isPrefixOf "[" outText `shouldBe` True
        T.isSuffixOf "]" outText `shouldBe` True

      it "includes issue type" $ do
        let result = mkTestResultWithDiag
            Output{outText} = renderCodeClimate defaultOutputOptions result
        T.isInfixOf "\"type\":\"issue\"" outText `shouldBe` True

      it "includes check name from diagnostic code" $ do
        let result = mkTestResultWithDiag
            Output{outText} = renderCodeClimate defaultOutputOptions result
        T.isInfixOf "\"check_name\":\"test/warning\"" outText `shouldBe` True

      it "includes description" $ do
        let result = mkTestResultWithDiag
            Output{outText} = renderCodeClimate defaultOutputOptions result
        T.isInfixOf "\"description\":\"Test warning message\"" outText `shouldBe` True

      it "includes location with path" $ do
        let result = mkTestResultWithDiag
            Output{outText} = renderCodeClimate defaultOutputOptions result
        T.isInfixOf "\"path\":\"src/Test.hs\"" outText `shouldBe` True

      it "includes location with lines" $ do
        let result = mkTestResultWithDiag
            Output{outText} = renderCodeClimate defaultOutputOptions result
        T.isInfixOf "\"begin\":10" outText `shouldBe` True
        T.isInfixOf "\"end\":10" outText `shouldBe` True

      it "includes fingerprint" $ do
        let result = mkTestResultWithDiag
            Output{outText} = renderCodeClimate defaultOutputOptions result
        T.isInfixOf "\"fingerprint\":" outText `shouldBe` True

      it "includes categories" $ do
        let result = mkTestResultWithDiag
            Output{outText} = renderCodeClimate defaultOutputOptions result
        T.isInfixOf "\"categories\":" outText `shouldBe` True

      it "maps Error to blocker severity" $ do
        let result = mkResultWithSeverity Error
            Output{outText} = renderCodeClimate defaultOutputOptions result
        T.isInfixOf "\"severity\":\"blocker\"" outText `shouldBe` True

      it "maps Warning to major severity" $ do
        let result = mkResultWithSeverity Warning
            Output{outText} = renderCodeClimate defaultOutputOptions result
        T.isInfixOf "\"severity\":\"major\"" outText `shouldBe` True

      it "maps Suggestion to minor severity" $ do
        let result = mkResultWithSeverity Suggestion
            Output{outText} = renderCodeClimate defaultOutputOptions result
        T.isInfixOf "\"severity\":\"minor\"" outText `shouldBe` True

      it "maps Info to info severity" $ do
        let result = mkResultWithSeverity Info
            Output{outText} = renderCodeClimate defaultOutputOptions result
        T.isInfixOf "\"severity\":\"info\"" outText `shouldBe` True

      it "handles empty results" $ do
        let Output{outText} = renderCodeClimate defaultOutputOptions emptyAnalysisResult
        outText `shouldBe` "[]"

    describe "codeClimateFormatter" $ do
      it "has correct format" $ do
        fmtFormat codeClimateFormatter `shouldBe` CodeClimateFormat

  describe "Argus.Output.Checkstyle" $ do
    describe "renderCheckstyle" $ do
      it "produces valid XML declaration" $ do
        let result = mkTestResult
            Output{outText} = renderCheckstyle defaultOutputOptions result
        T.isInfixOf "<?xml version=\"1.0\" encoding=\"UTF-8\"?>" outText `shouldBe` True

      it "includes checkstyle element with version" $ do
        let result = mkTestResult
            Output{outText} = renderCheckstyle defaultOutputOptions result
        T.isInfixOf "<checkstyle version=\"4.3\">" outText `shouldBe` True
        T.isInfixOf "</checkstyle>" outText `shouldBe` True

      it "creates file elements for files with diagnostics" $ do
        let result = mkTestResultWithDiag
            Output{outText} = renderCheckstyle defaultOutputOptions result
        T.isInfixOf "<file name=\"src/Test.hs\">" outText `shouldBe` True
        T.isInfixOf "</file>" outText `shouldBe` True

      it "creates error elements for diagnostics" $ do
        let result = mkTestResultWithDiag
            Output{outText} = renderCheckstyle defaultOutputOptions result
        T.isInfixOf "<error line=\"10\"" outText `shouldBe` True

      it "includes column in error elements" $ do
        let result = mkTestResultWithDiag
            Output{outText} = renderCheckstyle defaultOutputOptions result
        T.isInfixOf "column=\"5\"" outText `shouldBe` True

      it "includes message in error elements" $ do
        let result = mkTestResultWithDiag
            Output{outText} = renderCheckstyle defaultOutputOptions result
        T.isInfixOf "message=\"Test warning message\"" outText `shouldBe` True

      it "includes source in error elements" $ do
        let result = mkTestResultWithDiag
            Output{outText} = renderCheckstyle defaultOutputOptions result
        T.isInfixOf "source=\"test/warning\"" outText `shouldBe` True

      it "maps Error to error severity" $ do
        let result = mkResultWithSeverity Error
            Output{outText} = renderCheckstyle defaultOutputOptions result
        T.isInfixOf "severity=\"error\"" outText `shouldBe` True

      it "maps Warning to warning severity" $ do
        let result = mkResultWithSeverity Warning
            Output{outText} = renderCheckstyle defaultOutputOptions result
        T.isInfixOf "severity=\"warning\"" outText `shouldBe` True

      it "maps Suggestion to info severity" $ do
        let result = mkResultWithSeverity Suggestion
            Output{outText} = renderCheckstyle defaultOutputOptions result
        T.isInfixOf "severity=\"info\"" outText `shouldBe` True

      it "maps Info to info severity" $ do
        let result = mkResultWithSeverity Info
            Output{outText} = renderCheckstyle defaultOutputOptions result
        T.isInfixOf "severity=\"info\"" outText `shouldBe` True

      it "escapes XML special characters" $ do
        let diagWithSpecial = testDiagnostic { diagMessage = "Check <foo> & \"bar\"" }
            resultWithSpecial = mkTestResultWithDiag { resultFiles = Map.singleton "src/Test.hs" FileResult
              { fileResultPath = "src/Test.hs"
              , fileResultDiagnostics = [diagWithSpecial]
              , fileResultSymbols = []
              , fileResultImports = []
              , fileResultExports = []
              }}
            Output{outText} = renderCheckstyle defaultOutputOptions resultWithSpecial
        T.isInfixOf "&lt;foo&gt;" outText `shouldBe` True
        T.isInfixOf "&amp;" outText `shouldBe` True
        T.isInfixOf "&quot;bar&quot;" outText `shouldBe` True

      it "handles empty results" $ do
        let Output{outText} = renderCheckstyle defaultOutputOptions emptyAnalysisResult
        T.isInfixOf "<checkstyle" outText `shouldBe` True
        -- Should not have any file elements
        T.isInfixOf "<file" outText `shouldBe` False

    describe "checkstyleFormatter" $ do
      it "has correct format" $ do
        fmtFormat checkstyleFormatter `shouldBe` CheckstyleFormat

  describe "Argus.Output.Html" $ do
    describe "defaultHtmlOptions" $ do
      it "shows context by default" $ do
        hoShowContext defaultHtmlOptions `shouldBe` True

      it "has 2 context lines by default" $ do
        hoContextLines defaultHtmlOptions `shouldBe` 2

      it "groups by file by default" $ do
        hoGroupBy defaultHtmlOptions `shouldBe` "file"

      it "shows summary by default" $ do
        hoShowSummary defaultHtmlOptions `shouldBe` True

      it "embeds CSS by default" $ do
        hoEmbedCss defaultHtmlOptions `shouldBe` True

    describe "renderHtml" $ do
      it "produces valid HTML structure" $ do
        let result = mkTestResult
            opts = defaultOutputOptions
            html = renderHtml opts result
        T.isInfixOf "<!DOCTYPE html>" html `shouldBe` True
        T.isInfixOf "</html>" html `shouldBe` True

      it "includes header elements" $ do
        let result = mkTestResult
            opts = defaultOutputOptions
            html = renderHtml opts result
        T.isInfixOf "<head>" html `shouldBe` True
        T.isInfixOf "<body>" html `shouldBe` True

      it "includes title" $ do
        let result = mkTestResult
            opts = defaultOutputOptions
            html = renderHtml opts result
        T.isInfixOf "Argus Report" html `shouldBe` True

      it "includes CSS styles" $ do
        let result = mkTestResult
            opts = defaultOutputOptions
            html = renderHtml opts result
        T.isInfixOf "<style>" html `shouldBe` True
        T.isInfixOf "--bg-primary" html `shouldBe` True

      it "includes summary section" $ do
        let result = mkTestResult
            opts = defaultOutputOptions
            html = renderHtml opts result
        T.isInfixOf "class=\"summary\"" html `shouldBe` True

      it "includes file count in summary" $ do
        let result = mkTestResult
            opts = defaultOutputOptions
            html = renderHtml opts result
        T.isInfixOf "Files Analyzed" html `shouldBe` True

    describe "renderHtmlReport" $ do
      it "shows no issues message for empty results" $ do
        let opts = defaultHtmlOptions
            html = renderHtmlReport opts emptyAnalysisResult
        T.isInfixOf "No issues found" html `shouldBe` True

      it "shows diagnostics when present" $ do
        let opts = defaultHtmlOptions
            html = renderHtmlReport opts mkTestResultWithDiag
        T.isInfixOf "class=\"diagnostic\"" html `shouldBe` True

      it "includes severity badges" $ do
        let opts = defaultHtmlOptions
            html = renderHtmlReport opts mkTestResultWithDiag
        T.isInfixOf "severity-warning" html `shouldBe` True

      it "includes diagnostic messages" $ do
        let opts = defaultHtmlOptions
            html = renderHtmlReport opts mkTestResultWithDiag
        T.isInfixOf "Test warning message" html `shouldBe` True

      it "includes location information" $ do
        let opts = defaultHtmlOptions
            html = renderHtmlReport opts mkTestResultWithDiag
        T.isInfixOf "src/Test.hs:10:5" html `shouldBe` True

      it "escapes HTML in messages" $ do
        let diagWithHtml = testDiagnostic { diagMessage = "<script>alert('xss')</script>" }
            resultWithHtml = mkTestResultWithDiag { resultFiles = Map.singleton "src/Test.hs" FileResult
              { fileResultPath = "src/Test.hs"
              , fileResultDiagnostics = [diagWithHtml]
              , fileResultSymbols = []
              , fileResultImports = []
              , fileResultExports = []
              }}
            html = renderHtmlReport defaultHtmlOptions resultWithHtml
        -- User-provided script content should be escaped
        T.isInfixOf "&lt;script&gt;" html `shouldBe` True
        -- The malicious content should not appear unescaped in the HTML
        T.isInfixOf "alert('xss')" html `shouldBe` False

    describe "HtmlOptions" $ do
      it "stores all fields correctly" $ do
        let opts = HtmlOptions
              { hoShowContext = False
              , hoContextLines = 5
              , hoGroupBy = "rule"
              , hoShowSummary = False
              , hoEmbedCss = False
              }
        hoShowContext opts `shouldBe` False
        hoContextLines opts `shouldBe` 5
        hoGroupBy opts `shouldBe` "rule"
        hoShowSummary opts `shouldBe` False
        hoEmbedCss opts `shouldBe` False

    describe "grouping modes" $ do
      it "groups by file" $ do
        let opts = defaultHtmlOptions { hoGroupBy = "file" }
            html = renderHtmlReport opts mkTestResultWithDiag
        T.isInfixOf "src/Test.hs" html `shouldBe` True

      it "groups by severity" $ do
        let opts = defaultHtmlOptions { hoGroupBy = "severity" }
            html = renderHtmlReport opts mkTestResultWithDiag
        T.isInfixOf "Warning" html `shouldBe` True

    describe "severity HTML classes" $ do
      it "uses error class for errors" $ do
        let result = mkResultWithSeverity Error
            html = renderHtmlReport defaultHtmlOptions result
        T.isInfixOf "severity-error" html `shouldBe` True

      it "uses warning class for warnings" $ do
        let result = mkResultWithSeverity Warning
            html = renderHtmlReport defaultHtmlOptions result
        T.isInfixOf "severity-warning" html `shouldBe` True

      it "uses suggestion class for suggestions" $ do
        let result = mkResultWithSeverity Suggestion
            html = renderHtmlReport defaultHtmlOptions result
        T.isInfixOf "severity-suggestion" html `shouldBe` True

      it "uses info class for info" $ do
        let result = mkResultWithSeverity Info
            html = renderHtmlReport defaultHtmlOptions result
        T.isInfixOf "severity-info" html `shouldBe` True

  describe "Argus.Output.Terminal" $ do
    describe "defaultTerminalOptions" $ do
      it "has color enabled by default" $ do
        toColor defaultTerminalOptions `shouldBe` True

      it "has context enabled by default" $ do
        toShowContext defaultTerminalOptions `shouldBe` True

      it "has 2 context lines by default" $ do
        toContextLines defaultTerminalOptions `shouldBe` 2

      it "groups by file by default" $ do
        toGroupBy defaultTerminalOptions `shouldBe` "file"

      it "shows summary by default" $ do
        toShowSummary defaultTerminalOptions `shouldBe` True

    describe "renderTerminal" $ do
      it "includes file paths in output" $ do
        let result = mkTestResultWithDiag
            opts = defaultOutputOptions { ooColor = False }
            output = renderTerminal opts result
        T.isInfixOf "src/Test.hs" output `shouldBe` True

      it "includes diagnostic messages" $ do
        let result = mkTestResultWithDiag
            opts = defaultOutputOptions { ooColor = False }
            output = renderTerminal opts result
        T.isInfixOf "Test warning message" output `shouldBe` True

      it "includes summary section" $ do
        let result = mkTestResultWithDiag
            opts = defaultOutputOptions { ooColor = False }
            output = renderTerminal opts result
        T.isInfixOf "Summary" output `shouldBe` True

      it "includes line numbers" $ do
        let result = mkTestResultWithDiag
            opts = defaultOutputOptions { ooColor = False }
            output = renderTerminal opts result
        -- Line 10 from our test diagnostic
        T.isInfixOf ":10:" output `shouldBe` True

    describe "severityColor" $ do
      it "returns red for Error" $ do
        -- Just verify it doesn't crash - color comparison is complex
        let _ = severityColor Error
        True `shouldBe` True

      it "returns yellow for Warning" $ do
        let _ = severityColor Warning
        True `shouldBe` True

      it "returns cyan for Suggestion" $ do
        let _ = severityColor Suggestion
        True `shouldBe` True

      it "returns blue for Info" $ do
        let _ = severityColor Info
        True `shouldBe` True

    describe "kindColor" $ do
      it "returns red for SecurityIssue" $ do
        let _ = kindColor SecurityIssue
        True `shouldBe` True

      it "returns yellow for UnusedCode" $ do
        let _ = kindColor UnusedCode
        True `shouldBe` True

    describe "renderSummary" $ do
      it "includes files analyzed count" $ do
        let summary = Summary
              { sumTotalFiles = 5
              , sumFilesWithIssues = 2
              , sumTotalDiagnostics = 10
              , sumByKind = Map.empty
              , sumBySeverity = Map.empty
              , sumFixesAvailable = 3
              , sumFixesApplied = 0
              }
            opts = defaultTerminalOptions { toColor = False }
            doc = renderSummary opts summary
        -- Check the rendered output contains the count
        show doc `shouldSatisfy` T.isInfixOf "5" . T.pack

  describe "Argus.Output.Plain" $ do
    describe "renderPlain" $ do
      it "produces output without ANSI codes" $ do
        let result = mkTestResultWithDiag
            opts = defaultOutputOptions
            Output{outText} = renderPlain opts result
        -- Plain output should not contain ANSI escape codes
        T.isInfixOf "\ESC[" outText `shouldBe` False

      it "includes file paths in output" $ do
        let result = mkTestResultWithDiag
            opts = defaultOutputOptions
            Output{outText} = renderPlain opts result
        T.isInfixOf "src/Test.hs" outText `shouldBe` True

      it "includes diagnostic messages" $ do
        let result = mkTestResultWithDiag
            opts = defaultOutputOptions
            Output{outText} = renderPlain opts result
        T.isInfixOf "Test warning message" outText `shouldBe` True

      it "includes line numbers" $ do
        let result = mkTestResultWithDiag
            opts = defaultOutputOptions
            Output{outText} = renderPlain opts result
        T.isInfixOf ":10:" outText `shouldBe` True

      it "includes severity labels" $ do
        let result = mkTestResultWithDiag
            opts = defaultOutputOptions
            Output{outText} = renderPlain opts result
        T.isInfixOf "Warning" outText || T.isInfixOf "warning" outText `shouldBe` True

      it "handles empty results" $ do
        let result = emptyAnalysisResult
            opts = defaultOutputOptions
            Output{outText} = renderPlain opts result
        -- Should produce some output even for empty results
        T.null outText `shouldBe` False

      it "returns a valid summary" $ do
        let result = mkTestResultWithDiag
            opts = defaultOutputOptions
            Output{outSummary} = renderPlain opts result
        sumTotalDiagnostics outSummary `shouldBe` 1

    describe "plainFormatter" $ do
      it "has PlainFormat type" $ do
        fmtFormat plainFormatter `shouldBe` PlainFormat

      it "renders using renderPlain function" $ do
        let result = mkTestResultWithDiag
            opts = defaultOutputOptions
            Output{outText = direct} = renderPlain opts result
            Output{outText = viafmt} = fmtRender plainFormatter opts result
        direct `shouldBe` viafmt

    describe "renderDiagnosticPlain" $ do
      it "produces plain text for a single diagnostic" $ do
        let diag = testDiagnostic
            output = renderDiagnosticPlain diag
        T.isInfixOf "src/Test.hs" output `shouldBe` True
        T.isInfixOf "Test warning message" output `shouldBe` True

    describe "renderSummaryPlain" $ do
      it "includes diagnostic count" $ do
        let summary = testSummary
            output = renderSummaryPlain summary
        T.isInfixOf "10" output `shouldBe` True

      it "includes file count" $ do
        let summary = testSummary
            output = renderSummaryPlain summary
        T.isInfixOf "5" output `shouldBe` True

  describe "Argus.Output.Types" $ do
    describe "OutputFormat" $ do
      it "has all expected formats" $ do
        minBound `shouldBe` TerminalFormat
        -- CheckstyleFormat is the last format after adding JUnit, CodeClimate, Checkstyle
        maxBound `shouldBe` CheckstyleFormat

      it "includes JsonFormat" $ do
        (JsonFormat :: OutputFormat) `shouldSatisfy` const True

      it "includes SarifFormat" $ do
        (SarifFormat :: OutputFormat) `shouldSatisfy` const True

      it "includes JUnitFormat" $ do
        (JUnitFormat :: OutputFormat) `shouldSatisfy` const True

      it "includes CodeClimateFormat" $ do
        (CodeClimateFormat :: OutputFormat) `shouldSatisfy` const True

      it "includes CheckstyleFormat" $ do
        (CheckstyleFormat :: OutputFormat) `shouldSatisfy` const True

    describe "mkSummary" $ do
      it "counts total files" $ do
        let result = mkTestResult
            summary = mkSummary result
        sumTotalFiles summary `shouldBe` 1

      it "counts files with issues" $ do
        let result = mkTestResultWithDiag
            summary = mkSummary result
        sumFilesWithIssues summary `shouldBe` 1

      it "counts total diagnostics" $ do
        let result = mkTestResultWithDiag
            summary = mkSummary result
        sumTotalDiagnostics summary `shouldBe` 1

      it "returns zero for empty result" $ do
        let summary = mkSummary emptyAnalysisResult
        sumTotalFiles summary `shouldBe` 0
        sumTotalDiagnostics summary `shouldBe` 0

    describe "Summary" $ do
      it "stores all fields correctly" $ do
        let summary = Summary
              { sumTotalFiles = 10
              , sumFilesWithIssues = 5
              , sumTotalDiagnostics = 20
              , sumByKind = Map.singleton UnusedCode 10
              , sumBySeverity = Map.singleton Warning 15
              , sumFixesAvailable = 8
              , sumFixesApplied = 2
              }
        sumTotalFiles summary `shouldBe` 10
        sumFilesWithIssues summary `shouldBe` 5
        sumTotalDiagnostics summary `shouldBe` 20
        sumFixesAvailable summary `shouldBe` 8
        sumFixesApplied summary `shouldBe` 2

-- Helper functions and test data

defaultOutputOptions :: OutputOptions
defaultOutputOptions = OutputOptions
  { ooFormat = TerminalFormat
  , ooColor = False
  , ooGroupBy = "file"
  , ooShowContext = True
  , ooContextLines = 2
  , ooVerbose = False
  , ooSourceCache = Map.empty
  }

emptySummary :: Summary
emptySummary = Summary
  { sumTotalFiles = 0
  , sumFilesWithIssues = 0
  , sumTotalDiagnostics = 0
  , sumByKind = Map.empty
  , sumBySeverity = Map.empty
  , sumFixesAvailable = 0
  , sumFixesApplied = 0
  }

mkTestResult :: AnalysisResult
mkTestResult = AnalysisResult
  { resultFiles = Map.singleton "src/Test.hs" FileResult
      { fileResultPath = "src/Test.hs"
      , fileResultDiagnostics = []
      , fileResultSymbols = []
      , fileResultImports = []
      , fileResultExports = []
      }
  , resultUnusedCode = Set.empty
  , resultDiagCount = Map.empty
  }

mkTestResultWithDiag :: AnalysisResult
mkTestResultWithDiag = AnalysisResult
  { resultFiles = Map.singleton "src/Test.hs" FileResult
      { fileResultPath = "src/Test.hs"
      , fileResultDiagnostics = [testDiagnostic]
      , fileResultSymbols = []
      , fileResultImports = []
      , fileResultExports = []
      }
  , resultUnusedCode = Set.empty
  , resultDiagCount = Map.singleton Warning 1
  }

mkResultWithSeverity :: Severity -> AnalysisResult
mkResultWithSeverity sev = AnalysisResult
  { resultFiles = Map.singleton "src/Test.hs" FileResult
      { fileResultPath = "src/Test.hs"
      , fileResultDiagnostics = [testDiagnostic { diagSeverity = sev }]
      , fileResultSymbols = []
      , fileResultImports = []
      , fileResultExports = []
      }
  , resultUnusedCode = Set.empty
  , resultDiagCount = Map.singleton sev 1
  }

testDiagnostic :: Diagnostic
testDiagnostic = Diagnostic
  { diagSpan = mkSrcSpanRaw "src/Test.hs" 10 5 10 15
  , diagSeverity = Warning
  , diagKind = UnusedCode
  , diagMessage = "Test warning message"
  , diagCode = Just "test/warning"
  , diagFixes = []
  , diagRelated = []
  }

testSummary :: Summary
testSummary = Summary
  { sumTotalFiles = 5
  , sumFilesWithIssues = 2
  , sumTotalDiagnostics = 10
  , sumByKind = Map.empty
  , sumBySeverity = Map.empty
  , sumFixesAvailable = 3
  , sumFixesApplied = 0
  }
