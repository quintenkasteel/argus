{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : DaemonSpec
-- Description : Comprehensive tests for Daemon functionality
-- Copyright   : (c) 2024
-- License     : MIT
--
-- Tests for daemon protocol, configuration, message encoding/decoding,
-- and all daemon types.
module DaemonSpec (spec) where

import Test.Hspec
import Data.Aeson (encode, decode, toJSON)
import Data.Aeson qualified as Aeson
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Time.Clock (UTCTime(..), secondsToDiffTime, addUTCTime)
import Data.Time.Calendar (fromGregorian)

import Argus.Daemon
import Argus.Types (Diagnostic(..), Severity(..), mkSrcSpanRaw, DiagnosticKind(..))

spec :: Spec
spec = do
  describe "Daemon" $ do
    daemonConfigSpec
    protocolSpec
    analyzeRequestSpec
    analyzeResponseSpec
    daemonStatusInfoSpec
    daemonStatsSpec
    daemonResponseSpec
    daemonMessageSpec
    socketPathSpec
    messageEncodingSpec
    jsonRoundTripSpec
    edgeCasesSpec

--------------------------------------------------------------------------------
-- DaemonConfig Tests
--------------------------------------------------------------------------------

daemonConfigSpec :: Spec
daemonConfigSpec = describe "DaemonConfig" $ do
  it "has reasonable defaults" $ do
    let cfg = defaultDaemonConfig
    dcSocketPath cfg `shouldBe` Nothing
    dcPort cfg `shouldBe` Nothing
    dcHost cfg `shouldBe` "127.0.0.1"
    dcMaxConnections cfg `shouldBe` 10
    dcIdleTimeout cfg `shouldBe` Nothing
    dcCacheSize cfg `shouldBe` 1000
    dcAutoReload cfg `shouldBe` True
    dcVerbose cfg `shouldBe` False

  it "has PID file as Nothing by default" $
    dcPidFile defaultDaemonConfig `shouldBe` Nothing

  it "has log file as Nothing by default" $
    dcLogFile defaultDaemonConfig `shouldBe` Nothing

  it "serializes to JSON and back" $ do
    let cfg = defaultDaemonConfig
          { dcSocketPath = Just "/tmp/test.sock"
          , dcPort = Just 9999
          , dcIdleTimeout = Just 300
          }
    let encoded = encode cfg
    decode encoded `shouldBe` Just cfg

  it "serializes all fields correctly" $ do
    let cfg = defaultDaemonConfig
          { dcSocketPath = Just "/var/run/argus.sock"
          , dcPort = Just 8080
          , dcHost = "0.0.0.0"
          , dcMaxConnections = 50
          , dcIdleTimeout = Just 600
          , dcCacheSize = 5000
          , dcAutoReload = False
          , dcPidFile = Just "/var/run/argus.pid"
          , dcLogFile = Just "/var/log/argus.log"
          , dcVerbose = True
          }
    decode (encode cfg) `shouldBe` Just cfg

  it "handles empty socket path" $ do
    let cfg = defaultDaemonConfig { dcSocketPath = Nothing }
    decode (encode cfg) `shouldBe` Just cfg

  it "handles zero max connections" $ do
    let cfg = defaultDaemonConfig { dcMaxConnections = 0 }
    decode (encode cfg) `shouldBe` Just cfg

  it "handles large cache size" $ do
    let cfg = defaultDaemonConfig { dcCacheSize = 1000000 }
    decode (encode cfg) `shouldBe` Just cfg

--------------------------------------------------------------------------------
-- Protocol Tests
--------------------------------------------------------------------------------

protocolSpec :: Spec
protocolSpec = describe "Protocol" $ do
  describe "DaemonRequest" $ do
    it "encodes and decodes ReqAnalyze" $ do
      let req = ReqAnalyze AnalyzeRequest
            { arPaths = ["src/"]
            , arForce = False
            , arQuiet = False
            , arTimeout = Just 30
            }
      decode (encode req) `shouldBe` Just req

    it "encodes and decodes ReqStatus" $ do
      let req = ReqStatus
      decode (encode req) `shouldBe` Just req

    it "encodes and decodes ReqStats" $ do
      let req = ReqStats
      decode (encode req) `shouldBe` Just req

    it "encodes and decodes ReqPing" $ do
      let req = ReqPing
      decode (encode req) `shouldBe` Just req

    it "encodes and decodes ReqShutdown" $ do
      let req = ReqShutdown
      decode (encode req) `shouldBe` Just req

    it "encodes and decodes ReqReload" $ do
      let req = ReqReload
      decode (encode req) `shouldBe` Just req

    it "encodes and decodes ReqClearCache" $ do
      let req = ReqClearCache
      decode (encode req) `shouldBe` Just req

  describe "all request types round-trip" $ do
    it "handles all simple requests" $ do
      let requests = [ReqStatus, ReqStats, ReqPing, ReqShutdown, ReqReload, ReqClearCache]
      mapM_ (\req -> decode (encode req) `shouldBe` Just req) requests

--------------------------------------------------------------------------------
-- AnalyzeRequest Tests
--------------------------------------------------------------------------------

analyzeRequestSpec :: Spec
analyzeRequestSpec = describe "AnalyzeRequest" $ do
  it "encodes and decodes empty paths" $ do
    let req = AnalyzeRequest [] False False Nothing
    decode (encode req) `shouldBe` Just req

  it "encodes and decodes with force flag" $ do
    let req = AnalyzeRequest ["test.hs"] True False Nothing
    decode (encode req) `shouldBe` Just req

  it "encodes and decodes with quiet flag" $ do
    let req = AnalyzeRequest ["test.hs"] False True Nothing
    decode (encode req) `shouldBe` Just req

  it "encodes and decodes with custom timeout" $ do
    let req = AnalyzeRequest ["test.hs"] False False (Just 120)
    decode (encode req) `shouldBe` Just req

  it "encodes and decodes with all flags set" $ do
    let req = AnalyzeRequest ["src/", "test/", "app/Main.hs"] True True (Just 300)
    decode (encode req) `shouldBe` Just req

  it "handles single file path" $ do
    let req = AnalyzeRequest ["Main.hs"] False False Nothing
    decode (encode req) `shouldBe` Just req

  it "handles multiple file paths" $ do
    let req = AnalyzeRequest ["a.hs", "b.hs", "c.hs", "d.hs", "e.hs"] False False Nothing
    decode (encode req) `shouldBe` Just req

  it "handles paths with special characters" $ do
    let req = AnalyzeRequest ["path with spaces/file.hs", "path/with-dashes.hs", "path_with_underscores.hs"] False False Nothing
    decode (encode req) `shouldBe` Just req

  it "handles very long paths" $ do
    let longPath = T.unpack $ T.replicate 100 "dir/" <> "file.hs"
        req = AnalyzeRequest [longPath] False False Nothing
    decode (encode req) `shouldBe` Just req

  it "handles zero timeout" $ do
    let req = AnalyzeRequest ["test.hs"] False False (Just 0)
    decode (encode req) `shouldBe` Just req

  it "handles large timeout" $ do
    let req = AnalyzeRequest ["test.hs"] False False (Just 86400)  -- 24 hours
    decode (encode req) `shouldBe` Just req

--------------------------------------------------------------------------------
-- AnalyzeResponse Tests
--------------------------------------------------------------------------------

analyzeResponseSpec :: Spec
analyzeResponseSpec = describe "AnalyzeResponse" $ do
  it "encodes and decodes with empty diagnostics" $ do
    let resp = AnalyzeResponse
          { arDiagnostics = []
          , arFileCount = 10
          , arErrorCount = 0
          , arWarningCount = 0
          , arCacheHits = 10
          , arCacheMisses = 0
          , arElapsedMs = 50.0
          }
    decode (encode resp) `shouldBe` Just resp

  it "encodes and decodes with diagnostics" $ do
    let diag = Diagnostic
          { diagSpan = mkSrcSpanRaw "test.hs" 5 1 5 20
          , diagSeverity = Warning
          , diagKind = CodePattern
          , diagMessage = "Use null instead of length"
          , diagCode = Just "perf/null"
          , diagFixes = []
          , diagRelated = []
          }
        resp = AnalyzeResponse
          { arDiagnostics = [diag]
          , arFileCount = 1
          , arErrorCount = 0
          , arWarningCount = 1
          , arCacheHits = 0
          , arCacheMisses = 1
          , arElapsedMs = 123.45
          }
    decode (encode resp) `shouldBe` Just resp

  it "handles zero counts" $ do
    let resp = AnalyzeResponse [] 0 0 0 0 0 0.0
    decode (encode resp) `shouldBe` Just resp

  it "handles large counts" $ do
    let resp = AnalyzeResponse [] 10000 500 2000 8000 2000 5000.0
    decode (encode resp) `shouldBe` Just resp

  it "handles precise elapsed time" $ do
    let resp = AnalyzeResponse [] 1 0 0 1 0 0.001
    decode (encode resp) `shouldBe` Just resp

--------------------------------------------------------------------------------
-- DaemonStatusInfo Tests
--------------------------------------------------------------------------------

daemonStatusInfoSpec :: Spec
daemonStatusInfoSpec = describe "DaemonStatusInfo" $ do
  it "encodes and decodes" $ do
    let info = DaemonStatusInfo
          { dsiRunning = True
          , dsiUptime = 3600.0
          , dsiConnections = 2
          , dsiCachedFiles = 100
          , dsiVersion = "1.0.0"
          }
    decode (encode info) `shouldBe` Just info

  it "handles not running state" $ do
    let info = DaemonStatusInfo
          { dsiRunning = False
          , dsiUptime = 0.0
          , dsiConnections = 0
          , dsiCachedFiles = 0
          , dsiVersion = "1.0.0"
          }
    decode (encode info) `shouldBe` Just info

  it "handles large uptime" $ do
    let info = DaemonStatusInfo
          { dsiRunning = True
          , dsiUptime = 31536000.0  -- 1 year in seconds
          , dsiConnections = 0
          , dsiCachedFiles = 0
          , dsiVersion = "1.0.0"
          }
    decode (encode info) `shouldBe` Just info

  it "handles many connections" $ do
    let info = DaemonStatusInfo True 100.0 1000 500 "1.0.0"
    decode (encode info) `shouldBe` Just info

  it "handles different version formats" $ do
    let versions = ["0.0.1", "1.0.0", "2.5.3", "10.20.30", "1.0.0-beta.1"]
    mapM_ (\v -> do
      let info = DaemonStatusInfo True 0.0 0 0 v
      decode (encode info) `shouldBe` Just info) versions

--------------------------------------------------------------------------------
-- DaemonStats Tests
--------------------------------------------------------------------------------

daemonStatsSpec :: Spec
daemonStatsSpec = describe "DaemonStats" $ do
  it "encodes and decodes" $ do
    let testTime = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)
        stats = DaemonStats
          { dsTotalRequests    = 100
          , dsTotalAnalyses    = 50
          , dsCacheHits        = 30
          , dsCacheMisses      = 20
          , dsTotalFiles       = 500
          , dsStartTime        = testTime
          , dsLastAnalysisTime = Just testTime
          , dsAvgAnalysisMs    = 150.5
          }
    decode (encode stats) `shouldBe` Just stats

  it "handles no last analysis time" $ do
    let testTime = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)
        stats = DaemonStats
          { dsTotalRequests    = 0
          , dsTotalAnalyses    = 0
          , dsCacheHits        = 0
          , dsCacheMisses      = 0
          , dsTotalFiles       = 0
          , dsStartTime        = testTime
          , dsLastAnalysisTime = Nothing
          , dsAvgAnalysisMs    = 0.0
          }
    decode (encode stats) `shouldBe` Just stats

  it "handles zero values" $ do
    let testTime = UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime 0)
        stats = DaemonStats 0 0 0 0 0 testTime Nothing 0.0
    decode (encode stats) `shouldBe` Just stats

  it "handles large values" $ do
    let testTime = UTCTime (fromGregorian 2024 12 31) (secondsToDiffTime 86399)
        stats = DaemonStats
          { dsTotalRequests    = 1000000
          , dsTotalAnalyses    = 500000
          , dsCacheHits        = 400000
          , dsCacheMisses      = 100000
          , dsTotalFiles       = 2000000
          , dsStartTime        = testTime
          , dsLastAnalysisTime = Just testTime
          , dsAvgAnalysisMs    = 50.123456
          }
    decode (encode stats) `shouldBe` Just stats

  it "handles different time zones" $ do
    let times =
          [ UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime 0)
          , UTCTime (fromGregorian 2024 6 15) (secondsToDiffTime 43200)
          , UTCTime (fromGregorian 2024 12 31) (secondsToDiffTime 86399)
          ]
    mapM_ (\t -> do
      let stats = DaemonStats 0 0 0 0 0 t Nothing 0.0
      decode (encode stats) `shouldBe` Just stats) times

--------------------------------------------------------------------------------
-- DaemonResponse Tests
--------------------------------------------------------------------------------

daemonResponseSpec :: Spec
daemonResponseSpec = describe "DaemonResponse" $ do
  it "encodes and decodes RespOk" $ do
    let resp = RespOk "Success"
    decode (encode resp) `shouldBe` Just resp

  it "encodes and decodes RespError" $ do
    let resp = RespError "Something went wrong"
    decode (encode resp) `shouldBe` Just resp

  it "encodes and decodes RespPong" $ do
    let testTime = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 43200)
        resp = RespPong testTime
    decode (encode resp) `shouldBe` Just resp

  it "encodes and decodes RespAnalyze" $ do
    let resp = RespAnalyze AnalyzeResponse
          { arDiagnostics = []
          , arFileCount = 5
          , arErrorCount = 0
          , arWarningCount = 2
          , arCacheHits = 3
          , arCacheMisses = 2
          , arElapsedMs = 100.0
          }
    decode (encode resp) `shouldBe` Just resp

  it "encodes and decodes RespStatus" $ do
    let resp = RespStatus DaemonStatusInfo
          { dsiRunning = True
          , dsiUptime = 1000.0
          , dsiConnections = 5
          , dsiCachedFiles = 200
          , dsiVersion = "1.0.0"
          }
    decode (encode resp) `shouldBe` Just resp

  it "encodes and decodes RespStats" $ do
    let testTime = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 0)
        resp = RespStats DaemonStats
          { dsTotalRequests = 100
          , dsTotalAnalyses = 50
          , dsCacheHits = 30
          , dsCacheMisses = 20
          , dsTotalFiles = 500
          , dsStartTime = testTime
          , dsLastAnalysisTime = Just testTime
          , dsAvgAnalysisMs = 75.0
          }
    decode (encode resp) `shouldBe` Just resp

  it "handles empty RespOk message" $ do
    let resp = RespOk ""
    decode (encode resp) `shouldBe` Just resp

  it "handles long RespError message" $ do
    let longMsg = T.replicate 1000 "error "
        resp = RespError longMsg
    decode (encode resp) `shouldBe` Just resp

  it "handles RespOk with unicode" $ do
    let resp = RespOk "Success: \x2713 Configuration reloaded \x1F44D"
    decode (encode resp) `shouldBe` Just resp

--------------------------------------------------------------------------------
-- DaemonMessage Tests
--------------------------------------------------------------------------------

daemonMessageSpec :: Spec
daemonMessageSpec = describe "DaemonMessage" $ do
  it "encodes and decodes with ID" $ do
    let msg = DaemonMessage
          { dmId = Just 42
          , dmMethod = "analyze"
          , dmParams = Just $ toJSON (["src/"] :: [String])
          }
    decode (encode msg) `shouldBe` Just msg

  it "encodes and decodes without ID" $ do
    let msg = DaemonMessage
          { dmId = Nothing
          , dmMethod = "ping"
          , dmParams = Nothing
          }
    decode (encode msg) `shouldBe` Just msg

  it "encodes and decodes with complex params" $ do
    let params = Aeson.object
          [ "paths" Aeson..= (["src/", "test/"] :: [String])
          , "force" Aeson..= True
          , "timeout" Aeson..= (30 :: Int)
          ]
        msg = DaemonMessage (Just 1) "analyze" (Just params)
    decode (encode msg) `shouldBe` Just msg

  it "handles various method names" $ do
    let methods = ["ping", "status", "analyze", "shutdown", "reload", "clear_cache"]
    mapM_ (\m -> do
      let msg = DaemonMessage Nothing m Nothing
      decode (encode msg) `shouldBe` Just msg) methods

  it "handles large ID values" $ do
    let msg = DaemonMessage (Just maxBound) "test" Nothing
    decode (encode msg) `shouldBe` Just msg

  it "handles zero ID" $ do
    let msg = DaemonMessage (Just 0) "test" Nothing
    decode (encode msg) `shouldBe` Just msg

  it "handles negative ID" $ do
    let msg = DaemonMessage (Just (-1)) "test" Nothing
    decode (encode msg) `shouldBe` Just msg

--------------------------------------------------------------------------------
-- Socket Path Tests
--------------------------------------------------------------------------------

socketPathSpec :: Spec
socketPathSpec = describe "Socket Path" $ do
  it "returns a valid path" $ do
    path <- getDefaultSocketPath
    path `shouldSatisfy` (not . null)
    path `shouldSatisfy` T.isInfixOf "daemon.sock" . T.pack

  it "returns a valid PID file path" $ do
    path <- getDaemonPidFile
    path `shouldSatisfy` (not . null)
    path `shouldSatisfy` T.isInfixOf "daemon.pid" . T.pack

  it "socket path contains argus directory" $ do
    path <- getDefaultSocketPath
    path `shouldSatisfy` T.isInfixOf "argus" . T.pack

  it "PID path contains argus directory" $ do
    path <- getDaemonPidFile
    path `shouldSatisfy` T.isInfixOf "argus" . T.pack

--------------------------------------------------------------------------------
-- Message Encoding Tests
--------------------------------------------------------------------------------

messageEncodingSpec :: Spec
messageEncodingSpec = describe "Message Encoding" $ do
  it "encodes messages with length prefix" $ do
    let msg = ReqPing
        encoded = encodeMessage msg
    -- Should have a newline in the header (10 is '\n' in ASCII)
    BS.elem 10 encoded `shouldBe` True

  it "decodes messages" $ do
    let msg = ReqPing
        encoded = BL.toStrict $ encode msg
    decodeMessage encoded `shouldBe` Just msg

  it "encodes and decodes ReqStatus" $ do
    let msg = ReqStatus
        encoded = BL.toStrict $ encode msg
    decodeMessage encoded `shouldBe` Just msg

  it "encodes and decodes ReqAnalyze" $ do
    let msg = ReqAnalyze $ AnalyzeRequest ["test.hs"] True False (Just 60)
        encoded = BL.toStrict $ encode msg
    decodeMessage encoded `shouldBe` Just msg

  it "encodes complex analyze request" $ do
    let req = ReqAnalyze $ AnalyzeRequest
          ["src/", "test/", "app/Main.hs", "bench/"]
          True True (Just 300)
        encoded = encodeMessage req
    BS.length encoded `shouldSatisfy` (> 0)

  it "decodes response messages" $ do
    let resp = RespOk "Done"
        encoded = BL.toStrict $ encode resp
    decodeMessage encoded `shouldBe` Just resp

  it "decodes analyze response" $ do
    let resp = RespAnalyze $ AnalyzeResponse [] 10 0 5 8 2 150.0
        encoded = BL.toStrict $ encode resp
    decodeMessage encoded `shouldBe` Just resp

  it "handles unicode in messages" $ do
    let resp = RespOk "\x2713 Analysis complete \x1F44D"
        encoded = BL.toStrict $ encode resp
    decodeMessage encoded `shouldBe` Just resp

--------------------------------------------------------------------------------
-- JSON Round-Trip Tests
--------------------------------------------------------------------------------

jsonRoundTripSpec :: Spec
jsonRoundTripSpec = describe "JSON round-trip properties" $ do
  it "DaemonConfig round-trips" $ do
    let configs =
          [ defaultDaemonConfig
          , defaultDaemonConfig { dcSocketPath = Just "/tmp/test.sock" }
          , defaultDaemonConfig { dcPort = Just 9999, dcHost = "0.0.0.0" }
          , defaultDaemonConfig { dcIdleTimeout = Just 300, dcVerbose = True }
          ]
    mapM_ (\cfg -> decode (encode cfg) `shouldBe` Just cfg) configs

  it "DaemonRequest round-trips" $ do
    let requests =
          [ ReqPing
          , ReqStatus
          , ReqStats
          , ReqShutdown
          , ReqReload
          , ReqClearCache
          , ReqAnalyze $ AnalyzeRequest [] False False Nothing
          , ReqAnalyze $ AnalyzeRequest ["src/"] True True (Just 60)
          ]
    mapM_ (\req -> decode (encode req) `shouldBe` Just req) requests

  it "DaemonResponse round-trips" $ do
    let testTime = UTCTime (fromGregorian 2024 1 15) (secondsToDiffTime 0)
        responses =
          [ RespOk "Success"
          , RespError "Failed"
          , RespPong testTime
          , RespAnalyze $ AnalyzeResponse [] 0 0 0 0 0 0.0
          , RespStatus $ DaemonStatusInfo True 100.0 2 50 "1.0.0"
          , RespStats $ DaemonStats 100 50 30 20 500 testTime Nothing 75.0
          ]
    mapM_ (\resp -> decode (encode resp) `shouldBe` Just resp) responses

  it "AnalyzeRequest round-trips" $ do
    let requests =
          [ AnalyzeRequest [] False False Nothing
          , AnalyzeRequest ["test.hs"] True False Nothing
          , AnalyzeRequest ["a.hs", "b.hs"] False True (Just 30)
          , AnalyzeRequest ["src/", "test/", "bench/"] True True (Just 300)
          ]
    mapM_ (\req -> decode (encode req) `shouldBe` Just req) requests

  it "DaemonMessage round-trips" $ do
    let messages =
          [ DaemonMessage Nothing "ping" Nothing
          , DaemonMessage (Just 1) "analyze" Nothing
          , DaemonMessage (Just 42) "status" (Just $ toJSON True)
          ]
    mapM_ (\msg -> decode (encode msg) `shouldBe` Just msg) messages

--------------------------------------------------------------------------------
-- Edge Cases Tests
--------------------------------------------------------------------------------

edgeCasesSpec :: Spec
edgeCasesSpec = describe "Edge cases" $ do
  it "handles empty AnalyzeRequest paths" $ do
    let req = AnalyzeRequest [] False False Nothing
    decode (encode req) `shouldBe` Just req

  it "handles AnalyzeResponse with many diagnostics" $ do
    let diags = [Diagnostic
          { diagSpan = mkSrcSpanRaw "test.hs" n 1 n 10
          , diagSeverity = Warning
          , diagKind = CodePattern
          , diagMessage = "Warning " <> T.pack (show n)
          , diagCode = Nothing
          , diagFixes = []
          , diagRelated = []
          } | n <- [1..100]]
        resp = AnalyzeResponse diags 1 0 100 0 1 500.0
    -- Just verify it encodes without error
    BL.length (encode resp) `shouldSatisfy` (> 0)

  it "handles special characters in paths" $ do
    let paths =
          [ "path with spaces/file.hs"
          , "path/with-dashes/file.hs"
          , "path_with_underscores/file.hs"
          , "path.with.dots/file.hs"
          , "path/with/深/unicode/文件.hs"
          ]
        req = AnalyzeRequest paths False False Nothing
    decode (encode req) `shouldBe` Just req

  it "handles very long error messages" $ do
    let longMsg = T.replicate 10000 "error "
        resp = RespError longMsg
    decode (encode resp) `shouldBe` Just resp

  it "handles empty error message" $ do
    let resp = RespError ""
    decode (encode resp) `shouldBe` Just resp

  it "handles DaemonStatusInfo with zero values" $ do
    let info = DaemonStatusInfo False 0.0 0 0 "0.0.0"
    decode (encode info) `shouldBe` Just info

  it "handles DaemonStats with all zeros" $ do
    let testTime = UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime 0)
        stats = DaemonStats 0 0 0 0 0 testTime Nothing 0.0
    decode (encode stats) `shouldBe` Just stats

  it "handles empty method name in DaemonMessage" $ do
    let msg = DaemonMessage Nothing "" Nothing
    decode (encode msg) `shouldBe` Just msg

  it "handles decodeMessage with invalid JSON" $
    (decodeMessage "not json" :: Maybe DaemonRequest) `shouldBe` Nothing

  it "handles decodeMessage with empty input" $
    (decodeMessage "" :: Maybe DaemonRequest) `shouldBe` Nothing

  it "handles decodeMessage with valid JSON but wrong type" $
    (decodeMessage "{\"foo\": 1}" :: Maybe DaemonRequest) `shouldBe` Nothing

  it "handles time differences correctly" $ do
    let time1 = UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime 0)
        time2 = addUTCTime 3600 time1  -- Add 1 hour
        stats = DaemonStats 10 5 3 2 50 time1 (Just time2) 100.0
    decode (encode stats) `shouldBe` Just stats

  it "handles maximum int values in stats" $ do
    let testTime = UTCTime (fromGregorian 2024 1 1) (secondsToDiffTime 0)
        stats = DaemonStats maxBound maxBound maxBound maxBound maxBound testTime Nothing 0.0
    decode (encode stats) `shouldBe` Just stats

  it "handles config with all optional fields set" $ do
    let cfg = DaemonConfig
          { dcSocketPath = Just "/tmp/argus.sock"
          , dcPort = Just 9999
          , dcHost = "0.0.0.0"
          , dcMaxConnections = 100
          , dcIdleTimeout = Just 600
          , dcCacheSize = 10000
          , dcAutoReload = True
          , dcResourceConfig = defaultDaemonConfig.dcResourceConfig
          , dcPidFile = Just "/tmp/argus.pid"
          , dcLogFile = Just "/tmp/argus.log"
          , dcVerbose = True
          }
    decode (encode cfg) `shouldBe` Just cfg

  it "handles newlines in RespOk message" $ do
    let resp = RespOk "Line 1\nLine 2\nLine 3"
    decode (encode resp) `shouldBe` Just resp

  it "handles tabs in RespError message" $ do
    let resp = RespError "Error:\tdetails\there"
    decode (encode resp) `shouldBe` Just resp
