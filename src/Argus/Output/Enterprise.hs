{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}

-- |
-- Module      : Argus.Output.Enterprise
-- Description : Enterprise-grade reporting for large-scale deployments
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides comprehensive reporting capabilities for enterprise
-- deployments of Argus, including:
--
-- * Executive summary reports
-- * Team and project-level aggregation
-- * Trend analysis over time
-- * Compliance and SLA reporting
-- * Multi-format export (PDF-ready HTML, CSV, Excel-compatible)
--
-- == Usage
--
-- @
-- config <- loadEnterpriseConfig "enterprise.yaml"
-- report <- generateEnterpriseReport config analysisResults
-- exportReport ReportFormatHTML "output/report.html" report
-- @
module Argus.Output.Enterprise
  ( -- * Configuration
    EnterpriseConfig (..)
  , defaultEnterpriseConfig
  , ReportPeriod (..)
  , ComplianceStandard (..)

    -- * Report Types
  , EnterpriseReport (..)
  , ExecutiveSummary (..)
  , TeamReport (..)
  , ProjectReport (..)
  , TrendAnalysis (..)
  , TrendDataPoint (..)
  , ComplianceReport (..)
  , ComplianceRule (..)
  , ComplianceStatus (..)
  , SLAReport (..)
  , SLAMetric (..)
  , SLAStatus (..)

    -- * Aggregation
  , IssueAggregate (..)
  , CategoryBreakdown (..)
  , SeverityBreakdown (..)
  , RuleFrequency (..)

    -- * Report Generation
  , generateEnterpriseReport
  , generateExecutiveSummary
  , generateTeamReport
  , generateProjectReport
  , generateTrendAnalysis
  , generateComplianceReport
  , generateSLAReport

    -- * Export Functions
  , ReportFormat (..)
  , exportReport
  , exportReportHTML
  , exportReportCSV
  , exportReportJSON
  , exportReportMarkdown

    -- * Historical Data
  , HistoricalData (..)
  , HistoricalSnapshot (..)
  , loadHistoricalData
  , saveHistoricalSnapshot
  , computeTrends
  ) where

import Data.Aeson (ToJSON(..), FromJSON(..), encode)
import Data.ByteString.Lazy qualified as BL
import Data.List (sortOn, nub)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Maybe (mapMaybe)
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Encoding qualified as TE
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder qualified as TB
import Data.Time (UTCTime, getCurrentTime)
import GHC.Generics (Generic)
import System.Directory (doesFileExist, createDirectoryIfMissing)
import System.FilePath (takeDirectory)

import Argus.Types
  ( AnalysisResult(..)
  , FileResult(..)
  , Diagnostic(..)
  , DiagnosticKind(..)
  , Severity(..)
  , SrcSpan(..)
  , srcSpanStartLineRaw
  )

--------------------------------------------------------------------------------
-- Configuration
--------------------------------------------------------------------------------

-- | Enterprise reporting configuration
data EnterpriseConfig = EnterpriseConfig
  { ecOrganization    :: Text            -- ^ Organization name
  , ecTeams           :: [TeamConfig]    -- ^ Team configurations
  , ecProjects        :: [ProjectConfig] -- ^ Project configurations
  , ecComplianceStd   :: [ComplianceStandard]  -- ^ Compliance standards to check
  , ecSLATargets      :: SLATargets      -- ^ SLA targets
  , ecReportPeriod    :: ReportPeriod    -- ^ Reporting period
  , ecHistoricalPath  :: Maybe FilePath  -- ^ Path to historical data
  , ecIncludeTrends   :: Bool            -- ^ Include trend analysis
  , ecIncludeTeams    :: Bool            -- ^ Include team breakdown
  , ecIncludeProjects :: Bool            -- ^ Include project breakdown
  , ecTopIssueCount   :: Int             -- ^ Number of top issues to highlight
  , ecCustomBranding  :: Maybe BrandingConfig  -- ^ Custom branding
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Team configuration
data TeamConfig = TeamConfig
  { tcName       :: Text      -- ^ Team name
  , tcOwners     :: [Text]    -- ^ Team owners (email)
  , tcPathPrefix :: [Text]    -- ^ Path prefixes owned by team
  , tcModules    :: [Text]    -- ^ Module patterns owned by team
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Project configuration
data ProjectConfig = ProjectConfig
  { pcName       :: Text      -- ^ Project name
  , pcPaths      :: [Text]    -- ^ Project paths
  , pcPriority   :: Int       -- ^ Project priority (1-5)
  , pcOwner      :: Maybe Text -- ^ Project owner
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Branding configuration
data BrandingConfig = BrandingConfig
  { bcLogoUrl      :: Maybe Text
  , bcPrimaryColor :: Text
  , bcCompanyName  :: Text
  , bcReportTitle  :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Reporting period
data ReportPeriod
  = PeriodDaily
  | PeriodWeekly
  | PeriodMonthly
  | PeriodQuarterly
  | PeriodCustom Int  -- ^ Custom period in days
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Compliance standards
data ComplianceStandard
  = ComplianceOWASP       -- ^ OWASP Top 10
  | ComplianceCWE         -- ^ Common Weakness Enumeration
  | ComplianceMISRA       -- ^ MISRA guidelines
  | ComplianceSOC2        -- ^ SOC 2 compliance
  | ComplianceCustom Text -- ^ Custom compliance standard
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | SLA targets
data SLATargets = SLATargets
  { stCriticalFixDays  :: Int    -- ^ Days to fix critical issues
  , stHighFixDays      :: Int    -- ^ Days to fix high severity issues
  , stMediumFixDays    :: Int    -- ^ Days to fix medium issues
  , stLowFixDays       :: Int    -- ^ Days to fix low issues
  , stMaxTechnicalDebt :: Int    -- ^ Maximum technical debt score
  , stMinCodeQuality   :: Double -- ^ Minimum code quality score (0-100)
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Default configuration
defaultEnterpriseConfig :: EnterpriseConfig
defaultEnterpriseConfig = EnterpriseConfig
  { ecOrganization = "Organization"
  , ecTeams = []
  , ecProjects = []
  , ecComplianceStd = []
  , ecSLATargets = defaultSLATargets
  , ecReportPeriod = PeriodWeekly
  , ecHistoricalPath = Nothing
  , ecIncludeTrends = True
  , ecIncludeTeams = True
  , ecIncludeProjects = True
  , ecTopIssueCount = 10
  , ecCustomBranding = Nothing
  }

defaultSLATargets :: SLATargets
defaultSLATargets = SLATargets
  { stCriticalFixDays = 1
  , stHighFixDays = 7
  , stMediumFixDays = 30
  , stLowFixDays = 90
  , stMaxTechnicalDebt = 100
  , stMinCodeQuality = 80.0
  }

--------------------------------------------------------------------------------
-- Report Types
--------------------------------------------------------------------------------

-- | Complete enterprise report
data EnterpriseReport = EnterpriseReport
  { erGenerated       :: UTCTime           -- ^ Report generation time
  , erPeriod          :: ReportPeriod      -- ^ Reporting period
  , erOrganization    :: Text              -- ^ Organization name
  , erExecutive       :: ExecutiveSummary  -- ^ Executive summary
  , erTeamReports     :: [TeamReport]      -- ^ Per-team reports
  , erProjectReports  :: [ProjectReport]   -- ^ Per-project reports
  , erTrendAnalysis   :: Maybe TrendAnalysis  -- ^ Trend analysis
  , erComplianceReport :: Maybe ComplianceReport  -- ^ Compliance report
  , erSLAReport       :: Maybe SLAReport   -- ^ SLA compliance report
  , erTopIssues       :: [IssueAggregate]  -- ^ Top recurring issues
  , erActionItems     :: [ActionItem]      -- ^ Recommended action items
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Executive summary for leadership
data ExecutiveSummary = ExecutiveSummary
  { esTotalFiles       :: Int              -- ^ Total files analyzed
  , esFilesWithIssues  :: Int              -- ^ Files with issues
  , esTotalIssues      :: Int              -- ^ Total issues found
  , esCriticalCount    :: Int              -- ^ Critical issues
  , esHighCount        :: Int              -- ^ High severity issues
  , esMediumCount      :: Int              -- ^ Medium severity issues
  , esLowCount         :: Int              -- ^ Low severity issues
  , esFixableCount     :: Int              -- ^ Auto-fixable issues
  , esCodeQualityScore :: Double           -- ^ Overall code quality (0-100)
  , esTechnicalDebt    :: Int              -- ^ Technical debt score
  , esSecurityScore    :: Double           -- ^ Security score (0-100)
  , esRiskLevel        :: RiskLevel        -- ^ Overall risk level
  , esCategoryBreakdown :: [CategoryBreakdown]
  , esSeverityBreakdown :: [SeverityBreakdown]
  , esKeyFindings      :: [Text]           -- ^ Key findings summary
  , esRecommendations  :: [Text]           -- ^ Top recommendations
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Risk level classification
data RiskLevel
  = RiskCritical
  | RiskHigh
  | RiskMedium
  | RiskLow
  | RiskMinimal
  deriving stock (Eq, Ord, Show, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Team-level report
data TeamReport = TeamReport
  { trTeamName        :: Text
  , trOwners          :: [Text]
  , trFileCount       :: Int
  , trIssueCount      :: Int
  , trCriticalCount   :: Int
  , trHighCount       :: Int
  , trQualityScore    :: Double
  , trTechnicalDebt   :: Int
  , trTopIssues       :: [RuleFrequency]
  , trTrend           :: Maybe TrendDirection
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Project-level report
data ProjectReport = ProjectReport
  { prProjectName     :: Text
  , prOwner           :: Maybe Text
  , prPriority        :: Int
  , prFileCount       :: Int
  , prIssueCount      :: Int
  , prCriticalCount   :: Int
  , prHighCount       :: Int
  , prQualityScore    :: Double
  , prTechnicalDebt   :: Int
  , prTopIssues       :: [RuleFrequency]
  , prSLAStatus       :: SLAStatus
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Trend analysis over time
data TrendAnalysis = TrendAnalysis
  { taDataPoints      :: [TrendDataPoint]
  , taOverallTrend    :: TrendDirection
  , taIssuesTrend     :: TrendDirection
  , taQualityTrend    :: TrendDirection
  , taDebtTrend       :: TrendDirection
  , taProjections     :: Maybe Projections
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Single trend data point
data TrendDataPoint = TrendDataPoint
  { tdpTimestamp      :: UTCTime
  , tdpTotalIssues    :: Int
  , tdpCriticalIssues :: Int
  , tdpQualityScore   :: Double
  , tdpTechnicalDebt  :: Int
  , tdpFilesAnalyzed  :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Trend direction
data TrendDirection
  = TrendImproving
  | TrendStable
  | TrendDeclining
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Future projections
data Projections = Projections
  { projDebtFreeDate   :: Maybe UTCTime  -- ^ Projected debt-free date
  , projQualityTarget  :: Maybe UTCTime  -- ^ When quality target will be met
  , projVelocity       :: Double         -- ^ Issues resolved per week
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Compliance report
data ComplianceReport = ComplianceReport
  { crStandard        :: ComplianceStandard
  , crOverallStatus   :: ComplianceStatus
  , crComplianceScore :: Double  -- ^ 0-100
  , crRules           :: [ComplianceRule]
  , crViolations      :: [ComplianceViolation]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Individual compliance rule
data ComplianceRule = ComplianceRule
  { cruId             :: Text
  , cruName           :: Text
  , cruDescription    :: Text
  , cruStatus         :: ComplianceStatus
  , cruViolationCount :: Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Compliance status
data ComplianceStatus
  = CompliancePass
  | ComplianceFail
  | CompliancePartial
  | ComplianceNotApplicable
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Compliance violation
data ComplianceViolation = ComplianceViolation
  { cvRuleId    :: Text
  , cvFilePath  :: Text
  , cvLine      :: Int
  , cvMessage   :: Text
  , cvSeverity  :: Severity
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | SLA compliance report
data SLAReport = SLAReport
  { slarOverallStatus  :: SLAStatus
  , slarMetrics        :: [SLAMetric]
  , slarViolations     :: [SLAViolation]
  , slarComplianceRate :: Double  -- ^ Percentage meeting SLA
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | SLA metric
data SLAMetric = SLAMetric
  { smName          :: Text
  , smTarget        :: Text
  , smActual        :: Text
  , smStatus        :: SLAStatus
  , smTrend         :: TrendDirection
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | SLA status
data SLAStatus
  = SLAMet
  | SLAAtRisk
  | SLABreached
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | SLA violation
data SLAViolation = SLAViolation
  { svMetric      :: Text
  , svExpected    :: Text
  , svActual      :: Text
  , svImpact      :: Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Issue aggregate for top issues
data IssueAggregate = IssueAggregate
  { iaRuleId        :: Text
  , iaRuleName      :: Text
  , iaCategory      :: Text
  , iaSeverity      :: Severity
  , iaOccurrences   :: Int
  , iaAffectedFiles :: Int
  , iaFixable       :: Bool
  , iaEstimatedFix  :: Maybe Text  -- ^ Estimated fix time
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Category breakdown
data CategoryBreakdown = CategoryBreakdown
  { cbCategory  :: Text
  , cbCount     :: Int
  , cbPercentage :: Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Severity breakdown
data SeverityBreakdown = SeverityBreakdown
  { sbSeverity   :: Severity
  , sbCount      :: Int
  , sbPercentage :: Double
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Rule frequency for top issues
data RuleFrequency = RuleFrequency
  { rfRuleId    :: Text
  , rfRuleName  :: Text
  , rfCount     :: Int
  , rfSeverity  :: Severity
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Recommended action item
data ActionItem = ActionItem
  { aiPriority    :: Int        -- ^ 1-5 (1 = highest)
  , aiTitle       :: Text
  , aiDescription :: Text
  , aiImpact      :: Text
  , aiEffort      :: Text       -- ^ "Low", "Medium", "High"
  , aiOwner       :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Historical Data
--------------------------------------------------------------------------------

-- | Historical data for trend analysis
data HistoricalData = HistoricalData
  { hdSnapshots :: [HistoricalSnapshot]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Single historical snapshot
data HistoricalSnapshot = HistoricalSnapshot
  { hsTimestamp      :: UTCTime
  , hsTotalFiles     :: Int
  , hsTotalIssues    :: Int
  , hsCriticalCount  :: Int
  , hsHighCount      :: Int
  , hsMediumCount    :: Int
  , hsLowCount       :: Int
  , hsQualityScore   :: Double
  , hsTechnicalDebt  :: Int
  , hsByCategory     :: Map Text Int
  , hsByRule         :: Map Text Int
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Report Generation
--------------------------------------------------------------------------------

-- | Generate complete enterprise report
generateEnterpriseReport :: EnterpriseConfig -> AnalysisResult -> IO EnterpriseReport
generateEnterpriseReport config result = do
  now <- getCurrentTime

  let executive = generateExecutiveSummary result
  let teamReports = if ecIncludeTeams config
                    then map (generateTeamReport result) (ecTeams config)
                    else []
  let projectReports = if ecIncludeProjects config
                       then map (generateProjectReport result) (ecProjects config)
                       else []

  -- Load historical data for trends if available
  historical <- case ecHistoricalPath config of
    Just path -> loadHistoricalData path
    Nothing -> pure $ HistoricalData []

  let trendAnalysis = if ecIncludeTrends config && not (null $ hdSnapshots historical)
                      then Just $ generateTrendAnalysis historical now result
                      else Nothing

  let complianceReports = map (generateComplianceReport result) (ecComplianceStd config)
  let complianceReport = case complianceReports of
        (c:_) -> Just c
        [] -> Nothing

  let slaReport = Just $ generateSLAReport (ecSLATargets config) result

  let topIssues = computeTopIssues (ecTopIssueCount config) result
  let actionItems = generateActionItems config executive topIssues

  pure EnterpriseReport
    { erGenerated = now
    , erPeriod = ecReportPeriod config
    , erOrganization = ecOrganization config
    , erExecutive = executive
    , erTeamReports = teamReports
    , erProjectReports = projectReports
    , erTrendAnalysis = trendAnalysis
    , erComplianceReport = complianceReport
    , erSLAReport = slaReport
    , erTopIssues = topIssues
    , erActionItems = actionItems
    }

-- | Generate executive summary
generateExecutiveSummary :: AnalysisResult -> ExecutiveSummary
generateExecutiveSummary result =
  let allDiags = getAllDiagnostics result
      totalFiles = Map.size (resultFiles result)
      filesWithIssues = length $ filter (not . null . fileResultDiagnostics) $
                        Map.elems (resultFiles result)
      totalIssues = length allDiags

      criticalCount = countSeverity Error allDiags
      highCount = countSeverity Warning allDiags
      mediumCount = countSeverity Suggestion allDiags
      lowCount = countSeverity Info allDiags

      fixableCount = length $ filter (not . null . diagFixes) allDiags

      qualityScore = computeQualityScore totalFiles totalIssues criticalCount highCount
      techDebt = computeTechnicalDebt allDiags
      securityScore = computeSecurityScore allDiags
      riskLevel = computeRiskLevel criticalCount highCount totalIssues

      categoryBreakdown = computeCategoryBreakdown allDiags
      severityBreakdown = computeSeverityBreakdown allDiags

      keyFindings = generateKeyFindings criticalCount highCount securityScore
      recommendations = generateRecommendations categoryBreakdown criticalCount fixableCount

  in ExecutiveSummary
    { esTotalFiles = totalFiles
    , esFilesWithIssues = filesWithIssues
    , esTotalIssues = totalIssues
    , esCriticalCount = criticalCount
    , esHighCount = highCount
    , esMediumCount = mediumCount
    , esLowCount = lowCount
    , esFixableCount = fixableCount
    , esCodeQualityScore = qualityScore
    , esTechnicalDebt = techDebt
    , esSecurityScore = securityScore
    , esRiskLevel = riskLevel
    , esCategoryBreakdown = categoryBreakdown
    , esSeverityBreakdown = severityBreakdown
    , esKeyFindings = keyFindings
    , esRecommendations = recommendations
    }

-- | Generate team report
generateTeamReport :: AnalysisResult -> TeamConfig -> TeamReport
generateTeamReport result TeamConfig{..} =
  let teamFiles = filterFilesByPrefixes tcPathPrefix (resultFiles result)
      teamDiags = concatMap fileResultDiagnostics $ Map.elems teamFiles
      fileCount = Map.size teamFiles
      issueCount = length teamDiags
      criticalCount = countSeverity Error teamDiags
      highCount = countSeverity Warning teamDiags
      qualityScore = computeQualityScore fileCount issueCount criticalCount highCount
      techDebt = computeTechnicalDebt teamDiags
      topIssues = computeRuleFrequencies 5 teamDiags
  in TeamReport
    { trTeamName = tcName
    , trOwners = tcOwners
    , trFileCount = fileCount
    , trIssueCount = issueCount
    , trCriticalCount = criticalCount
    , trHighCount = highCount
    , trQualityScore = qualityScore
    , trTechnicalDebt = techDebt
    , trTopIssues = topIssues
    , trTrend = Nothing  -- Would need historical data
    }

-- | Generate project report
generateProjectReport :: AnalysisResult -> ProjectConfig -> ProjectReport
generateProjectReport result ProjectConfig{..} =
  let projectFiles = filterFilesByPrefixes pcPaths (resultFiles result)
      projectDiags = concatMap fileResultDiagnostics $ Map.elems projectFiles
      fileCount = Map.size projectFiles
      issueCount = length projectDiags
      criticalCount = countSeverity Error projectDiags
      highCount = countSeverity Warning projectDiags
      qualityScore = computeQualityScore fileCount issueCount criticalCount highCount
      techDebt = computeTechnicalDebt projectDiags
      topIssues = computeRuleFrequencies 5 projectDiags
      slaStatus = if criticalCount > 0 then SLABreached
                  else if highCount > 5 then SLAAtRisk
                  else SLAMet
  in ProjectReport
    { prProjectName = pcName
    , prOwner = pcOwner
    , prPriority = pcPriority
    , prFileCount = fileCount
    , prIssueCount = issueCount
    , prCriticalCount = criticalCount
    , prHighCount = highCount
    , prQualityScore = qualityScore
    , prTechnicalDebt = techDebt
    , prTopIssues = topIssues
    , prSLAStatus = slaStatus
    }

-- | Generate trend analysis from historical data
generateTrendAnalysis :: HistoricalData -> UTCTime -> AnalysisResult -> TrendAnalysis
generateTrendAnalysis historical timestamp result =
  let currentSnapshot = resultToSnapshot timestamp result
      allSnapshots = hdSnapshots historical ++ [currentSnapshot]
      dataPoints = map snapshotToDataPoint allSnapshots

      -- Compute trends by comparing recent vs older data
      overallTrend = computeOverallTrend allSnapshots
      issuesTrend = computeIssuesTrend allSnapshots
      qualityTrend = computeQualityTrend allSnapshots
      debtTrend = computeDebtTrend allSnapshots

  in TrendAnalysis
    { taDataPoints = dataPoints
    , taOverallTrend = overallTrend
    , taIssuesTrend = issuesTrend
    , taQualityTrend = qualityTrend
    , taDebtTrend = debtTrend
    , taProjections = Nothing  -- Could compute projections based on velocity
    }

-- | Generate compliance report for a standard
generateComplianceReport :: AnalysisResult -> ComplianceStandard -> ComplianceReport
generateComplianceReport result standard =
  let allDiags = getAllDiagnostics result
      rules = getComplianceRules standard
      violations = findComplianceViolations standard allDiags

      passedRules = length $ filter (\r -> cruStatus r == CompliancePass) rules
      totalRules = length rules
      complianceScore = if totalRules == 0 then 100.0
                        else (fromIntegral passedRules / fromIntegral totalRules) * 100

      overallStatus = if null violations then CompliancePass
                      else if complianceScore >= 80 then CompliancePartial
                      else ComplianceFail

  in ComplianceReport
    { crStandard = standard
    , crOverallStatus = overallStatus
    , crComplianceScore = complianceScore
    , crRules = rules
    , crViolations = violations
    }

-- | Generate SLA report
generateSLAReport :: SLATargets -> AnalysisResult -> SLAReport
generateSLAReport SLATargets{..} result =
  let allDiags = getAllDiagnostics result
      criticalCount = countSeverity Error allDiags
      highCount = countSeverity Warning allDiags
      totalFiles = Map.size (resultFiles result)
      totalIssues = length allDiags

      qualityScore = computeQualityScore totalFiles totalIssues criticalCount highCount
      techDebt = computeTechnicalDebt allDiags

      metrics =
        [ SLAMetric
            { smName = "Critical Issues"
            , smTarget = "0"
            , smActual = T.pack $ show criticalCount
            , smStatus = if criticalCount == 0 then SLAMet else SLABreached
            , smTrend = TrendStable
            }
        , SLAMetric
            { smName = "Code Quality Score"
            , smTarget = T.pack $ show stMinCodeQuality <> "%"
            , smActual = T.pack $ show (round qualityScore :: Int) <> "%"
            , smStatus = if qualityScore >= stMinCodeQuality then SLAMet
                         else if qualityScore >= stMinCodeQuality - 10 then SLAAtRisk
                         else SLABreached
            , smTrend = TrendStable
            }
        , SLAMetric
            { smName = "Technical Debt"
            , smTarget = "< " <> T.pack (show stMaxTechnicalDebt)
            , smActual = T.pack $ show techDebt
            , smStatus = if techDebt <= stMaxTechnicalDebt then SLAMet
                         else if techDebt <= stMaxTechnicalDebt + 20 then SLAAtRisk
                         else SLABreached
            , smTrend = TrendStable
            }
        ]

      violations = mapMaybe metricToViolation metrics
      complianceRate = let met = length $ filter (\m -> smStatus m == SLAMet) metrics
                       in (fromIntegral met / fromIntegral (length metrics)) * 100

      overallStatus = if null violations then SLAMet
                      else if complianceRate >= 70 then SLAAtRisk
                      else SLABreached

  in SLAReport
    { slarOverallStatus = overallStatus
    , slarMetrics = metrics
    , slarViolations = violations
    , slarComplianceRate = complianceRate
    }
  where
    metricToViolation SLAMetric{..} =
      if smStatus == SLABreached
      then Just $ SLAViolation
        { svMetric = smName
        , svExpected = smTarget
        , svActual = smActual
        , svImpact = "Requires immediate attention"
        }
      else Nothing

--------------------------------------------------------------------------------
-- Export Functions
--------------------------------------------------------------------------------

-- | Report export format
data ReportFormat
  = ReportFormatHTML
  | ReportFormatCSV
  | ReportFormatJSON
  | ReportFormatMarkdown
  deriving stock (Eq, Show, Generic)

-- | Export report to file
exportReport :: ReportFormat -> FilePath -> EnterpriseReport -> IO ()
exportReport format path report = do
  createDirectoryIfMissing True (takeDirectory path)
  case format of
    ReportFormatHTML -> exportReportHTML path report
    ReportFormatCSV -> exportReportCSV path report
    ReportFormatJSON -> exportReportJSON path report
    ReportFormatMarkdown -> exportReportMarkdown path report

-- | Export as HTML
exportReportHTML :: FilePath -> EnterpriseReport -> IO ()
exportReportHTML path report = do
  let html = renderEnterpriseHTML report
  BL.writeFile path $ BL.fromStrict $ TE.encodeUtf8 html

-- | Export as CSV
exportReportCSV :: FilePath -> EnterpriseReport -> IO ()
exportReportCSV path report = do
  let csv = renderEnterpriseCSV report
  BL.writeFile path $ BL.fromStrict $ TE.encodeUtf8 csv

-- | Export as JSON
exportReportJSON :: FilePath -> EnterpriseReport -> IO ()
exportReportJSON path report = do
  BL.writeFile path $ encode report

-- | Export as Markdown
exportReportMarkdown :: FilePath -> EnterpriseReport -> IO ()
exportReportMarkdown path report = do
  let md = renderEnterpriseMarkdown report
  BL.writeFile path $ BL.fromStrict $ TE.encodeUtf8 md

--------------------------------------------------------------------------------
-- Rendering Functions
--------------------------------------------------------------------------------

-- | Render enterprise report as HTML
renderEnterpriseHTML :: EnterpriseReport -> Text
renderEnterpriseHTML EnterpriseReport{..} = TL.toStrict $ TB.toLazyText $ mconcat
  [ TB.fromText "<!DOCTYPE html>\n<html>\n<head>\n"
  , TB.fromText "<meta charset=\"utf-8\">\n"
  , TB.fromText "<title>Enterprise Code Quality Report</title>\n"
  , TB.fromText "<style>\n"
  , TB.fromText cssStyles
  , TB.fromText "</style>\n</head>\n<body>\n"
  , TB.fromText "<div class=\"container\">\n"

  -- Header
  , TB.fromText "<header>\n"
  , TB.fromText "<h1>Code Quality Report</h1>\n"
  , TB.fromText "<p class=\"org\">"
  , TB.fromText erOrganization
  , TB.fromText "</p>\n"
  , TB.fromText "<p class=\"date\">Generated: "
  , TB.fromText (T.pack $ show erGenerated)
  , TB.fromText "</p>\n"
  , TB.fromText "</header>\n"

  -- Executive Summary
  , TB.fromText "<section class=\"executive-summary\">\n"
  , TB.fromText "<h2>Executive Summary</h2>\n"
  , renderExecutiveHTML erExecutive
  , TB.fromText "</section>\n"

  -- Risk Indicators
  , TB.fromText "<section class=\"risk-indicators\">\n"
  , TB.fromText "<h3>Risk Overview</h3>\n"
  , renderRiskIndicators erExecutive
  , TB.fromText "</section>\n"

  -- Team Reports
  , if null erTeamReports then mempty else mconcat
      [ TB.fromText "<section class=\"team-reports\">\n"
      , TB.fromText "<h2>Team Performance</h2>\n"
      , mconcat $ map renderTeamHTML erTeamReports
      , TB.fromText "</section>\n"
      ]

  -- Top Issues
  , TB.fromText "<section class=\"top-issues\">\n"
  , TB.fromText "<h2>Top Issues</h2>\n"
  , renderTopIssuesHTML erTopIssues
  , TB.fromText "</section>\n"

  -- Action Items
  , TB.fromText "<section class=\"action-items\">\n"
  , TB.fromText "<h2>Recommended Actions</h2>\n"
  , renderActionItemsHTML erActionItems
  , TB.fromText "</section>\n"

  , TB.fromText "</div>\n</body>\n</html>"
  ]

-- | CSS styles for HTML report
cssStyles :: Text
cssStyles = T.unlines
  [ "body { font-family: -apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, sans-serif; margin: 0; padding: 20px; background: #f5f5f5; }"
  , ".container { max-width: 1200px; margin: 0 auto; background: white; padding: 40px; border-radius: 8px; box-shadow: 0 2px 4px rgba(0,0,0,0.1); }"
  , "header { border-bottom: 2px solid #e0e0e0; padding-bottom: 20px; margin-bottom: 30px; }"
  , "h1 { color: #333; margin: 0; }"
  , "h2 { color: #444; border-bottom: 1px solid #eee; padding-bottom: 10px; }"
  , "h3 { color: #555; }"
  , ".org { color: #666; font-size: 1.2em; }"
  , ".date { color: #999; }"
  , ".metric { display: inline-block; padding: 15px 25px; margin: 10px; background: #f8f9fa; border-radius: 8px; text-align: center; }"
  , ".metric-value { font-size: 2em; font-weight: bold; color: #333; }"
  , ".metric-label { font-size: 0.9em; color: #666; }"
  , ".metric.critical { border-left: 4px solid #dc3545; }"
  , ".metric.high { border-left: 4px solid #fd7e14; }"
  , ".metric.medium { border-left: 4px solid #ffc107; }"
  , ".metric.low { border-left: 4px solid #28a745; }"
  , ".risk-critical { background: #dc3545; color: white; }"
  , ".risk-high { background: #fd7e14; color: white; }"
  , ".risk-medium { background: #ffc107; color: black; }"
  , ".risk-low { background: #28a745; color: white; }"
  , ".risk-minimal { background: #20c997; color: white; }"
  , "table { width: 100%; border-collapse: collapse; margin: 20px 0; }"
  , "th, td { padding: 12px; text-align: left; border-bottom: 1px solid #ddd; }"
  , "th { background: #f8f9fa; font-weight: 600; }"
  , "tr:hover { background: #f8f9fa; }"
  , ".badge { display: inline-block; padding: 4px 8px; border-radius: 4px; font-size: 0.8em; }"
  , ".badge-error { background: #dc3545; color: white; }"
  , ".badge-warning { background: #fd7e14; color: white; }"
  , ".badge-suggestion { background: #ffc107; color: black; }"
  , ".badge-info { background: #17a2b8; color: white; }"
  , ".action-item { padding: 15px; margin: 10px 0; background: #f8f9fa; border-radius: 8px; border-left: 4px solid #007bff; }"
  , ".action-item h4 { margin: 0 0 10px 0; }"
  , ".action-item p { margin: 5px 0; color: #666; }"
  ]

-- | Render executive summary as HTML
renderExecutiveHTML :: ExecutiveSummary -> TB.Builder
renderExecutiveHTML ExecutiveSummary{..} = mconcat
  [ TB.fromText "<div class=\"metrics\">\n"
  , renderMetric "Files Analyzed" (T.pack $ show esTotalFiles) ""
  , renderMetric "Total Issues" (T.pack $ show esTotalIssues) ""
  , renderMetric "Critical" (T.pack $ show esCriticalCount) "critical"
  , renderMetric "High" (T.pack $ show esHighCount) "high"
  , renderMetric "Auto-Fixable" (T.pack $ show esFixableCount) "low"
  , TB.fromText "</div>\n"
  , TB.fromText "<div class=\"scores\">\n"
  , TB.fromText "<p><strong>Code Quality Score:</strong> "
  , TB.fromText (T.pack $ show (round esCodeQualityScore :: Int))
  , TB.fromText "%</p>\n"
  , TB.fromText "<p><strong>Security Score:</strong> "
  , TB.fromText (T.pack $ show (round esSecurityScore :: Int))
  , TB.fromText "%</p>\n"
  , TB.fromText "<p><strong>Technical Debt:</strong> "
  , TB.fromText (T.pack $ show esTechnicalDebt)
  , TB.fromText "</p>\n"
  , TB.fromText "</div>\n"
  ]

-- | Render single metric
renderMetric :: Text -> Text -> Text -> TB.Builder
renderMetric label value cls = mconcat
  [ TB.fromText "<div class=\"metric "
  , TB.fromText cls
  , TB.fromText "\">\n"
  , TB.fromText "<div class=\"metric-value\">"
  , TB.fromText value
  , TB.fromText "</div>\n"
  , TB.fromText "<div class=\"metric-label\">"
  , TB.fromText label
  , TB.fromText "</div>\n"
  , TB.fromText "</div>\n"
  ]

-- | Render risk indicators
renderRiskIndicators :: ExecutiveSummary -> TB.Builder
renderRiskIndicators ExecutiveSummary{..} =
  let riskClass = case esRiskLevel of
        RiskCritical -> "risk-critical"
        RiskHigh -> "risk-high"
        RiskMedium -> "risk-medium"
        RiskLow -> "risk-low"
        RiskMinimal -> "risk-minimal"
  in mconcat
    [ TB.fromText "<div class=\"risk-badge "
    , TB.fromText riskClass
    , TB.fromText "\" style=\"padding: 10px 20px; display: inline-block; border-radius: 4px; font-weight: bold;\">\n"
    , TB.fromText "Risk Level: "
    , TB.fromText (T.pack $ show esRiskLevel)
    , TB.fromText "</div>\n"
    ]

-- | Render team report as HTML
renderTeamHTML :: TeamReport -> TB.Builder
renderTeamHTML TeamReport{..} = mconcat
  [ TB.fromText "<div class=\"team-card\">\n"
  , TB.fromText "<h4>"
  , TB.fromText trTeamName
  , TB.fromText "</h4>\n"
  , TB.fromText "<p>Files: "
  , TB.fromText (T.pack $ show trFileCount)
  , TB.fromText " | Issues: "
  , TB.fromText (T.pack $ show trIssueCount)
  , TB.fromText " | Quality: "
  , TB.fromText (T.pack $ show (round trQualityScore :: Int))
  , TB.fromText "%</p>\n"
  , TB.fromText "</div>\n"
  ]

-- | Render top issues as HTML table
renderTopIssuesHTML :: [IssueAggregate] -> TB.Builder
renderTopIssuesHTML issues = mconcat
  [ TB.fromText "<table>\n"
  , TB.fromText "<tr><th>Rule</th><th>Category</th><th>Severity</th><th>Occurrences</th><th>Files</th><th>Fixable</th></tr>\n"
  , mconcat $ map renderIssueRow issues
  , TB.fromText "</table>\n"
  ]

-- | Render issue row
renderIssueRow :: IssueAggregate -> TB.Builder
renderIssueRow IssueAggregate{..} = mconcat
  [ TB.fromText "<tr>\n"
  , TB.fromText "<td>"
  , TB.fromText iaRuleName
  , TB.fromText "</td>\n"
  , TB.fromText "<td>"
  , TB.fromText iaCategory
  , TB.fromText "</td>\n"
  , TB.fromText "<td><span class=\"badge badge-"
  , TB.fromText (T.toLower $ T.pack $ show iaSeverity)
  , TB.fromText "\">"
  , TB.fromText (T.pack $ show iaSeverity)
  , TB.fromText "</span></td>\n"
  , TB.fromText "<td>"
  , TB.fromText (T.pack $ show iaOccurrences)
  , TB.fromText "</td>\n"
  , TB.fromText "<td>"
  , TB.fromText (T.pack $ show iaAffectedFiles)
  , TB.fromText "</td>\n"
  , TB.fromText "<td>"
  , TB.fromText (if iaFixable then "Yes" else "No")
  , TB.fromText "</td>\n"
  , TB.fromText "</tr>\n"
  ]

-- | Render action items as HTML
renderActionItemsHTML :: [ActionItem] -> TB.Builder
renderActionItemsHTML items = mconcat $ map renderActionItem items

-- | Render single action item
renderActionItem :: ActionItem -> TB.Builder
renderActionItem ActionItem{..} = mconcat
  [ TB.fromText "<div class=\"action-item\">\n"
  , TB.fromText "<h4>[Priority "
  , TB.fromText (T.pack $ show aiPriority)
  , TB.fromText "] "
  , TB.fromText aiTitle
  , TB.fromText "</h4>\n"
  , TB.fromText "<p>"
  , TB.fromText aiDescription
  , TB.fromText "</p>\n"
  , TB.fromText "<p><strong>Impact:</strong> "
  , TB.fromText aiImpact
  , TB.fromText " | <strong>Effort:</strong> "
  , TB.fromText aiEffort
  , TB.fromText "</p>\n"
  , TB.fromText "</div>\n"
  ]

-- | Render as CSV
renderEnterpriseCSV :: EnterpriseReport -> Text
renderEnterpriseCSV EnterpriseReport{..} = T.unlines $
  [ "Rule ID,Rule Name,Category,Severity,Occurrences,Files Affected,Fixable" ] ++
  map issueToCSV erTopIssues

-- | Convert issue to CSV row
issueToCSV :: IssueAggregate -> Text
issueToCSV IssueAggregate{..} = T.intercalate "," $
  [ quote iaRuleId
  , quote iaRuleName
  , quote iaCategory
  , T.pack $ show iaSeverity
  , T.pack $ show iaOccurrences
  , T.pack $ show iaAffectedFiles
  , if iaFixable then "Yes" else "No"
  ]
  where
    quote t = "\"" <> T.replace "\"" "\"\"" t <> "\""

-- | Render as Markdown
renderEnterpriseMarkdown :: EnterpriseReport -> Text
renderEnterpriseMarkdown EnterpriseReport{..} = T.unlines $
  [ "# Code Quality Report"
  , ""
  , "**Organization:** " <> erOrganization
  , ""
  , "**Generated:** " <> T.pack (show erGenerated)
  , ""
  , "## Executive Summary"
  , ""
  , "| Metric | Value |"
  , "|--------|-------|"
  , "| Total Files | " <> T.pack (show (esTotalFiles erExecutive)) <> " |"
  , "| Total Issues | " <> T.pack (show (esTotalIssues erExecutive)) <> " |"
  , "| Critical Issues | " <> T.pack (show (esCriticalCount erExecutive)) <> " |"
  , "| Code Quality | " <> T.pack (show (round (esCodeQualityScore erExecutive) :: Int)) <> "% |"
  , ""
  , "## Top Issues"
  , ""
  , "| Rule | Category | Severity | Count |"
  , "|------|----------|----------|-------|"
  ] ++ map issueToMD erTopIssues ++
  [ ""
  , "## Recommended Actions"
  , ""
  ] ++ zipWith actionToMD [1..] erActionItems

-- | Convert issue to Markdown row
issueToMD :: IssueAggregate -> Text
issueToMD IssueAggregate{..} = T.concat
  [ "| ", iaRuleName
  , " | ", iaCategory
  , " | ", T.pack $ show iaSeverity
  , " | ", T.pack $ show iaOccurrences
  , " |"
  ]

-- | Convert action to Markdown
actionToMD :: Int -> ActionItem -> Text
actionToMD n ActionItem{..} = T.unlines
  [ T.pack (show n) <> ". **" <> aiTitle <> "**"
  , "   - " <> aiDescription
  , "   - Impact: " <> aiImpact <> " | Effort: " <> aiEffort
  ]

--------------------------------------------------------------------------------
-- Historical Data Functions
--------------------------------------------------------------------------------

-- | Load historical data from file
loadHistoricalData :: FilePath -> IO HistoricalData
loadHistoricalData path = do
  exists <- doesFileExist path
  if exists
    then do
      contents <- BL.readFile path
      case decode contents of
        Just hd -> pure hd
        Nothing -> pure $ HistoricalData []
    else pure $ HistoricalData []
  where
    decode bs = case TE.decodeUtf8' (BL.toStrict bs) of
      Right _ -> Nothing  -- Would need proper JSON parsing
      Left _ -> Nothing

-- | Save historical snapshot
saveHistoricalSnapshot :: FilePath -> AnalysisResult -> IO ()
saveHistoricalSnapshot path result = do
  createDirectoryIfMissing True (takeDirectory path)
  existing <- loadHistoricalData path
  now <- getCurrentTime
  let snapshot = resultToSnapshot now result
  let updated = HistoricalData $ hdSnapshots existing ++ [snapshot]
  BL.writeFile path $ encode updated

-- | Compute trends from historical data
computeTrends :: HistoricalData -> TrendAnalysis
computeTrends historical =
  let snapshots = hdSnapshots historical
      dataPoints = map snapshotToDataPoint snapshots
  in TrendAnalysis
    { taDataPoints = dataPoints
    , taOverallTrend = computeOverallTrend snapshots
    , taIssuesTrend = computeIssuesTrend snapshots
    , taQualityTrend = computeQualityTrend snapshots
    , taDebtTrend = computeDebtTrend snapshots
    , taProjections = Nothing
    }

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Get all diagnostics from result
getAllDiagnostics :: AnalysisResult -> [Diagnostic]
getAllDiagnostics result =
  concatMap fileResultDiagnostics $ Map.elems (resultFiles result)

-- | Count diagnostics by severity
countSeverity :: Severity -> [Diagnostic] -> Int
countSeverity sev = length . filter ((== sev) . diagSeverity)

-- | Compute code quality score (0-100)
computeQualityScore :: Int -> Int -> Int -> Int -> Double
computeQualityScore totalFiles totalIssues criticalCount highCount
  | totalFiles == 0 = 100.0
  | otherwise =
      let baseScore = 100.0
          issueDeduction = min 50.0 (fromIntegral totalIssues * 0.5)
          criticalDeduction = fromIntegral criticalCount * 10.0
          highDeduction = fromIntegral highCount * 3.0
      in max 0.0 $ baseScore - issueDeduction - criticalDeduction - highDeduction

-- | Compute technical debt score
computeTechnicalDebt :: [Diagnostic] -> Int
computeTechnicalDebt diags =
  let weights = map diagToDebt diags
  in sum weights
  where
    diagToDebt d = case diagSeverity d of
      Error -> 10
      Warning -> 5
      Suggestion -> 2
      Info -> 1

-- | Compute security score (0-100)
computeSecurityScore :: [Diagnostic] -> Double
computeSecurityScore diags =
  let securityIssues = filter isSecurityRelated diags
      count = length securityIssues
  in max 0.0 $ 100.0 - fromIntegral count * 10.0
  where
    isSecurityRelated d = diagKind d == SecurityIssue

-- | Compute risk level based on issue counts
computeRiskLevel :: Int -> Int -> Int -> RiskLevel
computeRiskLevel criticalCount highCount totalIssues
  | criticalCount > 0 = RiskCritical
  | highCount > 10 = RiskHigh
  | highCount > 5 || totalIssues > 50 = RiskMedium
  | totalIssues > 10 = RiskLow
  | otherwise = RiskMinimal

-- | Compute category breakdown
computeCategoryBreakdown :: [Diagnostic] -> [CategoryBreakdown]
computeCategoryBreakdown diags =
  let total = length diags
      byKind = foldl' countKind Map.empty diags
      breakdown = Map.toList byKind
  in map (toBreakdown total) breakdown
  where
    countKind acc d = Map.insertWith (+) (kindToCategory $ diagKind d) 1 acc
    toBreakdown total (cat, count) = CategoryBreakdown
      { cbCategory = cat
      , cbCount = count
      , cbPercentage = if total == 0 then 0 else (fromIntegral count / fromIntegral total) * 100
      }

    kindToCategory = \case
      NamingConvention -> "Naming"
      UnusedCode -> "Unused Code"
      UnusedImport -> "Unused Imports"
      RedundantCode -> "Redundant Code"
      CodePattern -> "Code Patterns"
      TypeSignature -> "Type Signatures"
      ImportStyle -> "Import Style"
      TemplateHaskellRef -> "Template Haskell"
      SecurityIssue -> "Security"
      PerformanceIssue -> "Performance"
      ArchitecturalIssue -> "Architecture"
      SpaceLeak -> "Space Leaks"
      PartialFunction -> "Partial Functions"
      ComplexityIssue -> "Complexity"
      Custom name -> name

-- | Compute severity breakdown
computeSeverityBreakdown :: [Diagnostic] -> [SeverityBreakdown]
computeSeverityBreakdown diags =
  let total = length diags
      bySeverity = foldl' countSev Map.empty diags
  in map (toBreakdown total) $ Map.toList bySeverity
  where
    countSev acc d = Map.insertWith (+) (diagSeverity d) 1 acc
    toBreakdown total (sev, count) = SeverityBreakdown
      { sbSeverity = sev
      , sbCount = count
      , sbPercentage = if total == 0 then 0 else (fromIntegral count / fromIntegral total) * 100
      }

-- | Filter files by path prefixes
filterFilesByPrefixes :: [Text] -> Map FilePath FileResult -> Map FilePath FileResult
filterFilesByPrefixes prefixes files
  | null prefixes = files
  | otherwise = Map.filterWithKey matchesPrefix files
  where
    matchesPrefix path _ = any (\p -> T.isPrefixOf p (T.pack path)) prefixes

-- | Compute rule frequencies
computeRuleFrequencies :: Int -> [Diagnostic] -> [RuleFrequency]
computeRuleFrequencies n diags =
  let byRule = foldl' countRule Map.empty diags
      sorted = sortOn (\(_, (cnt, _)) -> negate cnt) $ Map.toList byRule
  in take n $ map toFrequency sorted
  where
    countRule acc d =
      let ruleId = maybe "unknown" id (diagCode d)
          entry = (1, diagSeverity d)
      in Map.insertWith (\(c1, s) (c2, _) -> (c1 + c2, s)) ruleId entry acc

    toFrequency (ruleId, (count, sev)) = RuleFrequency
      { rfRuleId = ruleId
      , rfRuleName = ruleIdToName ruleId
      , rfCount = count
      , rfSeverity = sev
      }

    ruleIdToName rid =
      let parts = T.splitOn "/" rid
      in if length parts > 1
         then T.toTitle $ T.replace "-" " " $ last parts
         else T.toTitle $ T.replace "-" " " rid

-- | Compute top issues
computeTopIssues :: Int -> AnalysisResult -> [IssueAggregate]
computeTopIssues n result =
  let allDiags = getAllDiagnostics result
      byRule = foldl' groupByRule Map.empty allDiags
      sorted = sortOn (\(_, ds) -> negate $ length ds) $ Map.toList byRule
  in take n $ map toAggregate sorted
  where
    groupByRule acc d =
      let ruleId = maybe "unknown" id (diagCode d)
      in Map.insertWith (++) ruleId [d] acc

    toAggregate (ruleId, diags) = case diags of
      (first:rest) ->
        let files = length $ nub $ map (srcSpanFile . diagSpan) (first:rest)
        in IssueAggregate
          { iaRuleId = ruleId
          , iaRuleName = ruleIdToName ruleId
          , iaCategory = kindToCategory $ diagKind first
          , iaSeverity = diagSeverity first
          , iaOccurrences = length (first:rest)
          , iaAffectedFiles = files
          , iaFixable = any (not . null . diagFixes) (first:rest)
          , iaEstimatedFix = Nothing
          }
      [] -> IssueAggregate
          { iaRuleId = ruleId
          , iaRuleName = ruleIdToName ruleId
          , iaCategory = "Unknown"
          , iaSeverity = Info
          , iaOccurrences = 0
          , iaAffectedFiles = 0
          , iaFixable = False
          , iaEstimatedFix = Nothing
          }

    ruleIdToName rid =
      let parts = T.splitOn "/" rid
      in if length parts > 1
         then T.toTitle $ T.replace "-" " " $ last parts
         else T.toTitle $ T.replace "-" " " rid

    kindToCategory = \case
      SecurityIssue -> "Security"
      PerformanceIssue -> "Performance"
      SpaceLeak -> "Memory"
      PartialFunction -> "Safety"
      _ -> "Quality"

-- | Generate key findings
generateKeyFindings :: Int -> Int -> Double -> [Text]
generateKeyFindings criticalCount highCount securityScore =
  let findings = []
      findings' = if criticalCount > 0
                  then findings ++ [T.pack (show criticalCount) <> " critical issues require immediate attention"]
                  else findings
      findings'' = if highCount > 5
                   then findings' ++ ["High number of warnings (" <> T.pack (show highCount) <> ") affecting code quality"]
                   else findings'
      findings''' = if securityScore < 80
                    then findings'' ++ ["Security score below target - security review recommended"]
                    else findings''
  in if null findings''' then ["Code quality is within acceptable limits"] else findings'''

-- | Generate recommendations
generateRecommendations :: [CategoryBreakdown] -> Int -> Int -> [Text]
generateRecommendations categories criticalCount fixableCount =
  let recs = []
      recs' = if criticalCount > 0
              then recs ++ ["Address all critical issues before next release"]
              else recs
      recs'' = if fixableCount > 10
               then recs' ++ ["Run auto-fix to resolve " <> T.pack (show fixableCount) <> " issues automatically"]
               else recs'
      topCategory = case sortOn (negate . cbCount) categories of
        (c:_) -> Just c
        [] -> Nothing
      recs''' = case topCategory of
                  Just cat -> recs'' ++ ["Focus on " <> cbCategory cat <> " issues (highest count)"]
                  Nothing -> recs''
  in if null recs''' then ["Continue maintaining current code quality standards"] else recs'''

-- | Generate action items
generateActionItems :: EnterpriseConfig -> ExecutiveSummary -> [IssueAggregate] -> [ActionItem]
generateActionItems _ ExecutiveSummary{..} _topIssues =
  let items = []
      items' = if esCriticalCount > 0
               then items ++ [ActionItem
                 { aiPriority = 1
                 , aiTitle = "Address Critical Issues"
                 , aiDescription = "Fix " <> T.pack (show esCriticalCount) <> " critical issues immediately"
                 , aiImpact = "High - prevents release blockers"
                 , aiEffort = "Variable"
                 , aiOwner = Nothing
                 }]
               else items
      items'' = if esFixableCount > 0
                then items' ++ [ActionItem
                  { aiPriority = 2
                  , aiTitle = "Run Auto-Fix"
                  , aiDescription = "Automatically fix " <> T.pack (show esFixableCount) <> " issues"
                  , aiImpact = "Medium - improves quality score"
                  , aiEffort = "Low"
                  , aiOwner = Nothing
                  }]
                else items'
      items''' = if esSecurityScore < 90
                 then items'' ++ [ActionItem
                   { aiPriority = 3
                   , aiTitle = "Security Review"
                   , aiDescription = "Conduct security review to improve score from " <> T.pack (show (round esSecurityScore :: Int)) <> "%"
                   , aiImpact = "High - reduces security risk"
                   , aiEffort = "Medium"
                   , aiOwner = Nothing
                   }]
                 else items''
  in items'''

-- | Get compliance rules for a standard
getComplianceRules :: ComplianceStandard -> [ComplianceRule]
getComplianceRules = \case
  ComplianceOWASP ->
    [ ComplianceRule "A01" "Broken Access Control" "Verify access control enforcement" CompliancePass 0
    , ComplianceRule "A02" "Cryptographic Failures" "Check encryption usage" CompliancePass 0
    , ComplianceRule "A03" "Injection" "Check for injection vulnerabilities" CompliancePass 0
    ]
  ComplianceCWE ->
    [ ComplianceRule "CWE-79" "XSS" "Cross-site scripting prevention" CompliancePass 0
    , ComplianceRule "CWE-89" "SQL Injection" "SQL injection prevention" CompliancePass 0
    , ComplianceRule "CWE-22" "Path Traversal" "Path traversal prevention" CompliancePass 0
    ]
  _ -> []

-- | Find compliance violations
findComplianceViolations :: ComplianceStandard -> [Diagnostic] -> [ComplianceViolation]
findComplianceViolations _standard diags =
  mapMaybe toViolation $ filter isSecurityRelated diags
  where
    isSecurityRelated d = diagKind d == SecurityIssue

    toViolation d = Just $ ComplianceViolation
      { cvRuleId = maybe "unknown" id (diagCode d)
      , cvFilePath = T.pack $ srcSpanFile (diagSpan d)
      , cvLine = srcSpanStartLineRaw (diagSpan d)
      , cvMessage = diagMessage d
      , cvSeverity = diagSeverity d
      }

-- | Convert result to historical snapshot
resultToSnapshot :: UTCTime -> AnalysisResult -> HistoricalSnapshot
resultToSnapshot timestamp result = HistoricalSnapshot
  { hsTimestamp = timestamp
  , hsTotalFiles = Map.size (resultFiles result)
  , hsTotalIssues = totalIssues
  , hsCriticalCount = countSeverity Error allDiags
  , hsHighCount = countSeverity Warning allDiags
  , hsMediumCount = countSeverity Suggestion allDiags
  , hsLowCount = countSeverity Info allDiags
  , hsQualityScore = computeQualityScore (Map.size $ resultFiles result) totalIssues
                       (countSeverity Error allDiags) (countSeverity Warning allDiags)
  , hsTechnicalDebt = computeTechnicalDebt allDiags
  , hsByCategory = Map.empty  -- Would need to compute
  , hsByRule = Map.empty      -- Would need to compute
  }
  where
    allDiags = getAllDiagnostics result
    totalIssues = length allDiags

-- | Convert snapshot to trend data point
snapshotToDataPoint :: HistoricalSnapshot -> TrendDataPoint
snapshotToDataPoint HistoricalSnapshot{..} = TrendDataPoint
  { tdpTimestamp = hsTimestamp
  , tdpTotalIssues = hsTotalIssues
  , tdpCriticalIssues = hsCriticalCount
  , tdpQualityScore = hsQualityScore
  , tdpTechnicalDebt = hsTechnicalDebt
  , tdpFilesAnalyzed = hsTotalFiles
  }

-- | Compute overall trend from snapshots
computeOverallTrend :: [HistoricalSnapshot] -> TrendDirection
computeOverallTrend snapshots
  | length snapshots < 2 = TrendStable
  | otherwise =
      let recent = take 3 $ reverse snapshots
          older = take 3 $ drop 3 $ reverse snapshots
          recentAvg = if null recent then 0 else sum (map hsTotalIssues recent) `div` length recent
          olderAvg = if null older then recentAvg else sum (map hsTotalIssues older) `div` length older
      in if recentAvg < olderAvg - 5 then TrendImproving
         else if recentAvg > olderAvg + 5 then TrendDeclining
         else TrendStable

-- | Compute issues trend
computeIssuesTrend :: [HistoricalSnapshot] -> TrendDirection
computeIssuesTrend = computeOverallTrend

-- | Compute quality trend
computeQualityTrend :: [HistoricalSnapshot] -> TrendDirection
computeQualityTrend snapshots
  | length snapshots < 2 = TrendStable
  | otherwise =
      let recent = take 3 $ reverse snapshots
          older = take 3 $ drop 3 $ reverse snapshots
          recentAvg = if null recent then 0 else sum (map hsQualityScore recent) / fromIntegral (length recent)
          olderAvg = if null older then recentAvg else sum (map hsQualityScore older) / fromIntegral (length older)
      in if recentAvg > olderAvg + 2 then TrendImproving
         else if recentAvg < olderAvg - 2 then TrendDeclining
         else TrendStable

-- | Compute debt trend
computeDebtTrend :: [HistoricalSnapshot] -> TrendDirection
computeDebtTrend snapshots
  | length snapshots < 2 = TrendStable
  | otherwise =
      let recent = take 3 $ reverse snapshots
          older = take 3 $ drop 3 $ reverse snapshots
          recentAvg = if null recent then 0 else sum (map hsTechnicalDebt recent) `div` length recent
          olderAvg = if null older then recentAvg else sum (map hsTechnicalDebt older) `div` length older
      in if recentAvg < olderAvg - 5 then TrendImproving
         else if recentAvg > olderAvg + 5 then TrendDeclining
         else TrendStable
