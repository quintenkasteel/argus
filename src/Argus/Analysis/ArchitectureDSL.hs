{-# LANGUAGE StrictData #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}

-- |
-- Module      : Argus.Analysis.ArchitectureDSL
-- Description : DSL for defining architectural layer contracts
-- Copyright   : (c) 2024
-- License     : MIT
--
-- This module provides a domain-specific language for defining architectural
-- constraints and layer boundaries in Haskell projects.
--
-- == Basic Usage
--
-- @
-- myArchitecture :: ArchitectureSpec
-- myArchitecture = architecture "MyApp" $ do
--   layer "Core" $ do
--     modules ["MyApp.Types", "MyApp.Core.*"]
--     cannotImport ["Service", "API", "App"]
--
--   layer "Data" $ do
--     modules ["MyApp.Data.*", "MyApp.Model.*"]
--     canImport ["Core"]
--
--   layer "Service" $ do
--     modules ["MyApp.Service.*"]
--     canImport ["Core", "Data"]
--     mustNotImport ["API"]
--
--   layer "API" $ do
--     modules ["MyApp.API.*", "MyApp.Handler.*"]
--     canImport ["Core", "Data", "Service"]
--
--   layer "App" $ do
--     modules ["Main", "MyApp.CLI"]
--     canImport ["*"]  -- App can import anything
--
--   -- Global constraints
--   forbidCircularDeps
--   maxCouplingPerModule 10
--   requireQualifiedImports ["Data.Map", "Data.Set"]
-- @
--
-- == Hexagonal Architecture
--
-- @
-- hexagonalArchitecture :: ArchitectureSpec
-- hexagonalArchitecture = architecture "HexApp" $ do
--   -- Domain (innermost)
--   layer "Domain" $ do
--     modules ["*.Domain.*", "*.Entity.*"]
--     cannotImport ["Adapter", "Port", "Infrastructure"]
--
--   -- Application services
--   layer "Application" $ do
--     modules ["*.Application.*", "*.UseCase.*"]
--     canImport ["Domain"]
--
--   -- Ports (interfaces)
--   layer "Port" $ do
--     modules ["*.Port.*", "*.Interface.*"]
--     canImport ["Domain", "Application"]
--
--   -- Adapters (implementations)
--   layer "Adapter" $ do
--     modules ["*.Adapter.*", "*.Repository.*", "*.Controller.*"]
--     canImport ["Domain", "Application", "Port"]
--
--   -- Infrastructure
--   layer "Infrastructure" $ do
--     modules ["*.Infrastructure.*", "*.Config.*"]
--     canImport ["*"]
-- @
module Argus.Analysis.ArchitectureDSL
  ( -- * Architecture Specification
    ArchitectureSpec (..)
  , architecture
  , LayerSpec (..)
  , layer

    -- * Layer Constraints
  , LayerBuilder
  , modules
  , canImport
  , cannotImport
  , mustNotImport
  , allowSelfImport
  , forbidSelfImport

    -- * Global Constraints
  , ArchBuilder
  , forbidCircularDeps
  , allowCircularDeps
  , maxCouplingPerModule
  , maxImportDepth
  , requireQualifiedImports
  , forbidOrphanInstances
  , allowOrphanInstances

    -- * Predefined Architectures
  , layeredArchitecture
  , hexagonalArchitecture
  , cleanArchitecture
  , onionArchitecture

    -- * Conversion
  , specToConfig
  , validateAgainstSpec
  , ArchitectureViolation (..)

    -- * Parsing from TOML
  , parseArchitectureSpec
  ) where

import Control.Monad.State.Strict
import Data.Aeson (ToJSON, FromJSON)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text (Text)
import Data.Text qualified as T
import GHC.Generics (Generic)

import Argus.Analysis.Architecture (ArchitectureConfig(..), LayerConfig(..), ArchitectureReport(..), LayerViolation(..), CircularDep(..), CouplingMetrics(..))
import Argus.Types (SrcSpan)

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------

-- | Complete architecture specification
data ArchitectureSpec = ArchitectureSpec
  { asName                 :: Text
  , asLayers               :: [LayerSpec]
  , asForbidCircular       :: Bool
  , asMaxCoupling          :: Maybe Int
  , asMaxDepth             :: Maybe Int
  , asQualifiedImports     :: Set Text
  , asForbidOrphans        :: Bool
  , asCustomConstraints    :: [CustomConstraint]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Layer specification
data LayerSpec = LayerSpec
  { lsName            :: Text
  , lsModulePatterns  :: [Text]
  , lsCanImport       :: Set Text      -- Allowed import layers
  , lsCannotImport    :: Set Text      -- Forbidden import layers
  , lsAllowSelfImport :: Bool
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Custom architectural constraint
data CustomConstraint
  = ModuleMustNotImport Text Text      -- Module pattern, forbidden module pattern
  | ModuleMustImport Text Text         -- Module pattern, required module pattern
  | MaxModuleSize Int                  -- Max lines per module
  | MaxFunctionsPerModule Int          -- Max functions per module
  | RequireExplicitExports             -- All modules must have explicit exports
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Violation of architectural constraint
data ArchitectureViolation = ArchitectureViolation
  { avType        :: ViolationType
  , avModule      :: Text
  , avLayer       :: Maybe Text
  , avMessage     :: Text
  , avSpan        :: Maybe SrcSpan
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ViolationType
  = LayerViolationType
  | CircularDepType
  | CouplingThresholdType
  | DepthThresholdType
  | OrphanInstanceType
  | QualifiedImportType
  | CustomConstraintType
  deriving stock (Eq, Show, Ord, Enum, Bounded, Generic)
  deriving anyclass (ToJSON, FromJSON)

--------------------------------------------------------------------------------
-- Builder Monads
--------------------------------------------------------------------------------

-- | Architecture builder state
data ArchState = ArchState
  { archName               :: Text
  , archLayers             :: [LayerSpec]
  , archForbidCircular     :: Bool
  , archMaxCoupling        :: Maybe Int
  , archMaxDepth           :: Maybe Int
  , archQualifiedImports   :: Set Text
  , archForbidOrphans      :: Bool
  , archCustomConstraints  :: [CustomConstraint]
  }

-- | Layer builder state
data LayerState = LayerState
  { layerName         :: Text
  , layerPatterns     :: [Text]
  , layerCanImport    :: Set Text
  , layerCannotImport :: Set Text
  , layerAllowSelf    :: Bool
  }

type ArchBuilder = State ArchState ()
type LayerBuilder = State LayerState ()

--------------------------------------------------------------------------------
-- DSL Functions
--------------------------------------------------------------------------------

-- | Define an architecture specification
architecture :: Text -> ArchBuilder -> ArchitectureSpec
architecture name builder =
  let initialState = ArchState
        { archName = name
        , archLayers = []
        , archForbidCircular = True
        , archMaxCoupling = Nothing
        , archMaxDepth = Nothing
        , archQualifiedImports = Set.empty
        , archForbidOrphans = False
        , archCustomConstraints = []
        }
      finalState = execState builder initialState
  in ArchitectureSpec
       { asName = archName finalState
       , asLayers = reverse $ archLayers finalState
       , asForbidCircular = archForbidCircular finalState
       , asMaxCoupling = archMaxCoupling finalState
       , asMaxDepth = archMaxDepth finalState
       , asQualifiedImports = archQualifiedImports finalState
       , asForbidOrphans = archForbidOrphans finalState
       , asCustomConstraints = archCustomConstraints finalState
       }

-- | Define a layer within an architecture
layer :: Text -> LayerBuilder -> ArchBuilder
layer name builder = do
  let initialState = LayerState
        { layerName = name
        , layerPatterns = []
        , layerCanImport = Set.empty
        , layerCannotImport = Set.empty
        , layerAllowSelf = True
        }
      finalState = execState builder initialState
      spec = LayerSpec
        { lsName = layerName finalState
        , lsModulePatterns = reverse $ layerPatterns finalState
        , lsCanImport = layerCanImport finalState
        , lsCannotImport = layerCannotImport finalState
        , lsAllowSelfImport = layerAllowSelf finalState
        }
  modify $ \s -> s { archLayers = spec : archLayers s }

-- | Specify module patterns for a layer
modules :: [Text] -> LayerBuilder
modules pats = modify $ \s -> s { layerPatterns = reverse pats ++ layerPatterns s }

-- | Specify layers this layer can import from
canImport :: [Text] -> LayerBuilder
canImport layers = modify $ \s -> s { layerCanImport = Set.fromList layers `Set.union` layerCanImport s }

-- | Specify layers this layer cannot import from
cannotImport :: [Text] -> LayerBuilder
cannotImport layers = modify $ \s -> s { layerCannotImport = Set.fromList layers `Set.union` layerCannotImport s }

-- | Alias for cannotImport
mustNotImport :: [Text] -> LayerBuilder
mustNotImport = cannotImport

-- | Allow modules in this layer to import from each other
allowSelfImport :: LayerBuilder
allowSelfImport = modify $ \s -> s { layerAllowSelf = True }

-- | Forbid modules in this layer from importing each other
forbidSelfImport :: LayerBuilder
forbidSelfImport = modify $ \s -> s { layerAllowSelf = False }

-- | Forbid circular dependencies
forbidCircularDeps :: ArchBuilder
forbidCircularDeps = modify $ \s -> s { archForbidCircular = True }

-- | Allow circular dependencies (not recommended)
allowCircularDeps :: ArchBuilder
allowCircularDeps = modify $ \s -> s { archForbidCircular = False }

-- | Set maximum coupling per module
maxCouplingPerModule :: Int -> ArchBuilder
maxCouplingPerModule n = modify $ \s -> s { archMaxCoupling = Just n }

-- | Set maximum import depth
maxImportDepth :: Int -> ArchBuilder
maxImportDepth n = modify $ \s -> s { archMaxDepth = Just n }

-- | Require qualified imports for certain modules
requireQualifiedImports :: [Text] -> ArchBuilder
requireQualifiedImports mods = modify $ \s ->
  s { archQualifiedImports = Set.fromList mods `Set.union` archQualifiedImports s }

-- | Forbid orphan instances
forbidOrphanInstances :: ArchBuilder
forbidOrphanInstances = modify $ \s -> s { archForbidOrphans = True }

-- | Allow orphan instances
allowOrphanInstances :: ArchBuilder
allowOrphanInstances = modify $ \s -> s { archForbidOrphans = False }

--------------------------------------------------------------------------------
-- Predefined Architectures
--------------------------------------------------------------------------------

-- | Standard layered architecture (UI -> Service -> Data -> Core)
layeredArchitecture :: Text -> ArchitectureSpec
layeredArchitecture prefix = architecture (prefix <> " Layered") $ do
  layer "Core" $ do
    modules [prefix <> ".Types", prefix <> ".Core.*"]
    cannotImport ["Data", "Service", "UI"]

  layer "Data" $ do
    modules [prefix <> ".Data.*", prefix <> ".Model.*"]
    canImport ["Core"]

  layer "Service" $ do
    modules [prefix <> ".Service.*", prefix <> ".Logic.*"]
    canImport ["Core", "Data"]

  layer "UI" $ do
    modules [prefix <> ".UI.*", prefix <> ".Handler.*", prefix <> ".API.*"]
    canImport ["Core", "Data", "Service"]

  forbidCircularDeps
  forbidOrphanInstances

-- | Hexagonal (Ports and Adapters) architecture
hexagonalArchitecture :: Text -> ArchitectureSpec
hexagonalArchitecture prefix = architecture (prefix <> " Hexagonal") $ do
  layer "Domain" $ do
    modules [prefix <> ".Domain.*", prefix <> ".Entity.*"]
    cannotImport ["Application", "Port", "Adapter", "Infrastructure"]

  layer "Application" $ do
    modules [prefix <> ".Application.*", prefix <> ".UseCase.*"]
    canImport ["Domain"]

  layer "Port" $ do
    modules [prefix <> ".Port.*", prefix <> ".Interface.*"]
    canImport ["Domain", "Application"]

  layer "Adapter" $ do
    modules [prefix <> ".Adapter.*", prefix <> ".Repository.*", prefix <> ".Controller.*"]
    canImport ["Domain", "Application", "Port"]

  layer "Infrastructure" $ do
    modules [prefix <> ".Infrastructure.*", prefix <> ".Config.*"]
    canImport ["Domain", "Application", "Port", "Adapter"]

  forbidCircularDeps

-- | Clean Architecture (Uncle Bob's)
cleanArchitecture :: Text -> ArchitectureSpec
cleanArchitecture prefix = architecture (prefix <> " Clean") $ do
  layer "Enterprise" $ do
    modules [prefix <> ".Enterprise.*", prefix <> ".Entity.*"]
    cannotImport ["Application", "Interface", "Framework"]

  layer "Application" $ do
    modules [prefix <> ".Application.*", prefix <> ".UseCase.*"]
    canImport ["Enterprise"]

  layer "Interface" $ do
    modules [prefix <> ".Interface.*", prefix <> ".Controller.*", prefix <> ".Presenter.*"]
    canImport ["Enterprise", "Application"]

  layer "Framework" $ do
    modules [prefix <> ".Framework.*", prefix <> ".DB.*", prefix <> ".Web.*"]
    canImport ["Enterprise", "Application", "Interface"]

  forbidCircularDeps
  forbidOrphanInstances

-- | Onion Architecture
onionArchitecture :: Text -> ArchitectureSpec
onionArchitecture prefix = architecture (prefix <> " Onion") $ do
  layer "Domain" $ do
    modules [prefix <> ".Domain.*"]
    cannotImport ["DomainServices", "ApplicationServices", "Infrastructure", "UI"]

  layer "DomainServices" $ do
    modules [prefix <> ".DomainServices.*"]
    canImport ["Domain"]

  layer "ApplicationServices" $ do
    modules [prefix <> ".Application.*", prefix <> ".Services.*"]
    canImport ["Domain", "DomainServices"]

  layer "Infrastructure" $ do
    modules [prefix <> ".Infrastructure.*", prefix <> ".Persistence.*"]
    canImport ["Domain", "DomainServices", "ApplicationServices"]

  layer "UI" $ do
    modules [prefix <> ".UI.*", prefix <> ".API.*", prefix <> ".Web.*"]
    canImport ["Domain", "DomainServices", "ApplicationServices", "Infrastructure"]

  forbidCircularDeps

--------------------------------------------------------------------------------
-- Conversion
--------------------------------------------------------------------------------

-- | Convert specification to ArchitectureConfig
specToConfig :: ArchitectureSpec -> ArchitectureConfig
specToConfig spec = ArchitectureConfig
  { acEnabled = True
  , acLayers = map convertLayer (asLayers spec)
  , acMaxCycleLength = if asForbidCircular spec then 0 else 10
  , acInstabilityThreshold = 0.8
  , acCouplingThreshold = fromMaybe 15 (asMaxCoupling spec)
  , acCheckOrphans = asForbidOrphans spec
  , acCheckQualified = not (Set.null (asQualifiedImports spec))
  }
  where
    convertLayer ls = LayerConfig
      { lcName = lsName ls
      , lcPatterns = lsModulePatterns ls
      , lcCanImport = Set.toList (lsCanImport ls)
      }

-- | Validate an architecture report against a specification
validateAgainstSpec :: ArchitectureSpec -> ArchitectureReport -> [ArchitectureViolation]
validateAgainstSpec spec report = concat
  [ map convertLayerViolation (arLayerViolations report)
  , checkCircularDeps spec report
  , checkCoupling spec report
  ]
  where
    convertLayerViolation lv = ArchitectureViolation
      { avType = LayerViolationType
      , avModule = lvFrom lv
      , avLayer = Just (lvFromLayer lv)
      , avMessage = "Layer '" <> lvFromLayer lv <> "' should not import from '" <> lvToLayer lv <> "'"
      , avSpan = Just (lvSpan lv)
      }

    checkCircularDeps s r
      | asForbidCircular s && not (null (arCircularDeps r)) =
          [ ArchitectureViolation
              { avType = CircularDepType
              , avModule = T.intercalate " -> " (cdModules cd)
              , avLayer = Nothing
              , avMessage = "Circular dependency detected"
              , avSpan = Nothing
              }
          | cd <- arCircularDeps r
          ]
      | otherwise = []

    checkCoupling s r = case asMaxCoupling s of
      Nothing -> []
      Just maxC ->
        [ ArchitectureViolation
            { avType = CouplingThresholdType
            , avModule = cmModule cm
            , avLayer = Nothing
            , avMessage = "Coupling exceeds threshold: " <> T.pack (show (cmEfferentCoupling cm)) <> " > " <> T.pack (show maxC)
            , avSpan = Nothing
            }
        | cm <- arCouplingMetrics r
        , cmEfferentCoupling cm > maxC
        ]

--------------------------------------------------------------------------------
-- Parsing
--------------------------------------------------------------------------------

-- | Parse architecture specification from TOML text
parseArchitectureSpec :: Text -> Either Text ArchitectureSpec
parseArchitectureSpec _tomlText =
  -- This would use a TOML parser in practice
  Right $ architecture "Parsed" $ do
    layer "Default" $ modules ["*"]
