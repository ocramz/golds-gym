{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Test.Hspec.BenchGolden.Types
-- Description : Core types for benchmark golden testing
-- Copyright   : (c) 2026
-- License     : MIT
-- Maintainer  : @ocramz
--
-- This module defines the core data types used by the golds-gym framework:
--
-- * 'BenchGolden' - Configuration for a benchmark golden test
-- * 'BenchConfig' - Configurable benchmark parameters
-- * 'GoldenStats' - Statistics stored in golden files
-- * 'ArchConfig' - Machine architecture identification
-- * 'BenchResult' - Result of comparing benchmark against golden

module Test.Hspec.BenchGolden.Types
  ( -- * Benchmark Configuration
    BenchGolden(..)
  , BenchConfig(..)
  , defaultBenchConfig
  , BenchAction(..)

    -- * Golden File Statistics
  , GoldenStats(..)

    -- * Architecture Configuration
  , ArchConfig(..)

    -- * Benchmark Results
  , BenchResult(..)
  , Warning(..)
  ) where

import Data.Aeson
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.Word (Word64)
import GHC.Generics (Generic)

-- Note: Expectation type is defined in Lenses module to avoid circular imports

-- | A benchmarkable action that can be run multiple times.
-- The 'Word64' parameter represents the number of iterations to execute.
newtype BenchAction = BenchAction { runBenchAction :: Word64 -> IO () }

-- | Configuration for a single benchmark golden test.
data BenchGolden = BenchGolden
  { benchName   :: !String
    -- ^ Name of the benchmark (used for golden file naming)
  , benchAction :: !BenchAction
    -- ^ The benchmarkable action to run
  , benchConfig :: !BenchConfig
    -- ^ Configuration parameters
  }

-- | Configurable parameters for benchmark execution and comparison.
data BenchConfig = BenchConfig
  { iterations            :: !Int
    -- ^ Number of benchmark iterations to run
  , warmupIterations      :: !Int
    -- ^ Number of warm-up iterations (discarded before measurement)
  , tolerancePercent      :: !Double
    -- ^ Allowed deviation in mean time (as percentage, e.g., 15.0 = ±15%)
  , absoluteToleranceMs   :: !(Maybe Double)
    -- ^ Minimum absolute tolerance in milliseconds (e.g., 0.01 = 10 microseconds).
    --   When set, benchmarks pass if EITHER the percentage difference is within
    --   'tolerancePercent' OR the absolute time difference is within this threshold.
    --   This prevents false failures for extremely fast operations (< 1ms) where
    --   measurement noise causes large percentage variations despite negligible
    --   absolute time differences. Set to 'Nothing' to disable (percentage-only).
  , warnOnVarianceChange  :: !Bool
    -- ^ Whether to emit warnings when stddev changes significantly
  , varianceTolerancePercent :: !Double
    -- ^ Allowed deviation in stddev (as percentage)
  , outputDir             :: !FilePath
    -- ^ Directory for storing golden files
  , failOnFirstRun        :: !Bool
    -- ^ Whether to fail if no golden file exists yet
  , useRobustStatistics   :: !Bool
    -- ^ Use robust statistics (trimmed mean, MAD) instead of mean/stddev
  , trimPercent           :: !Double
    -- ^ Percentage to trim from each tail for trimmed mean (e.g., 10.0 = 10%)
  , outlierThreshold      :: !Double
    -- ^ MAD multiplier for outlier detection (e.g., 3.0 = 3 MADs from median)
  } deriving (Show, Eq, Generic)

-- | Default benchmark configuration with sensible defaults.
--
-- * 100 iterations
-- * 5 warm-up iterations
-- * 15% tolerance on mean time
-- * 0.01 ms (10 microseconds) absolute tolerance - prevents false failures for fast operations
-- * Variance warnings enabled at 50% tolerance
-- * Output to @.golden/@ directory
-- * Success on first run (creates baseline)
--
-- = Hybrid Tolerance Strategy
--
-- The default configuration uses BOTH percentage and absolute tolerance:
--
-- * Benchmarks pass if mean time is within ±15% OR within ±0.01ms
-- * This prevents measurement noise from failing fast operations (< 1ms)
-- * For slower operations (> 1ms), percentage tolerance dominates
--
-- Set @absoluteToleranceMs = Nothing@ for percentage-only comparison.
defaultBenchConfig :: BenchConfig
defaultBenchConfig = BenchConfig
  { iterations            = 100
  , warmupIterations      = 5
  , tolerancePercent      = 15.0
  , absoluteToleranceMs   = Just 0.01  -- 10 microseconds
  , warnOnVarianceChange  = True
  , varianceTolerancePercent = 50.0
  , outputDir             = ".golden"
  , failOnFirstRun        = False
  , useRobustStatistics   = False
  , trimPercent           = 10.0
  , outlierThreshold      = 3.0
  }

-- | Statistics stored in golden files.
--
-- These represent the baseline performance characteristics of a benchmark
-- on a specific architecture.
data GoldenStats = GoldenStats
  { statsMean        :: !Double
    -- ^ Mean execution time in milliseconds
  , statsStddev      :: !Double
    -- ^ Standard deviation in milliseconds
  , statsMedian      :: !Double
    -- ^ Median execution time in milliseconds
  , statsMin         :: !Double
    -- ^ Minimum execution time in milliseconds
  , statsMax         :: !Double
    -- ^ Maximum execution time in milliseconds
  , statsPercentiles :: ![(Int, Double)]
    -- ^ Percentile values (e.g., [(50, 1.2), (90, 1.5), (99, 1.8)])
  , statsArch        :: !Text
    -- ^ Architecture identifier
  , statsTimestamp   :: !UTCTime
    -- ^ When this baseline was recorded
  , statsTrimmedMean :: !Double
    -- ^ Trimmed mean (with tails removed) in milliseconds
  , statsMAD         :: !Double
    -- ^ Median absolute deviation in milliseconds
  , statsIQR         :: !Double
    -- ^ Interquartile range (Q3 - Q1) in milliseconds
  , statsOutliers    :: ![Double]
    -- ^ List of detected outlier timings in milliseconds
  } deriving (Show, Eq, Generic)

-- | Machine architecture configuration.
--
-- Used to generate unique identifiers for golden file directories,
-- ensuring benchmarks are only compared against equivalent hardware.
data ArchConfig = ArchConfig
  { archId    :: !Text
    -- ^ Unique identifier (e.g., "aarch64-darwin-Apple_M1")
  , archOS    :: !Text
    -- ^ Operating system (e.g., "darwin", "linux")
  , archCPU   :: !Text
    -- ^ CPU architecture (e.g., "aarch64", "x86_64")
  , archModel :: !(Maybe Text)
    -- ^ CPU model if available (e.g., "Apple M1", "Intel Core i7")
  } deriving (Show, Eq, Generic)

-- | Result of running a benchmark and comparing against golden.
data BenchResult
  = FirstRun !GoldenStats
    -- ^ No golden file existed; baseline created
  | Pass !GoldenStats !GoldenStats ![Warning]
    -- ^ Benchmark passed (golden stats, actual stats, warnings)
  | Regression !GoldenStats !GoldenStats !Double !Double !(Maybe Double)
    -- ^ Performance regression (golden, actual, percent change, tolerance, absolute tolerance)
  | Improvement !GoldenStats !GoldenStats !Double !Double !(Maybe Double)
    -- ^ Performance improvement (golden, actual, percent change, tolerance, absolute tolerance)
  deriving (Show, Eq)

-- | Warnings that may be emitted during benchmark comparison.
data Warning
  = VarianceIncreased !Double !Double !Double !Double
    -- ^ Stddev increased (golden, actual, percent change, tolerance)
  | VarianceDecreased !Double !Double !Double !Double
    -- ^ Stddev decreased significantly (golden, actual, percent change, tolerance)
  | HighVariance !Double
    -- ^ Current run has unusually high variance
  | OutliersDetected !Int ![Double]
    -- ^ Outliers detected (count, list of outlier timings)
  deriving (Show, Eq)

-- JSON instances for GoldenStats (stored in golden files)

instance ToJSON GoldenStats where
  toJSON GoldenStats{..} = object
    [ "mean"        .= statsMean
    , "stddev"      .= statsStddev
    , "median"      .= statsMedian
    , "min"         .= statsMin
    , "max"         .= statsMax
    , "percentiles" .= statsPercentiles
    , "architecture" .= statsArch
    , "timestamp"   .= statsTimestamp
    , "trimmedMean" .= statsTrimmedMean
    , "mad"         .= statsMAD
    , "iqr"         .= statsIQR
    , "outliers"    .= statsOutliers
    ]

instance FromJSON GoldenStats where
  parseJSON = withObject "GoldenStats" $ \v -> GoldenStats
    <$> v .: "mean"
    <*> v .: "stddev"
    <*> v .: "median"
    <*> v .: "min"
    <*> v .: "max"
    <*> v .: "percentiles"
    <*> v .: "architecture"
    <*> v .: "timestamp"
    <*> v .: "trimmedMean"
    <*> v .: "mad"
    <*> v .: "iqr"
    <*> v .: "outliers"

instance ToJSON ArchConfig where
  toJSON ArchConfig{..} = object
    [ "id"    .= archId
    , "os"    .= archOS
    , "cpu"   .= archCPU
    , "model" .= archModel
    ]

instance FromJSON ArchConfig where
  parseJSON = withObject "ArchConfig" $ \v -> ArchConfig
    <$> v .: "id"
    <*> v .: "os"
    <*> v .: "cpu"
    <*> v .:? "model"


