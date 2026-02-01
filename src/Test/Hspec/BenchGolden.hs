{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -Wno-orphans #-}

-- |
-- Module      : Test.Hspec.BenchGolden
-- Description : Golden testing for performance benchmarks
-- Copyright   : (c) 2026
-- License     : MIT
-- Maintainer  : your.email@example.com
--
-- = Overview
--
-- @golds-gym@ is a framework for golden testing of performance benchmarks.
-- It integrates with hspec and uses benchpress for lightweight timing measurements.
--
-- Optionally, benchmarks can use robust statistics to mitigate the impact of outliers.
--
-- = Quick Start
--
-- @
-- import Test.Hspec
-- import Test.Hspec.BenchGolden
--
-- main :: IO ()
-- main = hspec $ do
--   describe \"Performance\" $ do
--     `benchGolden` "my algorithm" $
--       return $ myAlgorithm input
-- @
--
-- = How It Works
--
-- 1. On first run, the benchmark is executed and results are saved to a
--    golden file as the baseline.
--
-- 2. On subsequent runs, the benchmark is executed and compared against
--    the baseline using a configurable tolerance (default ±15%).
--
-- 3. If the mean time exceeds the tolerance, the test fails with a
--    regression or improvement notification.
--
-- = Architecture-Specific Baselines
--
-- Golden files are stored per-architecture to ensure benchmarks are only
-- compared against equivalent hardware. The architecture identifier includes
-- CPU type, OS, and CPU model.
--
-- = Configuration
--
-- Use 'benchGoldenWith' with a custom 'BenchConfig' to adjust:
--
-- * Number of iterations
-- * Warm-up iterations
-- * Tolerance percentage
-- * Absolute tolerance (hybrid tolerance strategy)
-- * Variance warnings
-- * Robust statistics mode (trimmed mean, MAD, outlier detection)
--
-- == Tolerance Configuration
--
-- The framework supports two tolerance mechanisms that work together:
--
-- 1. __Percentage tolerance__ ('tolerancePercent'): Checks if the mean time
--    change is within ±X% of the baseline. This is the traditional approach
--    and works well for operations that take more than a few milliseconds.
--
-- 2. __Absolute tolerance__ ('absoluteToleranceMs'): Checks if the absolute
--    time difference is within X milliseconds. This prevents false failures
--    for extremely fast operations (< 1ms) where measurement noise causes
--    large percentage variations despite negligible absolute differences.
--
-- By default, benchmarks pass if __EITHER__ tolerance is satisfied:
--
-- @
-- pass = (percentChange <= 15%) OR (absTimeDiff <= 0.01 ms)
-- @
--
-- This hybrid strategy combines the benefits of both approaches:
--
-- * For fast operations (< 1ms): Absolute tolerance dominates, preventing noise
-- * For slow operations (> 1ms): Percentage tolerance dominates, catching real regressions
--
-- To disable absolute tolerance and use percentage-only comparison:
--
-- @
-- benchGoldenWith defaultBenchConfig
--   { absoluteToleranceMs = Nothing
--   }
--   \"benchmark\" $ ...
-- @
--
-- To adjust the absolute tolerance threshold:
--
-- @
-- benchGoldenWith defaultBenchConfig
--   { absoluteToleranceMs = Just 0.001  -- 1 microsecond (very strict)
--   }
--   \"benchmark\" $ ...
-- @
---- = Lens-Based Expectations (Advanced)
--
-- For custom performance expectations, use lens-based combinators:
--
-- @
-- import Test.Hspec.BenchGolden.Lenses
--
-- -- Median-based comparison instead of mean
-- benchGoldenWithExpectation "median test" defaultBenchConfig
--   [expect _statsMedian (Percent 10.0)]
--   myAction
--
-- -- Compose multiple expectations
-- benchGoldenWithExpectation "strict test" defaultBenchConfig
--   [ expect _statsMean (Percent 15.0) &&~
--     expect _statsMAD (Percent 50.0)
--   ]
--   myAction
--
-- -- Expect improvement (must be faster)
-- benchGoldenWithExpectation "optimization" defaultBenchConfig
--   [expect _statsMean (MustImprove 10.0)]  -- Must be ≥10% faster
--   myAction
-- @
---- = Environment Variables
--
-- * @GOLDS_GYM_ACCEPT=1@ - Regenerate all golden files
-- * @GOLDS_GYM_SKIP=1@ - Skip all benchmark tests
-- * @GOLDS_GYM_ARCH=custom-id@ - Override architecture detection

module Test.Hspec.BenchGolden
  ( -- * Spec Combinators
    benchGolden
  , benchGoldenWith
  , benchGoldenIO
  , benchGoldenIOWith
  , benchGoldenWithExpectation

    -- * Configuration
  , BenchConfig(..)
  , defaultBenchConfig

    -- * Types
  , BenchGolden(..)
  , GoldenStats(..)
  , BenchResult(..)
  , Warning(..)
  , ArchConfig(..)

    -- * Low-Level API
  , runBenchGolden

    -- * Lens-Based Expectations
  , module Test.Hspec.BenchGolden.Lenses

    -- * Re-exports
  , module Test.Hspec.BenchGolden.Arch
  ) where

import Data.IORef
import qualified Data.Text as T
import System.Environment (lookupEnv)
import Text.Printf (printf)
import qualified Text.PrettyPrint.Boxes as Box

import Test.Hspec.Core.Spec

import Test.Hspec.BenchGolden.Arch
import qualified Test.Hspec.BenchGolden.Lenses as L
import Test.Hspec.BenchGolden.Lenses hiding (Expectation)
import Test.Hspec.BenchGolden.Runner (runBenchGolden, setAcceptGoldens, setSkipBenchmarks)
import Test.Hspec.BenchGolden.Types

-- | Create a benchmark golden test with default configuration.
--
-- This is the simplest way to add a benchmark test:
--
-- @
-- describe "Sorting" $ do
--   benchGolden "quicksort 1000 elements" $
--     return $ quicksort [1000, 999..1]
-- @
--
-- Default configuration:
--
-- * 100 iterations
-- * 5 warm-up iterations
-- * 15% tolerance
-- * Variance warnings enabled
-- * Standard statistics (not robust mode)
benchGolden :: 
    String  -- ^ Name of the benchmark
    -> IO () -- ^ The IO action to benchmark
    -> Spec
benchGolden name action = benchGoldenWith defaultBenchConfig name action

-- | Create a benchmark golden test with custom configuration.
--
-- Examples:
--
-- @
-- -- Tighter tolerance for critical code
-- benchGoldenWith defaultBenchConfig
--   { iterations = 500
--   , tolerancePercent = 5.0
--   , warmupIterations = 20
--   }
--   "hot loop" $
--   return $ criticalFunction input
--
-- -- Robust statistics mode for noisy environments
-- benchGoldenWith defaultBenchConfig
--   { useRobustStatistics = True
--   , trimPercent = 10.0
--   , outlierThreshold = 3.0
--   }
--   "benchmark with outliers" $
--   return $ computation input
-- @
benchGoldenWith :: BenchConfig  -- ^ Configuration parameters
    -> String -- ^ Name of the benchmark
    -> IO () -- ^ The IO action to benchmark
    -> Spec
benchGoldenWith config name action =
  it name $ BenchGolden
    { benchName   = name
    , benchAction = action
    , benchConfig = config
    }

-- | Create a benchmark golden test for an IO action.
--
-- This is an alias for 'benchGolden' that makes it clear the action
-- involves IO (e.g., file operations, network calls).
--
-- @
-- benchGoldenIO "file read" $ do
--   contents <- readFile "large-file.txt"
--   evaluate (length contents)
-- @
--
-- Note: For IO actions in noisy environments (CI, shared systems),
-- consider using 'benchGoldenIOWith' with @useRobustStatistics = True@.
benchGoldenIO :: String -- ^ Name of the benchmark
    -> IO () -- ^ The IO action to benchmark
    -> Spec
benchGoldenIO = benchGolden

-- | Create an IO benchmark golden test with custom configuration.
benchGoldenIOWith :: BenchConfig -- ^ Configuration parameters
    -> String -- ^ Name of the benchmark
    -> IO () -- ^ The IO action to benchmark
    -> Spec
benchGoldenIOWith = benchGoldenWith

-- | Create a benchmark golden test with custom lens-based expectations.
--
-- This combinator allows you to specify custom performance expectations using
-- lenses and tolerance combinators. Expectations can be composed using boolean
-- operators ('&&~', '||~').
--
-- Examples:
--
-- @
-- -- Median-based comparison (more robust to outliers)
-- benchGoldenWithExpectation "median test" defaultBenchConfig
--   [expect _statsMedian (Percent 10.0)]
--   myAction
--
-- -- Multiple metrics must pass (AND composition)
-- benchGoldenWithExpectation "strict test" defaultBenchConfig
--   [ expect _statsMean (Percent 15.0) &&~
--     expect _statsMAD (Percent 50.0)
--   ]
--   myAction
--
-- -- Either metric can pass (OR composition)
-- benchGoldenWithExpectation "flexible test" defaultBenchConfig
--   [ expect _statsMedian (Percent 10.0) ||~
--     expect _statsMin (Absolute 0.01)
--   ]
--   myAction
--
-- -- Expect performance improvement (must be faster)
-- benchGoldenWithExpectation "optimization" defaultBenchConfig
--   [expect _statsMean (MustImprove 10.0)]  -- Must be ≥10% faster
--   myAction
--
-- -- Expect controlled regression (for feature additions)
-- benchGoldenWithExpectation "new feature" defaultBenchConfig
--   [expect _statsMean (MustRegress 5.0)]  -- Accept 5-20% slowdown
--   myAction
--
-- -- Low variance requirement
-- benchGoldenWithExpectation "stable perf" defaultBenchConfig
--   [ expect _statsMean (Percent 15.0) &&~
--     expect _statsIQR (Absolute 0.1)
--   ]
--   myAction
-- @
--
-- Note: Expectations are checked against golden files. On first run, a baseline
-- is created. Use @GOLDS_GYM_ACCEPT=1@ to regenerate baselines.
benchGoldenWithExpectation ::
    String        -- ^ Name of the benchmark
    -> BenchConfig  -- ^ Configuration parameters
    -> [L.Expectation]  -- ^ List of expectations (all must pass)
    -> IO ()       -- ^ The IO action to benchmark
    -> Spec
benchGoldenWithExpectation name config expectations action =
  it name $ BenchGoldenWithExpectations name action config expectations

-- | Data type for benchmarks with custom lens-based expectations.
data BenchGoldenWithExpectations = BenchGoldenWithExpectations
  !String        -- Name
  !(IO ())       -- Action
  !BenchConfig   -- Config
  ![L.Expectation] -- Expectations

-- | Instance for BenchGolden without arguments.
instance Example BenchGolden where
  type Arg BenchGolden = ()
  evaluateExample bg params hook progress =
    evaluateExample (\() -> bg) params hook progress

-- | Instance for BenchGolden with an argument.
--
-- This allows benchmarks to receive setup data from @before@ or @around@ combinators.
instance Example (arg -> BenchGolden) where
  type Arg (arg -> BenchGolden) = arg
  evaluateExample bgFn _params hook _progress = do
    -- Read environment variables to determine accept/skip flags
    acceptEnv <- lookupEnv "GOLDS_GYM_ACCEPT"
    skipEnv <- lookupEnv "GOLDS_GYM_SKIP"
    
    let shouldAccept = case acceptEnv of
          Just "1"    -> True
          Just "true" -> True
          Just "yes"  -> True
          _           -> False
        shouldSkip = case skipEnv of
          Just "1"    -> True
          Just "true" -> True
          Just "yes"  -> True
          _           -> False
    
    -- Store the flags so Runner can access them
    setAcceptGoldens shouldAccept
    setSkipBenchmarks shouldSkip
    
    ref <- newIORef (Result "" Success)
    hook $ \arg -> do
      let bg = bgFn arg
      result <- runBenchGolden bg
      writeIORef ref (fromBenchResult result)
    readIORef ref

-- | Instance for BenchGoldenWithExpectations (custom expectations).
instance Example BenchGoldenWithExpectations where
  type Arg BenchGoldenWithExpectations = ()
  evaluateExample (BenchGoldenWithExpectations name action config expectations) _params hook _progress = do
    -- Read environment variables to determine accept/skip flags
    acceptEnv <- lookupEnv "GOLDS_GYM_ACCEPT"
    skipEnv <- lookupEnv "GOLDS_GYM_SKIP"
    
    let shouldAccept = case acceptEnv of
          Just "1"    -> True
          Just "true" -> True
          Just "yes"  -> True
          _           -> False
        shouldSkip = case skipEnv of
          Just "1"    -> True
          Just "true" -> True
          Just "yes"  -> True
          _           -> False
    
    -- Store the flags so Runner can access them
    setAcceptGoldens shouldAccept
    setSkipBenchmarks shouldSkip
    
    ref <- newIORef (Result "" Success)
    hook $ \() -> do
      result <- runBenchGoldenWithExpectations name action config expectations
      writeIORef ref (fromBenchResultWithExpectations expectations result)
    readIORef ref

-- | Run a benchmark with custom expectations.
runBenchGoldenWithExpectations :: String -> IO () -> BenchConfig -> [L.Expectation] -> IO BenchResult
runBenchGoldenWithExpectations name action config expectations = do
  -- Convert to BenchGolden and run normally first
  let bg = BenchGolden name action config
  result <- runBenchGolden bg
  
  -- Then check expectations for Pass/Regression/Improvement results
  case result of
    FirstRun stats -> return $ FirstRun stats
    Pass golden actual warnings ->
      -- Check all expectations
      let allPass = all (\e -> L.checkExpectation e golden actual) expectations
      in if allPass
         then return $ Pass golden actual warnings
         else return $ Regression golden actual 100.0 0.0 Nothing  -- Indicate expectation failure
    Regression golden actual pct tol absTol ->
      -- Check if regression is acceptable per expectations
      let allPass = all (\e -> L.checkExpectation e golden actual) expectations
      in if allPass
         then return $ Pass golden actual []
         else return $ Regression golden actual pct tol absTol
    Improvement golden actual pct tol absTol ->
      -- Check if improvement satisfies expectations
      let allPass = all (\e -> L.checkExpectation e golden actual) expectations
      in if allPass
         then return $ Pass golden actual []
         else return $ Improvement golden actual pct tol absTol

-- | Convert expectation-based benchmark result to hspec Result.
fromBenchResultWithExpectations :: [L.Expectation] -> BenchResult -> Result
fromBenchResultWithExpectations _expectations = fromBenchResult

-- | Convert a benchmark result to an hspec Result.
fromBenchResult :: BenchResult -> Result
fromBenchResult result = case result of
  FirstRun stats ->
    Result (formatFirstRun stats) Success

  Pass golden actual warnings ->
    let info = formatPass golden actual
        warningInfo = formatWarnings warnings
    in Result (info ++ warningInfo) Success

  Regression golden actual pctChange tolerance absToleranceMs ->
    let toleranceDesc :: String
        toleranceDesc = case absToleranceMs of
          Nothing -> printf "tolerance: %.1f%%" tolerance
          Just absMs -> printf "tolerance: %.1f%% or %.3f ms" tolerance absMs
        message = printf "Mean time increased by %.1f%% (%s)\n\n%s"
                    pctChange toleranceDesc (formatRegression golden actual)
    in Result message (Failure Nothing (Reason message))

  Improvement golden actual pctChange tolerance absToleranceMs ->
    let toleranceDesc :: String
        toleranceDesc = case absToleranceMs of
          Nothing -> printf "tolerance: %.1f%%" tolerance
          Just absMs -> printf "tolerance: %.1f%% or %.3f ms" tolerance absMs
    in Result (printf "Performance improved by %.1f%% (%s)\n%s"
                pctChange toleranceDesc (formatPass golden actual))
      Success

-- | Format statistics for the first run.
formatFirstRun :: GoldenStats -> String
formatFirstRun stats = "First run - baseline created\n" ++ formatStats stats

-- | Format a regression comparison with full details.
formatRegression :: GoldenStats -> GoldenStats -> String
formatRegression golden actual =
  let meanDiff = if statsMean golden == 0
                 then 0
                 else ((statsMean actual - statsMean golden) / statsMean golden) * 100
      stddevDiff = if statsStddev golden == 0
                   then 0
                   else ((statsStddev actual - statsStddev golden) / statsStddev golden) * 100
      medianDiff = if statsMedian golden == 0
                   then 0
                   else ((statsMedian actual - statsMedian golden) / statsMedian golden) * 100
      
      -- Create detailed comparison table
      metricCol = Box.vcat Box.left $ map Box.text 
        ["Metric", "------", "Mean", "Stddev", "Median", "Min", "Max"]
      actualCol = Box.vcat Box.right $ map Box.text 
        [ "Actual"
        , "------"
        , printf "%.3f ms" (statsMean actual)
        , printf "%.3f ms" (statsStddev actual)
        , printf "%.3f ms" (statsMedian actual)
        , printf "%.3f ms" (statsMin actual)
        , printf "%.3f ms" (statsMax actual)
        ]
      baselineCol = Box.vcat Box.right $ map Box.text
        [ "Baseline"
        , "--------"
        , printf "%.3f ms" (statsMean golden)
        , printf "%.3f ms" (statsStddev golden)
        , printf "%.3f ms" (statsMedian golden)
        , printf "%.3f ms" (statsMin golden)
        , printf "%.3f ms" (statsMax golden)
        ]
      diffCol = Box.vcat Box.right $ map Box.text
        [ "Diff"
        , "----"
        , printf "%+.1f%%" meanDiff
        , printf "%+.1f%%" stddevDiff
        , printf "%+.1f%%" medianDiff
        , ""
        , ""
        ]
      
      table = Box.hsep 2 Box.top [metricCol, actualCol, baselineCol, diffCol]
  in Box.render table

-- | Format a passing comparison.
formatPass :: GoldenStats -> GoldenStats -> String
formatPass golden actual =
  let meanDiff = if statsMean golden == 0
                 then 0
                 else ((statsMean actual - statsMean golden) / statsMean golden) * 100
      stddevDiff = if statsStddev golden == 0
                   then 0
                   else ((statsStddev actual - statsStddev golden) / statsStddev golden) * 100
      
      -- Create table with metric, actual, baseline, and diff columns
      metricCol = Box.vcat Box.left $ map Box.text ["Metric", "------", "Mean", "Stddev"]
      actualCol = Box.vcat Box.right $ map Box.text 
        [ "Actual"
        , "------"
        , printf "%.3f ms" (statsMean actual)
        , printf "%.3f ms" (statsStddev actual)
        ]
      baselineCol = Box.vcat Box.right $ map Box.text
        [ "Baseline"
        , "--------"
        , printf "%.3f ms" (statsMean golden)
        , printf "%.3f ms" (statsStddev golden)
        ]
      diffCol = Box.vcat Box.right $ map Box.text
        [ "Diff"
        , "----"
        , printf "%+.1f%%" meanDiff
        , printf "%+.1f%%" stddevDiff
        ]
      
      table = Box.hsep 2 Box.top [metricCol, actualCol, baselineCol, diffCol]
  in Box.render table

-- | Format statistics for display.
formatStats :: GoldenStats -> String
formatStats GoldenStats{..} =
  let metricCol = Box.vcat Box.left $ map Box.text
        [ "Metric", "------", "Mean", "Stddev", "Median", "Min", "Max", "Arch" ]
      valueCol = Box.vcat Box.right $ map Box.text
        [ "Value"
        , "-----"
        , printf "%.3f ms" statsMean
        , printf "%.3f ms" statsStddev
        , printf "%.3f ms" statsMedian
        , printf "%.3f ms" statsMin
        , printf "%.3f ms" statsMax
        , T.unpack statsArch
        ]
      table = Box.hsep 2 Box.top [metricCol, valueCol]
  in Box.render table

-- | Format warnings for display.
formatWarnings :: [Warning] -> String
formatWarnings [] = ""
formatWarnings ws = "\nWarnings:\n" ++ unlines (map formatWarning ws)

-- | Format a single warning.
formatWarning :: Warning -> String
formatWarning w = case w of
  VarianceIncreased golden actual pct tolerance ->
    printf "  ⚠ Variance increased by %.1f%% (%.3f ms -> %.3f ms, tolerance: %.1f%%)"
      pct golden actual tolerance

  VarianceDecreased golden actual pct tolerance ->
    printf "  ⚠ Variance decreased by %.1f%% (%.3f ms -> %.3f ms, tolerance: %.1f%%)"
      pct golden actual tolerance

  HighVariance cv ->
    printf "  ⚠ High variance detected (CV = %.1f%%)" (cv * 100)

  OutliersDetected count outliers ->
    let outlierStr = if count <= 5
                     then unwords (map (printf "%.3fms") outliers)
                     else unwords (map (printf "%.3fms") (take 5 outliers)) ++ "..."
    in printf "  ⚠ %d outlier(s) detected: %s" count outlierStr
