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
-- Maintainer  : @ocramz
--
-- = Overview
--
-- @golds-gym@ is a framework for golden testing of performance benchmarks.
-- It integrates with hspec and uses benchpress for lightweight timing measurements.
--
-- Benchmarks can use robust statistics to mitigate the impact of outliers.
--
-- The library can be used both to assert that performance does not regress, and to set expectations
-- for improvements across project versions (see `benchGoldenWithExpectation`).
--
-- = Quick Start
--
-- @
-- import Test.Hspec
-- import Test.Hspec.BenchGolden
-- import Data.List (sort)
--
-- main :: IO ()
-- main = hspec $ do
--   describe \"Performance\" $ do
--     -- Pure function with normal form evaluation
--     `benchGolden` "list sorting" $
--       `nf` (\\n -> sort [n, n-1 .. 1]) 1000
--
--     -- IO action with result forced to normal form
--     `benchGolden` "file read" $
--       `nfIO` (readFile "data.txt")
-- @
--
-- __Evaluation strategies__ control how values are forced:
--
-- * 'nf' - Force to normal form (deep evaluation, use for most cases)
-- * 'nfIO' - Variant for IO actions
-- * 'nfAppIO' - For functions returning IO
-- * 'io' - Plain IO action without forcing
--
-- Without proper evaluation strategies, GHC may optimize away computations
-- or share results across iterations, making benchmarks meaningless.
--
-- = Best Practices: Avoiding Shared Thunks
--
-- __CRITICAL:__ When benchmarking with data structures, ensure the data is
-- reconstructed on each iteration to avoid measuring shared, cached results.
--
-- ❌ __Anti-pattern__ (shared list across iterations):
--
-- @
-- benchGolden "sum" $ nf sum [1..1000000]
-- @
--
-- The list @[1..1000000]@ is constructed once and shared across all iterations.
-- This allocates the entire list in memory, creates GC pressure, and prevents
-- list fusion. The first iteration evaluates the shared thunk, and subsequent
-- iterations measure cached results.
--
-- ✅ __Correct pattern__ (list reconstructed per iteration):
--
-- @
-- benchGolden "sum" $ nf (\\n -> sum [1..n]) 1000000
-- @
--
-- The lambda wrapper ensures the list is reconstructed on every iteration,
-- measuring the true cost of both construction and computation.
--
-- __Other considerations:__
--
-- * Ensure return types are inhabited enough to depend on all computations
--   (avoid @b ~ ()@ where GHC might optimize away the payload)
-- * For inlinable functions, ensure full saturation: prefer @nf (\\n -> f n) x@
--   over @nf f x@ to guarantee inlining and rewrite rules fire
-- * Use 'NFData' constraints where applicable to ensure deep evaluation
--
-- = How It Works
--
-- 1. On first run, the benchmark is executed and results are saved to a
--    golden file as the baseline.
--
-- 2. On subsequent runs, the benchmark is executed and compared against
--    the baseline using a configurable tolerance or expectation combinators.
--
-- = Architecture-Specific Baselines
--
-- Golden files are stored per-architecture to ensure benchmarks are only
-- compared against equivalent hardware. The architecture identifier includes
-- CPU type, OS, and CPU model.
--
-- = Configuration
--
-- Use 'benchGoldenWith' or 'benchGoldenWithExpectation' with a custom 'BenchConfig':
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
--   (nf myAlgorithm input)
--
-- -- Compose multiple expectations
-- benchGoldenWithExpectation "strict test" defaultBenchConfig
--   [ expect _statsMean (Percent 15.0) &&~
--     expect _statsMAD (Percent 50.0)
--   ]
--   (nf criticalFunction data)
--
-- -- Expect improvement (must be faster)
-- benchGoldenWithExpectation "optimization" defaultBenchConfig
--   [expect _statsMean (MustImprove 10.0)]  -- Must be ≥10% faster
--   (nf optimizedVersion input)
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
  , benchGoldenWithExpectation

    -- * Configuration
  , BenchConfig(..)
  , defaultBenchConfig

    -- * Types
  , BenchGolden(..)
  , BenchAction(..)
  , GoldenStats(..)
  , BenchResult(..)
  , Warning(..)
  , ArchConfig(..)

    -- * Benchmarkable Constructors
  , nf
  , nfIO
  , nfAppIO
  , io

    -- * Low-Level API
  , runBenchGolden

    -- * Lens-Based Expectations
  , module Test.Hspec.BenchGolden.Lenses

    -- * Re-exports
  , module Test.Hspec.BenchGolden.Arch
  ) where

import Data.IORef
import qualified Data.Text as T
import Lens.Micro ((^.))
import System.Environment (lookupEnv)
import Text.Printf (printf)
import qualified Text.PrettyPrint.Boxes as Box

import Test.Hspec.Core.Spec

import Test.Hspec.BenchGolden.Arch
import qualified Test.Hspec.BenchGolden.Lenses as L
import Test.Hspec.BenchGolden.Lenses hiding (Expectation)
import Test.Hspec.BenchGolden.Runner (runBenchGolden, setAcceptGoldens, setSkipBenchmarks, nf, nfIO, nfAppIO, io)
import Test.Hspec.BenchGolden.Types

-- | Create a benchmark golden test with default configuration.
--
-- This is the simplest way to add a benchmark test:
--
-- @
-- describe "Sorting" $ do
--   benchGolden "quicksort 1000 elements" $
--     nf quicksort [1000, 999..1]
-- @
--
-- Use evaluation strategy combinators to control how values are forced:
--
-- * 'nf' - Normal form (deep evaluation)
-- * 'nfIO' - Normal form for IO actions
-- * 'nfAppIO' - Normal form for functions returning IO
-- * 'io' - Plain IO action (for backward compatibility)
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
    -> BenchAction -- ^ The benchmarkable action
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
--   nf criticalFunction input
--
-- -- Robust statistics mode for noisy environments
-- benchGoldenWith defaultBenchConfig
--   { useRobustStatistics = True
--   , trimPercent = 10.0
--   , outlierThreshold = 3.0
--   }
--   "benchmark with outliers" $
--   whnf computation input
-- @
benchGoldenWith :: BenchConfig  -- ^ Configuration parameters
    -> String -- ^ Name of the benchmark
    -> BenchAction -- ^ The benchmarkable action
    -> Spec
benchGoldenWith config name action =
  it name $ BenchGolden
    { benchName   = name
    , benchAction = action
    , benchConfig = config
    }


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
--   [`expect` `_statsMedian` (`Percent` 10.0)]
--   (nf sort [1000, 999..1])
--
-- -- Multiple metrics must pass (AND composition)
-- benchGoldenWithExpectation "strict test" defaultBenchConfig
--   [ expect `_statsMean` (Percent 15.0) &&~
--     expect `_statsMAD` (Percent 50.0)
--   ]
--   (nf algorithm data)
--
-- -- Either metric can pass (OR composition)
-- benchGoldenWithExpectation "flexible test" defaultBenchConfig
--   [ expect _statsMedian (Percent 10.0) ||~
--     expect _statsMin (`Absolute` 0.01)
--   ]
--   (nf fastOp input)
--
-- -- Expect performance improvement (must be faster)
-- benchGoldenWithExpectation "optimization" defaultBenchConfig
--   [expect _statsMean (`MustImprove` 10.0)]  -- Must be ≥10% faster
--   (nf optimizedVersion data)
--
-- -- Expect controlled regression (for feature additions)
-- benchGoldenWithExpectation "new feature" defaultBenchConfig
--   [expect _statsMean (`MustRegress` 5.0)]  -- Accept 5-20% slowdown
--   (nf newFeature input)
--
-- -- Low variance requirement
-- benchGoldenWithExpectation "stable perf" defaultBenchConfig
--   [ expect _statsMean (Percent 15.0) &&~
--     expect `_statsIQR` (Absolute 0.1)
--   ]
--   (nfIO stableOperation)
-- @
--
-- Note: Expectations are checked against golden files. On first run, a baseline
-- is created. Use @GOLDS_GYM_ACCEPT=1@ to regenerate baselines.
benchGoldenWithExpectation ::
    String        -- ^ Name of the benchmark
    -> BenchConfig  -- ^ Configuration parameters
    -> [L.Expectation]  -- ^ List of expectations (all must pass)
    -> BenchAction       -- ^ The benchmarkable action
    -> Spec
benchGoldenWithExpectation name config expectations action =
  it name $ BenchGoldenWithExpectations name action config expectations

-- | Data type for benchmarks with custom lens-based expectations.
data BenchGoldenWithExpectations = BenchGoldenWithExpectations
  !String        -- Name
  !BenchAction   -- Action
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
runBenchGoldenWithExpectations :: String -> BenchAction -> BenchConfig -> [L.Expectation] -> IO BenchResult
runBenchGoldenWithExpectations name action config expectations = do
  -- Convert to BenchGolden and run normally first
  let bg = BenchGolden name action config
  result <- runBenchGolden bg
  
  -- Extract tolerance from first expectation for error messages
  let (tolPct, tolAbs) = case expectations of
        [] -> (tolerancePercent config, absoluteToleranceMs config)
        (e:_) -> L.toleranceFromExpectation e
  
  -- Then check expectations for Pass/Regression/Improvement results
  case result of
    FirstRun stats -> return $ FirstRun stats
    Pass golden actual warnings ->
      -- Check all expectations
      let allPass = all (\e -> L.checkExpectation e golden actual) expectations
      in if allPass
         then return $ Pass golden actual warnings
         else 
           -- Expectations failed - calculate actual percentage diff for error message
           let lens = L.metricFor config
               goldenVal = golden ^. lens
               actualVal = actual ^. lens
               meanDiff = if goldenVal == 0 
                         then 100.0 
                         else ((actualVal - goldenVal) / goldenVal) * 100
           in return $ Regression golden actual meanDiff tolPct tolAbs
    Regression golden actual pct _tol _absTol ->
      -- Check if regression is acceptable per expectations
      let allPass = all (\e -> L.checkExpectation e golden actual) expectations
      in if allPass
         then return $ Pass golden actual []
         else return $ Regression golden actual pct tolPct tolAbs
    Improvement golden actual pct _tol _absTol ->
      -- Check if improvement satisfies expectations
      let allPass = all (\e -> L.checkExpectation e golden actual) expectations
      in if allPass
         then return $ Pass golden actual []
         else return $ Improvement golden actual pct tolPct tolAbs

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
        changeVerb :: String
        changeVerb = if pctChange >= 0 then "increased" else "decreased"
        absPctChange = abs pctChange
        message = printf "Mean time %s by %.1f%% (%s)\n\n%s"
                    changeVerb absPctChange toleranceDesc (formatRegression golden actual)
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
      baselineCol = Box.vcat Box.right $ map Box.text
        [ "Baseline"
        , "--------"
        , printf "%.3f ms" (statsMean golden)
        , printf "%.3f ms" (statsStddev golden)
        , printf "%.3f ms" (statsMedian golden)
        , printf "%.3f ms" (statsMin golden)
        , printf "%.3f ms" (statsMax golden)
        ]
      actualCol = Box.vcat Box.right $ map Box.text 
        [ "Actual"
        , "------"
        , printf "%.3f ms" (statsMean actual)
        , printf "%.3f ms" (statsStddev actual)
        , printf "%.3f ms" (statsMedian actual)
        , printf "%.3f ms" (statsMin actual)
        , printf "%.3f ms" (statsMax actual)
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
      
      table = Box.hsep 2 Box.top [metricCol, baselineCol, actualCol, diffCol]
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
      
      -- Create table with metric, baseline, actual, and diff columns
      metricCol = Box.vcat Box.left $ map Box.text ["Metric", "------", "Mean", "Stddev"]
      baselineCol = Box.vcat Box.right $ map Box.text
        [ "Baseline"
        , "--------"
        , printf "%.3f ms" (statsMean golden)
        , printf "%.3f ms" (statsStddev golden)
        ]
      actualCol = Box.vcat Box.right $ map Box.text 
        [ "Actual"
        , "------"
        , printf "%.3f ms" (statsMean actual)
        , printf "%.3f ms" (statsStddev actual)
        ]
      diffCol = Box.vcat Box.right $ map Box.text
        [ "Diff"
        , "----"
        , printf "%+.1f%%" meanDiff
        , printf "%+.1f%%" stddevDiff
        ]
      
      table = Box.hsep 2 Box.top [metricCol, baselineCol, actualCol, diffCol]
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
