{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Test.Hspec.BenchGolden.Runner
-- Description : Benchmark execution and golden file comparison
-- Copyright   : (c) 2026
-- License     : MIT
-- Maintainer  : your.email@example.com
--
-- This module handles running benchmarks and comparing results against
-- golden files. It includes:
--
-- * Benchmark execution with warm-up iterations
-- * Golden file I/O (reading/writing JSON statistics)
-- * Tolerance-based comparison with variance warnings
-- * Support for updating baselines via GOLDS_GYM_ACCEPT environment variable

module Test.Hspec.BenchGolden.Runner
  ( -- * Running Benchmarks
    runBenchGolden
  , runBenchmark
  , runBenchmarkWithRawTimings

    -- * Golden File Operations
  , readGoldenFile
  , writeGoldenFile
  , writeActualFile
  , getGoldenPath
  , getActualPath

    -- * Comparison
  , compareStats
  , checkVariance

    -- * Robust Statistics
  , calculateRobustStats
  , calculateTrimmedMean
  , calculateMAD
  , calculateIQR
  , detectOutliers

    -- * Environment
  , shouldUpdateGolden
  , shouldSkipBenchmarks
  , setAcceptGoldens
  , setSkipBenchmarks
  ) where

import Control.Monad (when, replicateM_)
import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sort)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Data.Vector.Unboxed as V
import qualified Statistics.Sample as Stats
import System.CPUTime (getCPUTime)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), (<.>))
import System.IO.Unsafe (unsafePerformIO)

import qualified Test.BenchPress as BP

import Test.Hspec.BenchGolden.Arch (detectArchitecture, sanitizeForFilename)
import Test.Hspec.BenchGolden.Types

-- | Run a benchmark golden test.
--
-- This function:
--
-- 1. Runs warm-up iterations (discarded)
-- 2. Runs the actual benchmark
-- 3. Writes actual results to @.actual@ file
-- 4. If no golden exists, creates it (first run)
-- 5. Otherwise, compares against golden with tolerance
--
-- The result includes any warnings (e.g., variance changes).
runBenchGolden :: BenchGolden -> IO BenchResult
runBenchGolden BenchGolden{..} = do
  -- Check for skip/update flags
  skip <- shouldSkipBenchmarks
  if skip
    then do
      -- Return a pass with dummy stats when skipped
      now <- getCurrentTime
      arch <- detectArchitecture
      let dummyStats = GoldenStats 0 0 0 0 0 [] (archId arch) now 0 0 0 []
      return $ Pass dummyStats dummyStats []
    else do
      -- Detect architecture for path construction
      arch <- detectArchitecture
      let archDir = T.unpack $ archId arch
          config  = benchConfig

      -- Create output directory
      let dir = outputDir config </> archDir
      createDirectoryIfMissing True dir

      -- Run warm-up iterations
      when (warmupIterations config > 0) $
        replicateM_ (warmupIterations config) benchAction

      -- Run the actual benchmark
      actualStats <- runBenchmark benchName benchAction config arch

      -- Write actual results
      writeActualFile (outputDir config) archDir benchName actualStats

      -- Check if we should force update
      update <- shouldUpdateGolden

      -- Read or create golden file
      let goldenPath = getGoldenPath (outputDir config) archDir benchName
      goldenExists <- doesFileExist goldenPath

      if update || not goldenExists
        then do
          -- First run or forced update: create/update golden
          writeGoldenFile (outputDir config) archDir benchName actualStats
          return $ FirstRun actualStats
        else do
          -- Compare against existing golden
          goldenResult <- readGoldenFile goldenPath
          case goldenResult of
            Left err -> error $ "Failed to read golden file: " ++ err
            Right goldenStats -> do
              let result = compareStats config goldenStats actualStats
              return result

-- | Run a benchmark and collect statistics.
runBenchmark :: String -> IO () -> BenchConfig -> ArchConfig -> IO GoldenStats
runBenchmark _name action config arch = do
  if useRobustStatistics config
    then runBenchmarkWithRawTimings _name action config arch
    else do
      -- Use benchpress for standard statistics
      (cpuStats, _wallStats) <- BP.benchmark
        (iterations config)
        (pure ())                    -- setup
        (const action)               -- action
        (const $ pure ())            -- teardown

      now <- getCurrentTime

      return GoldenStats
        { statsMean        = BP.mean cpuStats
        , statsStddev      = BP.stddev cpuStats
        , statsMedian      = BP.median cpuStats
        , statsMin         = BP.min cpuStats
        , statsMax         = BP.max cpuStats
        , statsPercentiles = BP.percentiles cpuStats
        , statsArch        = archId arch
        , statsTimestamp   = now
        , statsTrimmedMean = 0.0  -- Not calculated in non-robust mode
        , statsMAD         = 0.0
        , statsIQR         = 0.0
        , statsOutliers    = []
        }

-- | Run a benchmark with raw timing collection for robust statistics.
runBenchmarkWithRawTimings :: String -> IO () -> BenchConfig -> ArchConfig -> IO GoldenStats
runBenchmarkWithRawTimings _name action config arch = do
  -- Collect raw CPU timings
  timings <- mapM (const measureCPUTime) [1 .. iterations config]
  
  let sortedTimings = sort timings
      vec = V.fromList sortedTimings
      
      -- Standard statistics
      mean' = Stats.mean vec
      stddev' = Stats.stdDev vec
      -- Median: middle element of sorted vector
      median' = if V.null vec
                then 0.0
                else vec V.! (V.length vec `div` 2)
      min' = V.minimum vec
      max' = V.maximum vec
      
      -- Percentiles (matching benchpress format)
      percentiles' = [(p, quantile p vec) | p <- [50, 66, 75, 80, 90, 95, 98, 99, 100]]
      
      -- Robust statistics
      (trimmedMean', mad', iqr', outliers') = calculateRobustStats config vec median'

  now <- getCurrentTime

  return GoldenStats
    { statsMean        = mean'
    , statsStddev      = stddev'
    , statsMedian      = median'
    , statsMin         = min'
    , statsMax         = max'
    , statsPercentiles = percentiles'
    , statsArch        = archId arch
    , statsTimestamp   = now
    , statsTrimmedMean = trimmedMean'
    , statsMAD         = mad'
    , statsIQR         = iqr'
    , statsOutliers    = outliers'
    }
  where
    measureCPUTime = do
      startCpu <- getCPUTime
      action
      endCpu <- getCPUTime
      let cpuTime = picosToMillis (endCpu - startCpu)
      return cpuTime
    
    picosToMillis :: Integer -> Double
    picosToMillis t = realToFrac t / (10^(9 :: Int))
    
    quantile :: Int -> V.Vector Double -> Double
    quantile p v =
      let idx = ceiling ((fromIntegral (V.length v) / 100) * fromIntegral p :: Double) - 1
          safeIdx = max 0 (min (V.length v - 1) idx)
      in V.unsafeIndex v safeIdx

-- | Calculate robust statistics from raw timing data.
--
-- Returns: (trimmed mean, MAD, IQR, outliers)
calculateRobustStats :: BenchConfig -> V.Vector Double -> Double -> (Double, Double, Double, [Double])
calculateRobustStats config vec median' =
  let trimmedMean' = calculateTrimmedMean (trimPercent config) vec
      mad' = calculateMAD vec median'
      iqr' = calculateIQR vec
      outliers' = detectOutliers (outlierThreshold config) vec median' mad'
  in (trimmedMean', mad', iqr', outliers')

-- | Calculate trimmed mean by removing specified percentage from each tail.
calculateTrimmedMean :: Double -> V.Vector Double -> Double
calculateTrimmedMean trimPct vec
  | V.null vec = 0.0
  | trimPct <= 0 || trimPct >= 50 = Stats.mean vec
  | otherwise =
      let n = V.length vec
          trimCount = round (fromIntegral n * trimPct / 100.0)
          trimmed = V.slice trimCount (n - 2 * trimCount) vec
      in if V.null trimmed then Stats.mean vec else Stats.mean trimmed

-- | Calculate Median Absolute Deviation (MAD).
--
-- MAD = median(|x_i - median(x)|)
calculateMAD :: V.Vector Double -> Double -> Double
calculateMAD vec med
  | V.null vec = 0.0
  | otherwise =
      let deviations = V.toList $ V.map (\x -> abs (x - med)) vec
          sortedDevs = sort deviations
          n = length sortedDevs
      in sortedDevs !! (n `div` 2)

-- | Calculate Interquartile Range (IQR = Q3 - Q1).
calculateIQR :: V.Vector Double -> Double
calculateIQR vec
  | V.length vec < 4 = 0.0
  | otherwise =
      let q1 = quantileAt 25 vec
          q3 = quantileAt 75 vec
      in q3 - q1
  where
    quantileAt :: Int -> V.Vector Double -> Double
    quantileAt p v =
      let n = V.length v
          idx = max 0 $ min (n - 1) $ round (fromIntegral n * fromIntegral p / (100.0 :: Double)) - 1
      in V.unsafeIndex v idx

-- | Detect outliers using MAD-based threshold.
--
-- An observation is an outlier if: |x - median| > threshold * MAD
detectOutliers :: Double -> V.Vector Double -> Double -> Double -> [Double]
detectOutliers threshold vec med mad
  | V.null vec = []
  | mad == 0 = []  -- No variance, no outliers
  | otherwise =
      let isOutlier x = abs (x - med) > threshold * mad
      in V.toList $ V.filter isOutlier vec

-- | Compare actual stats against golden stats.
--
-- Returns a 'BenchResult' indicating whether the benchmark passed,
-- regressed, or improved, along with any warnings.
compareStats :: BenchConfig -> GoldenStats -> GoldenStats -> BenchResult
compareStats config golden actual =
  let -- Choose comparison metric based on config
      (goldenValue, actualValue) =
        if useRobustStatistics config
          then (statsTrimmedMean golden, statsTrimmedMean actual)
          else (statsMean golden, statsMean actual)

      -- Calculate percentage difference
      meanDiff = if goldenValue == 0
                 then if actualValue == 0 then 0 else 100
                 else ((actualValue - goldenValue) / goldenValue) * 100

      absDiff = abs meanDiff
      tolerance = tolerancePercent config

      -- Check variance if enabled
      baseWarnings = if warnOnVarianceChange config
                     then checkVariance config golden actual
                     else []
      
      -- Add outlier warnings if robust statistics enabled
      outlierWarnings = if useRobustStatistics config && not (null (statsOutliers actual))
                        then [OutliersDetected (length $ statsOutliers actual) (statsOutliers actual)]
                        else []
      
      warnings = baseWarnings ++ outlierWarnings

  in if absDiff <= tolerance
     then Pass golden actual warnings
     else if meanDiff > 0
          then Regression golden actual meanDiff tolerance
          else Improvement golden actual (abs meanDiff) tolerance

-- | Check for variance changes and generate warnings.
checkVariance :: BenchConfig -> GoldenStats -> GoldenStats -> [Warning]
checkVariance config golden actual =
  let -- Use MAD for robust statistics, stddev otherwise
      (goldenVar, actualVar) =
        if useRobustStatistics config
          then (statsMAD golden, statsMAD actual)
          else (statsStddev golden, statsStddev actual)

      varDiff = if goldenVar == 0
                then if actualVar == 0 then 0 else 100
                else ((actualVar - goldenVar) / goldenVar) * 100

      _absVarDiff = abs varDiff
      varTolerance = varianceTolerancePercent config

      -- Coefficient of variation (CV) for high variance detection
      -- Use appropriate measure based on mode
      cv = if useRobustStatistics config
           then if statsMedian actual == 0 then 0 else statsMAD actual / statsMedian actual
           else if statsMean actual == 0 then 0 else statsStddev actual / statsMean actual

  in concat
       [ [ VarianceIncreased goldenVar actualVar varDiff varTolerance
         | varDiff > varTolerance ]
       , [ VarianceDecreased goldenVar actualVar (abs varDiff) varTolerance
         | varDiff < negate varTolerance ]
       , [ HighVariance cv | cv > 0.5 ]  -- CV > 50% is considered high
       ]

-- | Get the path for a golden file.
getGoldenPath :: FilePath -> FilePath -> String -> FilePath
getGoldenPath outDir archDir name =
  outDir </> archDir </> sanitizeName name <.> "golden"

-- | Get the path for an actual results file.
getActualPath :: FilePath -> FilePath -> String -> FilePath
getActualPath outDir archDir name =
  outDir </> archDir </> sanitizeName name <.> "actual"

-- | Sanitize a benchmark name for use in filenames.
sanitizeName :: String -> FilePath
sanitizeName = T.unpack . sanitizeForFilename . T.pack

-- | Read a golden file.
readGoldenFile :: FilePath -> IO (Either String GoldenStats)
readGoldenFile = eitherDecodeFileStrict

-- | Write a golden file.
writeGoldenFile :: FilePath -> FilePath -> String -> GoldenStats -> IO ()
writeGoldenFile outDir archDir name stats = do
  let path = getGoldenPath outDir archDir name
  encodeFile path stats

-- | Write an actual results file.
writeActualFile :: FilePath -> FilePath -> String -> GoldenStats -> IO ()
writeActualFile outDir archDir name stats = do
  let path = getActualPath outDir archDir name
  encodeFile path stats

-- | Global state for tracking command-line flags set by hspec options.
{-# NOINLINE acceptGoldensRef #-}
acceptGoldensRef :: IORef Bool
acceptGoldensRef = unsafePerformIO $ newIORef False

{-# NOINLINE skipBenchmarksRef #-}
skipBenchmarksRef :: IORef Bool
skipBenchmarksRef = unsafePerformIO $ newIORef False

-- | Set the accept goldens flag (called from BenchGolden Example instance).
setAcceptGoldens :: Bool -> IO ()
setAcceptGoldens = writeIORef acceptGoldensRef

-- | Set the skip benchmarks flag (called from BenchGolden Example instance).
setSkipBenchmarks :: Bool -> IO ()
setSkipBenchmarks = writeIORef skipBenchmarksRef

-- | Check if golden files should be updated.
--
-- Returns 'True' if @GOLDS_GYM_ACCEPT@ environment variable is set.
-- 
-- Usage:
--
-- @
-- GOLDS_GYM_ACCEPT=1 cabal test
-- GOLDS_GYM_ACCEPT=1 stack test
-- @
shouldUpdateGolden :: IO Bool
shouldUpdateGolden = readIORef acceptGoldensRef

-- | Check if benchmarks should be skipped entirely.
--
-- Returns 'True' if @GOLDS_GYM_SKIP@ environment variable is set.
-- Useful for CI environments where benchmark hardware is inconsistent.
--
-- Usage:
--
-- @
-- GOLDS_GYM_SKIP=1 cabal test
-- GOLDS_GYM_SKIP=1 stack test
-- @
shouldSkipBenchmarks :: IO Bool
shouldSkipBenchmarks = readIORef skipBenchmarksRef
