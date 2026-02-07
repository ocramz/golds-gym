{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Test.Hspec.BenchGolden.Runner
-- Description : Benchmark execution and golden file comparison
-- Copyright   : (c) Marco Zocca 2026
-- License     : MIT
-- Maintainer  : @ocramz
--
-- This module handles running benchmarks and comparing results against
-- golden files. It includes:
--
-- * Benchmark execution with warm-up iterations
-- * Golden file IO (reading and writing JSON statistics)
-- * Tolerance-based comparison with variance warnings
-- * Support for updating baselines via GOLDS_GYM_ACCEPT environment variable
-- * Evaluation strategies to control how values are forced (nf variants).
--
-- = Evaluation Strategies
--
-- Benchmarks require explicit evaluation strategies to prevent GHC from
-- optimizing away computations or sharing results across iterations:
--
-- * 'nf' - Force result to normal form (deep, full evaluation)
-- * 'nfIO' - Execute IO and force result to normal form
-- * 'nfAppIO' - Apply function, execute IO, force result to normal form
-- * 'io' - Plain IO without additional forcing
--
-- These are vendored from tasty-bench under the MIT license, (c) 2021 Andrew Lelechenko.

module Test.Hspec.BenchGolden.Runner
  ( -- * Running Benchmarks
    runBenchGolden
  , runBenchmark
  , runBenchmarkWithRawTimings

    -- * Parameter Sweeps
  , runSweepPoint
  , runSweep

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

    -- * Benchmarkable Constructors
  , io
  , nf
  , nfIO
  , nfAppIO
  ) where

import Control.DeepSeq (NFData, rnf)
import Control.Exception (evaluate)
import Control.Monad (when, replicateM_, forM)
import Data.Aeson (eitherDecodeFileStrict, encodeFile)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Data.List (sort)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Data.Vector.Unboxed as V
import Data.Word (Word64)
import qualified Statistics.Sample as Stats
import System.CPUTime (getCPUTime)
import System.Directory (createDirectoryIfMissing, doesFileExist)
import System.FilePath ((</>), (<.>))
import System.IO.Unsafe (unsafePerformIO)
import GHC.Exts (SPEC(..))

import Lens.Micro ((^.))

import Test.Hspec.BenchGolden.Arch (detectArchitecture, sanitizeForFilename)
import Test.Hspec.BenchGolden.CSV (writeSweepCSV)
import Test.Hspec.BenchGolden.Lenses (metricFor, varianceFor)
import Test.Hspec.BenchGolden.Types

-- | Benchmark a pure function applied to an argument, forcing the result to
-- normal form (NF) using 'rnf' from "Control.DeepSeq".
-- This ensures the entire result structure is evaluated.
--
-- Example:
--
-- @
-- benchGolden "fib 30" (nf fib 30)
-- @
nf :: NFData b => (a -> b) -> a -> BenchAction
nf = funcToBench rnf
{-# INLINE nf #-}

-- | Benchmark an 'IO' action, forcing the result to normal form.
--
-- Example:
--
-- @
-- benchGolden "readFile" (nfIO $ readFile "data.txt")
-- @
nfIO :: NFData a => IO a -> BenchAction
nfIO = ioToBench rnf
{-# INLINE nfIO #-}

-- | Benchmark a function that performs 'IO', forcing the result to normal form.
--
-- Example:
--
-- @
-- benchGolden "lookup in map" (nfAppIO lookupInDB "key")
-- @
nfAppIO :: NFData b => (a -> IO b) -> a -> BenchAction
nfAppIO = ioFuncToBench rnf
{-# INLINE nfAppIO #-}

-- | Benchmark an 'IO' action, discarding the result.
-- This is for backward compatibility with code that uses @IO ()@ actions.
--
-- Example:
-- 
-- @
-- benchGolden "compute" (io $ do
--   result <- heavyComputation
--   evaluate result)
-- @
io :: IO () -> BenchAction
io action = BenchAction (\n -> replicateM_ (fromIntegral n) action)
{-# INLINE io #-}

-- Internal helpers

funcToBench :: forall a b c. (b -> c) -> (a -> b) -> a -> BenchAction
funcToBench frc = (BenchAction .) . funcToBenchLoop SPEC
  where
    -- Here we rely on the fact that GHC (unless spurred by
    -- -fstatic-argument-transformation) is not smart enough:
    -- it does not notice that `f` and `x` arguments are loop invariant
    -- and could be floated, and the whole `f x` expression shared.
    -- If we create a closure with `f` and `x` bound in the environment,
    -- then GHC is smart enough to share computation of `f x`.
    funcToBenchLoop :: SPEC -> (a -> b) -> a -> Word64 -> IO ()
    funcToBenchLoop !_ f x n
      | n == 0    = pure ()
      | otherwise = do
        _ <- evaluate (frc (f x))
        funcToBenchLoop SPEC f x (n - 1)
{-# INLINE funcToBench #-}

ioToBench :: forall a b. (a -> b) -> IO a -> BenchAction
ioToBench frc act = BenchAction (ioToBenchLoop SPEC act)
  where
    ioToBenchLoop :: SPEC -> IO a -> Word64 -> IO ()
    ioToBenchLoop !_ action n
      | n == 0    = pure ()
      | otherwise = do
        x <- action
        _ <- evaluate (frc x)
        ioToBenchLoop SPEC action (n - 1)
{-# INLINE ioToBench #-}

ioFuncToBench :: forall a b c. (b -> c) -> (a -> IO b) -> a -> BenchAction
ioFuncToBench frc = (BenchAction .) . ioFuncToBenchLoop SPEC
  where
    ioFuncToBenchLoop :: SPEC -> (a -> IO b) -> a -> Word64 -> IO ()
    ioFuncToBenchLoop !_ f x n
      | n == 0    = pure ()
      | otherwise = do
        y <- f x
        _ <- evaluate (frc y)
        ioFuncToBenchLoop SPEC f x (n - 1)
{-# INLINE ioFuncToBench #-}

-- -----------------------------------------------------------------------------
-- Benchmark execution
-- -----------------------------------------------------------------------------


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
        runBenchAction benchAction (fromIntegral $ warmupIterations config)

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
--
-- Uses raw timing collection with proper inner iteration counts to ensure
-- the SPEC trick in nf/nfIO prevents thunk sharing.
runBenchmark :: String -> BenchAction -> BenchConfig -> ArchConfig -> IO GoldenStats
runBenchmark name action config arch =
  -- Always use the raw timing path since it correctly handles SPEC
  runBenchmarkWithRawTimings name action config arch

-- | Run a benchmark with raw timing collection for robust statistics.
--
-- This function times running all iterations in a single batch, then
-- divides to get per-iteration timing. The SPEC trick in nf/nfIO
-- prevents sharing within the batch.
--
-- We collect multiple samples by running the full batch multiple times,
-- ensuring accurate measurements even with GHC's -O2 optimizations.
runBenchmarkWithRawTimings :: String -> BenchAction -> BenchConfig -> ArchConfig -> IO GoldenStats
runBenchmarkWithRawTimings _name action config arch = do
  let iters = fromIntegral (iterations config) :: Word64
      numSamples = 10 :: Int  -- Number of timing samples to collect
  
  -- Collect raw CPU timings (each sample runs all iterations)
  rawTimings <- forM [1 .. numSamples] $ \_ -> do
    startCpu <- getCPUTime
    runBenchAction action iters  -- SPEC trick prevents sharing within this call
    endCpu <- getCPUTime
    pure $ picosToMillis (endCpu - startCpu) / fromIntegral iters
  
  let sortedTimings = sort rawTimings
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
      
      -- Percentiles
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
--
-- = Hybrid Tolerance Strategy
--
-- The comparison uses BOTH percentage and absolute tolerance (when configured):
--
-- 1. Calculate percentage difference: @((actual - golden) / golden) * 100@
--
-- 2. Pass if @abs(percentDiff) <= tolerancePercent@ (percentage check)
--
-- 3. OR if @abs(actual - golden) <= absoluteToleranceMs@ (absolute check)
--
-- This prevents false failures for sub-millisecond operations where measurement
-- noise creates large percentage variations despite negligible absolute differences.
compareStats :: BenchConfig -> GoldenStats -> GoldenStats -> BenchResult
compareStats config golden actual =
  let -- Use lens-based metric selection
      metric = metricFor config
      goldenValue = golden ^. metric
      actualValue = actual ^. metric

      -- Calculate percentage difference
      meanDiff = if goldenValue == 0
                 then if actualValue == 0 then 0 else 100
                 else ((actualValue - goldenValue) / goldenValue) * 100

      absDiff = abs meanDiff
      tolerance = tolerancePercent config

      -- Calculate absolute time difference (in milliseconds)
      absTimeDiff = abs (actualValue - goldenValue)

      -- Check if within absolute tolerance (hybrid tolerance strategy)
      withinAbsoluteTolerance = case absoluteToleranceMs config of
        Nothing -> False
        Just absThreshold -> absTimeDiff <= absThreshold

      -- Check variance if enabled
      baseWarnings = if warnOnVarianceChange config
                     then checkVariance config golden actual
                     else []
      
      -- Add outlier warnings if robust statistics enabled
      outlierWarnings = if useRobustStatistics config && not (null (statsOutliers actual))
                        then [OutliersDetected (length $ statsOutliers actual) (statsOutliers actual)]
                        else []
      
      warnings = baseWarnings ++ outlierWarnings

  in if absDiff <= tolerance || withinAbsoluteTolerance
     then Pass golden actual warnings
     else if meanDiff > 0
          then Regression golden actual meanDiff tolerance (absoluteToleranceMs config)
          else Improvement golden actual (abs meanDiff) tolerance (absoluteToleranceMs config)

-- | Check for variance changes and generate warnings.
checkVariance :: BenchConfig -> GoldenStats -> GoldenStats -> [Warning]
checkVariance config golden actual =
  let -- Use lens-based variance metric selection
      vLens = varianceFor config
      goldenVar = golden ^. vLens
      actualVar = actual ^. vLens

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

-- -----------------------------------------------------------------------------
-- Parameter Sweeps
-- -----------------------------------------------------------------------------

-- | Run a single point of a parameter sweep.
--
-- This is similar to 'runBenchGolden' but returns the 'GoldenStats' along
-- with the 'BenchResult', allowing the caller to accumulate stats for CSV export.
--
-- Each point is saved to its own golden file with the parameter value
-- included in the filename (e.g., @sort-scaling_n=1000.golden@).
runSweepPoint ::
     Show a
  => String       -- ^ Base sweep name
  -> BenchConfig
  -> T.Text       -- ^ Parameter name
  -> a            -- ^ Parameter value
  -> BenchAction
  -> IO (BenchResult, GoldenStats)
runSweepPoint sweepName config paramName paramValue action = do
  let pointName = sweepName ++ "_" ++ T.unpack paramName ++ "=" ++ show paramValue

  -- Run the benchmark (this writes golden/actual files via runBenchGolden logic)
  skip <- shouldSkipBenchmarks
  if skip
    then do
      now <- getCurrentTime
      arch <- detectArchitecture
      let dummyStats = GoldenStats 0 0 0 0 0 [] (archId arch) now 0 0 0 []
      return (Pass dummyStats dummyStats [], dummyStats)
    else do
      arch <- detectArchitecture
      let archDir = T.unpack $ archId arch

      -- Create output directory
      let dir = outputDir config </> archDir
      createDirectoryIfMissing True dir

      -- Run warm-up iterations
      when (warmupIterations config > 0) $
        runBenchAction action (fromIntegral $ warmupIterations config)

      -- Run the actual benchmark
      actualStats <- runBenchmark pointName action config arch

      -- Write actual results
      writeActualFile (outputDir config) archDir pointName actualStats

      -- Check if we should force update
      update <- shouldUpdateGolden

      -- Read or create golden file
      let goldenPath = getGoldenPath (outputDir config) archDir pointName
      goldenExists <- doesFileExist goldenPath

      if update || not goldenExists
        then do
          -- First run or forced update: create/update golden
          writeGoldenFile (outputDir config) archDir pointName actualStats
          return (FirstRun actualStats, actualStats)
        else do
          -- Compare against existing golden
          goldenResult <- readGoldenFile goldenPath
          case goldenResult of
            Left err -> error $ "Failed to read golden file: " ++ err
            Right goldenStats -> do
              let result = compareStats config goldenStats actualStats
              return (result, actualStats)

-- | Run a full parameter sweep and write CSV output.
--
-- This runs benchmarks for all parameter values, saves individual golden
-- files, and writes a single CSV file with all results for analysis.
--
-- The CSV file is placed at:
--
-- @
-- \<outputDir\>/\<sweep-name\>-\<arch-id\>.csv
-- @
runSweep ::
     Show a
  => String           -- ^ Sweep name
  -> BenchConfig
  -> SweepParam a     -- ^ Parameter definition
  -> (a -> BenchAction)  -- ^ Action generator
  -> IO [(a, BenchResult, GoldenStats)]
runSweep sweepName config SweepParam{..} mkAction = do
  arch <- detectArchitecture
  
  -- Run each parameter value
  results <- forM paramValues $ \paramVal -> do
    let action = mkAction paramVal
    (result, stats) <- runSweepPoint sweepName config paramName paramVal action
    return (paramVal, result, stats)
  
  -- Write CSV with all results
  let csvRows = [(T.pack (show pv), stats) | (pv, _, stats) <- results]
  writeSweepCSV (outputDir config) (archId arch) sweepName paramName csvRows
  
  return results
