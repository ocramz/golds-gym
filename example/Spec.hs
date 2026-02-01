-- | Example benchmark golden tests demonstrating golds-gym usage.
module Main (main) where

import Control.Exception (evaluate)
import Data.List (sort)
import Test.Hspec
import Test.Hspec.BenchGolden
import Test.Hspec.BenchGolden.Lenses

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "List Operations" $ do
    -- Simple benchmark with default configuration
    benchGolden "list append (1000 elements)" $ do
      _ <- evaluate $ [1..1000 :: Int] ++ [1..1000]
      return ()

    -- Benchmark with more iterations for stability
    benchGoldenWith defaultBenchConfig { iterations = 200 }
      "list reverse (5000 elements)" $ do
      _ <- evaluate $ reverse [1..5000 :: Int]
      return ()

  describe "Sorting Algorithms" $ do
    -- Benchmark with tighter tolerance for critical code
    benchGoldenWith defaultBenchConfig
      { iterations = 150
      , tolerancePercent = 10.0
      , warmupIterations = 10
      }
      "sort 1000 elements" $ do
      _ <- evaluate $ sort [1000, 999..1 :: Int]
      return ()

    -- Benchmark with robust statistics for operations prone to outliers
    benchGoldenWith defaultBenchConfig
      { useRobustStatistics = True
      , trimPercent = 10.0
      , outlierThreshold = 3.0
      , warnOnVarianceChange = False
      }
      "sort already sorted" $ do
      _ <- evaluate $ sort [1..1000 :: Int]
      return ()

  describe "Numeric Operations" $ do
    benchGolden "fibonacci 30" $ do
      _ <- evaluate $ fib 30
      return ()

    benchGoldenWith defaultBenchConfig
      { iterations = 500
      , tolerancePercent = 20.0  -- Higher tolerance for fast operations
      }
      "sum of list" $ do
      _ <- evaluate $ sum [1..10000 :: Int]
      return ()

  describe "Robust Statistics Mode" $ do
    -- Benchmark using robust statistics (trimmed mean, MAD)
    benchGoldenWith defaultBenchConfig
      { useRobustStatistics = True
      , iterations = 100
      , trimPercent = 10.0      -- Trim 10% from each tail
      , outlierThreshold = 3.0  -- 3 MADs for outlier detection
      }
      "robust mode - list reverse" $ do
      _ <- evaluate $ reverse [1..5000 :: Int]
      return ()

    -- Benchmark with robust statistics and high outlier sensitivity
    benchGoldenWith defaultBenchConfig
      { useRobustStatistics = True
      , iterations = 200
      , trimPercent = 5.0       -- Minimal trimming
      , outlierThreshold = 2.5  -- More sensitive outlier detection
      , tolerancePercent = 10.0
      }
      "robust mode - sorting" $ do
      _ <- evaluate $ sort [1000, 999..1 :: Int]
      return ()

    -- Demonstrate outlier detection with intentionally noisy operation
    benchGoldenWith defaultBenchConfig
      { useRobustStatistics = True
      , iterations = 50
      , outlierThreshold = 2.0
      }
      "robust mode - with potential outliers" $ do
      _ <- evaluate $ fib 25
      return ()

  describe "Tolerance Configuration" $ do
    -- Hybrid tolerance (default): pass if within ±15% OR ±0.01ms
    benchGolden "hybrid tolerance - fast operation" $ do
      _ <- evaluate $ sum [1..100 :: Int]
      return ()

    -- Percentage-only tolerance: disable absolute tolerance
    -- Note: This test may fail occasionally due to measurement noise,
    -- demonstrating why hybrid tolerance is the recommended default
    benchGoldenWith defaultBenchConfig
      { absoluteToleranceMs = Nothing
      , tolerancePercent = 30.0  -- Increased to reduce false failures
      }
      "percentage-only tolerance" $ do
      _ <- evaluate $ sum [1..500 :: Int]
      return ()

    -- Strict absolute tolerance: 1 microsecond
    benchGoldenWith defaultBenchConfig
      { absoluteToleranceMs = Just 0.001  -- 1 microsecond
      , tolerancePercent = 10.0
      }
      "strict absolute tolerance" $ do
      _ <- evaluate $ fib 20
      return ()

    -- Relaxed absolute tolerance for CI environments
    benchGoldenWith defaultBenchConfig
      { absoluteToleranceMs = Just 0.1  -- 100 microseconds
      , tolerancePercent = 25.0
      }
      "relaxed tolerance for CI" $ do
      _ <- evaluate $ reverse [1..1000 :: Int]
      return ()

  describe "Lens-Based Expectations (Advanced)" $ do
    -- Median-based comparison instead of mean
    benchGoldenWithExpectation "median-based comparison" defaultBenchConfig
      [expect _statsMedian (Percent 10.0)]
      $ do
        _ <- evaluate $ sort [1000, 999..1 :: Int]
        return ()

    -- Compose multiple expectations with AND
    benchGoldenWithExpectation "composed expectations (AND)" defaultBenchConfig
      [ expect _statsMean (Percent 15.0) &&~
        expect _statsMAD (Percent 50.0)
      ]
      $ do
        _ <- evaluate $ fib 25
        return ()

    -- Compose multiple expectations with OR
    benchGoldenWithExpectation "composed expectations (OR)" defaultBenchConfig
      [ expect _statsMedian (Percent 10.0) ||~
        expect _statsMin (Absolute 0.01)
      ]
      $ do
        _ <- evaluate $ sum [1..500 :: Int]
        return ()

    -- Expect reasonable performance (will pass with normal variance)
    benchGoldenWithExpectation "flexible tolerance" defaultBenchConfig
      [expect _statsMean (Percent 20.0)]  -- Wide tolerance for example
      $ do
        _ <- evaluate $ [1..1000 :: Int]  -- Trivial operation
        return ()

    -- Hybrid tolerance with custom absolute threshold
    benchGoldenWithExpectation "hybrid tolerance custom" defaultBenchConfig
      [expect _statsMean (Hybrid 20.0 0.005)]  -- ±20% OR ±5μs
      $ do
        _ <- evaluate $ sort [100, 99..1 :: Int]
        return ()

    -- Use robust statistics lens (trimmed mean)
    benchGoldenWithExpectation "trimmed mean comparison" 
      (defaultBenchConfig { useRobustStatistics = True })
      [expect _statsTrimmedMean (Percent 20.0)]
      $ do
        _ <- evaluate $ fib 26
        return ()

-- | Naive Fibonacci for benchmarking purposes.
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
