-- | Example benchmark golden tests demonstrating golds-gym usage.
module Main (main) where

import Data.List (sort)
import Test.Hspec
import Test.Hspec.BenchGolden

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "List Operations" $ do
    -- Simple benchmark with default configuration using nf for normal form
    benchGoldenWith defaultBenchConfig { iterations = 1000 }
      "list append (1000 elements)" $
      nf (\xs -> xs ++ xs) [1..1000 :: Int]

    -- Benchmark with more iterations for stability
    benchGoldenWith defaultBenchConfig { iterations = 1000 }
      "list reverse (5000 elements)" $
      nf reverse [1..5000 :: Int]

  describe "Sorting Algorithms" $ do
    -- Benchmark with tighter tolerance for critical code
    benchGoldenWith defaultBenchConfig
      { iterations = 1000
      , tolerancePercent = 10.0
      , warmupIterations = 10
      }
      "sort 1000 elements" $
      nf sort [1000, 999..1 :: Int]

    -- Benchmark with robust statistics for operations prone to outliers
    benchGoldenWith defaultBenchConfig
      { useRobustStatistics = True
      , trimPercent = 10.0
      , outlierThreshold = 3.0
      , warnOnVarianceChange = False
      }
      "sort already sorted" $
      nf sort [1..1000 :: Int]

  describe "Numeric Operations" $ do
    benchGoldenWith defaultBenchConfig { iterations = 500 }
      "fibonacci 20" $
      nf fib 20

    benchGoldenWith defaultBenchConfig
      { iterations = 2000
      , tolerancePercent = 20.0  -- Higher tolerance for fast operations
      }
      "sum of list" $
      nf (\n -> sum [1..n]) (10000 :: Int)

  describe "Best Practices: Avoiding Shared Thunks" $ do
    -- Lambda wrapper forces list reconstruction on each iteration
    -- The list [1..n] is rebuilt for every benchmark iteration
    benchGoldenWith defaultBenchConfig { iterations = 1000 }
      "proper data structure reconstruction" $
      nf (\n -> sum [1..n]) (5000 :: Int)

    benchGoldenWith defaultBenchConfig { iterations = 1000 }
      "function application with parameter" $
      nf reverse [1..5000 :: Int]

    benchGoldenWith defaultBenchConfig { iterations = 1000 }
      "nested data structure reconstruction" $
      nf (\xs -> concat (replicate 100 xs)) [1..50 :: Int]

  describe "Robust Statistics Mode" $ do
    -- Benchmark using robust statistics (trimmed mean, MAD)
    benchGoldenWith defaultBenchConfig
      { useRobustStatistics = True
      , iterations = 500
      , trimPercent = 10.0      -- Trim 10% from each tail
      , outlierThreshold = 3.0  -- 3 MADs for outlier detection
      }
      "robust mode - list reverse" $
      nf reverse [1..5000 :: Int]

    -- Benchmark with robust statistics and high outlier sensitivity
    benchGoldenWith defaultBenchConfig
      { useRobustStatistics = True
      , iterations = 1000
      , trimPercent = 5.0       -- Minimal trimming
      , outlierThreshold = 2.5  -- More sensitive outlier detection
      , tolerancePercent = 10.0
      }
      "robust mode - sorting" $ do
      nf sort [100000, 99999..1 :: Int]

    -- Demonstrate outlier detection with intentionally noisy operation
    benchGoldenWith defaultBenchConfig
      { useRobustStatistics = True
      , iterations = 500
      , outlierThreshold = 2.0
      , tolerancePercent = 20.0
      }
      "robust mode - with potential outliers" $
      nf fib 25

  describe "Tolerance Configuration" $ do
    -- Hybrid tolerance (default): pass if within ±15% OR ±0.01ms
    benchGoldenWith defaultBenchConfig { iterations = 2000 }
      "hybrid tolerance - fast operation" $
      nf (\n -> sum [1..n]) (100 :: Int)

    -- Percentage-only tolerance: disable absolute tolerance
    -- Note: This test may fail occasionally due to measurement noise,
    -- demonstrating why hybrid tolerance is the recommended default
    benchGoldenWith defaultBenchConfig
      { absoluteToleranceMs = Nothing
      , tolerancePercent = 50.0  -- Increased to reduce false failures
      , iterations = 2000
      }
      "percentage-only tolerance" $
      nf (\n -> sum [1..n]) (500 :: Int)

    -- Strict absolute tolerance: 1 microsecond
    benchGoldenWith defaultBenchConfig
      { absoluteToleranceMs = Just 0.001  -- 1 microsecond
      , tolerancePercent = 10.0
      , iterations = 1000
      }
      "strict absolute tolerance" $
      nf fib 20


  describe "Lens-Based Expectations (Advanced)" $ do

    -- Hybrid tolerance with custom absolute threshold
    benchGoldenWithExpectation "hybrid tolerance custom" 
      defaultBenchConfig { iterations = 1000 }
      [expect _statsMean (Hybrid 5.0 0.001)] 
      $ nf (\n -> sort [n, n-1 .. 1]) (200 :: Int)

    -- Use robust statistics lens (trimmed mean)
    benchGoldenWithExpectation "trimmed mean comparison" 
      (defaultBenchConfig { useRobustStatistics = True, iterations = 1000 })
      [expect _statsTrimmedMean (Percent 25.0)]
      $ nf fib 26



-- | Naive Fibonacci for benchmarking purposes.
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
