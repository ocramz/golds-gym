-- | Example benchmark golden tests demonstrating golds-gym usage.
module Main (main) where

import Control.Exception (evaluate)
import Data.List (sort)
import Test.Hspec
import Test.Hspec.BenchGolden

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

    -- Benchmark with variance warnings disabled
    benchGoldenWith defaultBenchConfig
      { warnOnVarianceChange = False
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

  describe "IO Operations" $ do
    benchGoldenIO "evaluate lazy thunk" $ do
      let bigList = [1..100000 :: Int]
      _ <- evaluate (length bigList)
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

-- | Naive Fibonacci for benchmarking purposes.
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
