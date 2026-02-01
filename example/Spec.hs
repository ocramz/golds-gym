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
      nf sum [1..10000 :: Int]

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
      nf sum [1..100 :: Int]

    -- Percentage-only tolerance: disable absolute tolerance
    -- Note: This test may fail occasionally due to measurement noise,
    -- demonstrating why hybrid tolerance is the recommended default
    benchGoldenWith defaultBenchConfig
      { absoluteToleranceMs = Nothing
      , tolerancePercent = 50.0  -- Increased to reduce false failures
      , iterations = 2000
      }
      "percentage-only tolerance" $
      nf sum [1..500 :: Int]

    -- Strict absolute tolerance: 1 microsecond
    benchGoldenWith defaultBenchConfig
      { absoluteToleranceMs = Just 0.001  -- 1 microsecond
      , tolerancePercent = 10.0
      , iterations = 1000
      }
      "strict absolute tolerance" $
      nf fib 20

    -- Relaxed absolute tolerance for CI environments
    benchGoldenWith defaultBenchConfig
      { absoluteToleranceMs = Just 0.1  -- 100 microseconds
      , tolerancePercent = 25.0
      , iterations = 1000
      }
      "relaxed tolerance for CI" $
      nf reverse [1..1000 :: Int]

  describe "Lens-Based Expectations (Advanced)" $ do
    -- -- Median-based comparison instead of mean
    -- benchGoldenWithExpectation "median-based comparison" 
    --   defaultBenchConfig { iterations = 1000 }
    --   [expect _statsMedian (Percent 20.0)]
    --   $ nf sort [1000, 999..1 :: Int]

    -- -- Compose multiple expectations with AND
    -- benchGoldenWithExpectation "composed expectations (AND)" 
    --   defaultBenchConfig { iterations = 1000 }
    --   [ expect _statsMean (Percent 20.0) &&~
    --     expect _statsMAD (Percent 100.0)
    --   ]
    --   $ nf fib 25

    -- -- Compose multiple expectations with OR
    -- benchGoldenWithExpectation "composed expectations (OR)" 
    --   defaultBenchConfig { iterations = 2000 }
    --   [ expect _statsMedian (Percent 20.0) ||~
    --     expect _statsMin (Absolute 0.01)
    --   ]
    --   $ nf sum [1..500 :: Int]

    -- -- Expect reasonable performance (will pass with normal variance)
    -- benchGoldenWithExpectation "flexible tolerance" 
    --   defaultBenchConfig { iterations = 2000 }
    --   [expect _statsMean (Hybrid 20.0 0.0001)]  -- Wide tolerance for example
    --   $ whnf (const [1..1000 :: Int]) ()  -- Using whnf for lazy evaluation

    -- Hybrid tolerance with custom absolute threshold
    benchGoldenWithExpectation "hybrid tolerance custom" 
      defaultBenchConfig { iterations = 1000 }
      [expect _statsMean (Hybrid 5.0 0.0001)]  -- ±25% OR ±.1μs
      $ nf sort [100, 99..1 :: Int]

    -- Use robust statistics lens (trimmed mean)
    benchGoldenWithExpectation "trimmed mean comparison" 
      (defaultBenchConfig { useRobustStatistics = True, iterations = 1000 })
      [expect _statsTrimmedMean (Percent 25.0)]
      $ nf fib 26

  -- describe "Different Evaluation Strategies" $ do
  --   -- Normal form: fully evaluates entire structure
  --   benchGoldenWith defaultBenchConfig { iterations = 1000 }
  --     "nf - deep evaluation" $
  --     nf (replicate 1000) (42 :: Int)

  --   -- Weak head normal form: evaluates only outermost constructor
  --   benchGoldenWith defaultBenchConfig { iterations = 1000 }
  --     "whnf - shallow evaluation" $
  --     whnf (replicate 1000) (42 :: Int)

  --   -- IO action with normal form result
  --   benchGoldenWith defaultBenchConfig { iterations = 1000 }
  --     "nfIO - IO with deep evaluation" $
  --     nfIO (return $ sum [1..1000 :: Int])

  --   -- IO action with weak head normal form result
  --   benchGoldenWith defaultBenchConfig { iterations = 1000 }
  --     "whnfIO - IO with shallow evaluation" $
  --     whnfIO (return [1..1000 :: Int])

  --   -- Function returning IO with normal form
  --   benchGoldenWith defaultBenchConfig { iterations = 1000 }
  --     "nfAppIO - function with IO and deep eval" $
  --     nfAppIO (\n -> return $ fib n) 20


-- | Naive Fibonacci for benchmarking purposes.
fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
