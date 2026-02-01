{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module      : Test.Hspec.BenchGolden.Lenses
-- Description : Lens-based expectation combinators for benchmark comparison
-- Copyright   : (c) 2026
-- License     : MIT
-- Maintainer  : your.email@example.com
--
-- This module provides van Laarhoven lenses for 'GoldenStats' fields and
-- expectation combinators for building custom performance assertions.
--
-- = Quick Start
--
-- @
-- import Test.Hspec
-- import Test.Hspec.BenchGolden
-- import Test.Hspec.BenchGolden.Lenses
--
-- main :: IO ()
-- main = hspec $ do
--   describe \"Custom Expectations\" $ do
--     -- Expect median within 10% tolerance
--     benchGoldenWithExpectation \"median-based\" defaultBenchConfig
--       [expect _statsMedian (Percent 10.0)]
--       myAction
--
--     -- Expect IQR within absolute 0.5ms
--     benchGoldenWithExpectation \"low variance\" defaultBenchConfig
--       [expect _statsIQR (Absolute 0.5)]
--       myAction
--
--     -- Compose multiple expectations (both must pass)
--     benchGoldenWithExpectation \"composed\" defaultBenchConfig
--       [ expect _statsMean (Percent 15.0) &&~
--         expect _statsMAD (Percent 50.0)
--       ]
--       myAction
-- @
--
-- = Lenses
--
-- Simple van Laarhoven lenses provide access to 'GoldenStats' fields:
--
-- * '_statsMean', '_statsMedian', '_statsTrimmedMean' - Central tendency metrics
-- * '_statsStddev', '_statsMAD', '_statsIQR' - Dispersion metrics  
-- * '_statsMin', '_statsMax' - Range metrics
--
-- = Smart Selectors
--
-- 'metricFor' and 'varianceFor' automatically select the appropriate lens
-- based on 'BenchConfig' settings:
--
-- @
-- let lens = metricFor config  -- Returns _statsTrimmedMean if useRobustStatistics
--     baseline = golden ^. lens
--     current = actual ^. lens
-- @
--
-- = Expectation Combinators
--
-- Build expectations with 'expect' and compose them:
--
-- * 'Percent' tolerance - e.g., @Percent 15.0@ for ±15%
-- * 'Absolute' tolerance - e.g., @Absolute 0.01@ for ±0.01ms
-- * 'Hybrid' tolerance - e.g., @Hybrid 15.0 0.01@ (pass if either satisfied)
--
-- Boolean composition operators:
--
-- * '(&&~)' - AND (both expectations must pass)
-- * '(||~)' - OR (either expectation can pass)
--
-- = Infix Operators
--
-- For concise tolerance checking:
--
-- * '(@~)' - Within percentage: @baseline \@~ 15.0 $ actual@
-- * '(@<)' - Within absolute: @baseline \@< 0.01 $ actual@
-- * '(@<<)' - Must be faster (negative tolerance): @baseline \@<< 5.0 $ actual@
-- * '(@>>)' - Must be slower (positive tolerance): @baseline \@>> 5.0 $ actual@

module Test.Hspec.BenchGolden.Lenses
  ( -- * Lenses for GoldenStats
    _statsMean
  , _statsStddev
  , _statsMedian
  , _statsMin
  , _statsMax
  , _statsTrimmedMean
  , _statsMAD
  , _statsIQR

    -- * Smart Metric Selectors
  , metricFor
  , varianceFor

    -- * Expectation Types
  , Expectation(..)
  , Tolerance(..)

    -- * Expectation Combinators
  , expect
  , expectStat
  , checkExpectation

    -- * Tolerance Checking Functions
  , withinPercent
  , withinAbsolute
  , withinHybrid
  , mustImprove
  , mustRegress

    -- * Infix Operators
  , (@~)
  , (@<)
  , (@<<)
  , (@>>)

    -- * Boolean Composition
  , (&&~)
  , (||~)

    -- * Utilities
  , percentDiff
  , absDiff
  ) where

import Lens.Micro
import Test.Hspec.BenchGolden.Types

-- -----------------------------------------------------------------------------
-- Lenses for GoldenStats fields
-- -----------------------------------------------------------------------------

-- | Lens for mean execution time in milliseconds.
_statsMean :: Lens' GoldenStats Double
_statsMean f s = fmap (\x -> s { statsMean = x }) (f (statsMean s))

-- | Lens for standard deviation in milliseconds.
_statsStddev :: Lens' GoldenStats Double
_statsStddev f s = fmap (\x -> s { statsStddev = x }) (f (statsStddev s))

-- | Lens for median execution time in milliseconds.
_statsMedian :: Lens' GoldenStats Double
_statsMedian f s = fmap (\x -> s { statsMedian = x }) (f (statsMedian s))

-- | Lens for minimum execution time in milliseconds.
_statsMin :: Lens' GoldenStats Double
_statsMin f s = fmap (\x -> s { statsMin = x }) (f (statsMin s))

-- | Lens for maximum execution time in milliseconds.
_statsMax :: Lens' GoldenStats Double
_statsMax f s = fmap (\x -> s { statsMax = x }) (f (statsMax s))

-- | Lens for trimmed mean (with tails removed) in milliseconds.
_statsTrimmedMean :: Lens' GoldenStats Double
_statsTrimmedMean f s = fmap (\x -> s { statsTrimmedMean = x }) (f (statsTrimmedMean s))

-- | Lens for median absolute deviation (MAD) in milliseconds.
_statsMAD :: Lens' GoldenStats Double
_statsMAD f s = fmap (\x -> s { statsMAD = x }) (f (statsMAD s))

-- | Lens for interquartile range (IQR = Q3 - Q1) in milliseconds.
_statsIQR :: Lens' GoldenStats Double
_statsIQR f s = fmap (\x -> s { statsIQR = x }) (f (statsIQR s))

-- -----------------------------------------------------------------------------
-- Smart Metric Selectors
-- -----------------------------------------------------------------------------

-- | Select the appropriate central tendency metric based on configuration.
--
-- Returns:
--
-- * '_statsTrimmedMean' if 'useRobustStatistics' is 'True'
-- * '_statsMean' otherwise
--
-- Example:
--
-- @
-- let lens = metricFor config
--     baseline = golden ^. lens
--     current = actual ^. lens
-- @
metricFor :: BenchConfig -> Lens' GoldenStats Double
metricFor cfg = if useRobustStatistics cfg 
                then _statsTrimmedMean 
                else _statsMean

-- | Select the appropriate dispersion metric based on configuration.
--
-- Returns:
--
-- * '_statsMAD' if 'useRobustStatistics' is 'True'
-- * '_statsStddev' otherwise
--
-- Example:
--
-- @
-- let vLens = varianceFor config
--     goldenVar = golden ^. vLens
--     actualVar = actual ^. vLens
-- @
varianceFor :: BenchConfig -> Lens' GoldenStats Double
varianceFor cfg = if useRobustStatistics cfg
                  then _statsMAD
                  else _statsStddev

-- -----------------------------------------------------------------------------
-- Expectation Types
-- -----------------------------------------------------------------------------

-- | Tolerance specification for performance comparison.
data Tolerance
  = Percent !Double
    -- ^ Percentage tolerance (e.g., @Percent 15.0@ = ±15%)
  | Absolute !Double
    -- ^ Absolute tolerance in milliseconds (e.g., @Absolute 0.01@ = ±0.01ms)
  | Hybrid !Double !Double
    -- ^ Hybrid tolerance: pass if EITHER percentage OR absolute is satisfied
    --   (e.g., @Hybrid 15.0 0.01@ = pass if within ±15% OR ±0.01ms)
  | MustImprove !Double
    -- ^ Must be faster by at least this percentage (e.g., @MustImprove 10.0@ = must be ≥10% faster)
  | MustRegress !Double
    -- ^ Must be slower by at least this percentage (e.g., @MustRegress 5.0@ = must be ≥5% slower)
  deriving (Show, Eq)

-- | An expectation for comparing golden and actual statistics.
--
-- Expectations can be composed using boolean operators:
--
-- @
-- expect _statsMean (Percent 15.0) &&~ expect _statsMAD (Percent 50.0)
-- @
data Expectation
  = ExpectStat !(Lens' GoldenStats Double) !Tolerance
    -- ^ Expect a specific field to be within tolerance
  | And !Expectation !Expectation
    -- ^ Both expectations must pass
  | Or !Expectation !Expectation
    -- ^ Either expectation can pass

-- Manual Eq instance (lenses can't be compared, so we only compare structure)
instance Eq Expectation where
  ExpectStat _ tol1 == ExpectStat _ tol2 = tol1 == tol2
  And e1 e2 == And e3 e4 = e1 == e3 && e2 == e4
  Or e1 e2 == Or e3 e4 = e1 == e3 && e2 == e4
  _ == _ = False

instance Show Expectation where
  show (ExpectStat _ tol) = "expect <field> " ++ show tol
  show (And e1 e2) = "(" ++ show e1 ++ " &&~ " ++ show e2 ++ ")"
  show (Or e1 e2) = "(" ++ show e1 ++ " ||~ " ++ show e2 ++ ")"

-- -----------------------------------------------------------------------------
-- Expectation Combinators
-- -----------------------------------------------------------------------------

-- | Create an expectation for a specific statistic field.
--
-- Example:
--
-- @
-- expect _statsMedian (Percent 10.0)
-- expect _statsIQR (Absolute 0.5)
-- expect _statsMean (Hybrid 15.0 0.01)
-- expect _statsMean (MustImprove 10.0)
-- @
expect :: Lens' GoldenStats Double -> Tolerance -> Expectation
expect = ExpectStat

-- | Create an expectation using a custom lens.
--
-- This is an alias for 'expect' for compatibility.
expectStat :: Lens' GoldenStats Double -> Tolerance -> Expectation
expectStat = expect

-- | Check if an expectation is satisfied for the given golden and actual stats.
--
-- Returns 'True' if the expectation passes, 'False' otherwise.
checkExpectation :: Expectation -> GoldenStats -> GoldenStats -> Bool
checkExpectation (ExpectStat lns tol) golden actual =
  let baseline = golden ^. lns
      current = actual ^. lns
  in checkTolerance tol baseline current
checkExpectation (And e1 e2) golden actual =
  checkExpectation e1 golden actual && checkExpectation e2 golden actual
checkExpectation (Or e1 e2) golden actual =
  checkExpectation e1 golden actual || checkExpectation e2 golden actual

-- | Check tolerance between baseline and current values.
checkTolerance :: Tolerance -> Double -> Double -> Bool
checkTolerance (Percent pct) baseline current =
  withinPercent pct baseline current
checkTolerance (Absolute absThreshold) baseline current =
  withinAbsolute absThreshold baseline current
checkTolerance (Hybrid pct absThreshold) baseline current =
  withinHybrid pct absThreshold baseline current
checkTolerance (MustImprove minPct) baseline current =
  mustImprove minPct baseline current
checkTolerance (MustRegress minPct) baseline current =
  mustRegress minPct baseline current

-- -----------------------------------------------------------------------------
-- Tolerance Checking Functions
-- -----------------------------------------------------------------------------

-- | Check if value is within percentage tolerance.
--
-- @
-- withinPercent 15.0 baseline actual  -- within ±15%
-- @
withinPercent :: Double -> Double -> Double -> Bool
withinPercent tolerance baseline actual =
  let pct = percentDiff baseline actual
  in abs pct <= tolerance

-- | Check if value is within absolute tolerance (milliseconds).
--
-- @
-- withinAbsolute 0.01 baseline actual  -- within ±0.01ms
-- @
withinAbsolute :: Double -> Double -> Double -> Bool
withinAbsolute threshold baseline actual =
  absDiff baseline actual <= threshold

-- | Check if value satisfies hybrid tolerance (percentage OR absolute).
--
-- @
-- withinHybrid 15.0 0.01 baseline actual  -- within ±15% OR ±0.01ms
-- @
withinHybrid :: Double -> Double -> Double -> Double -> Bool
withinHybrid pctTolerance absThreshold baseline actual =
  withinPercent pctTolerance baseline actual ||
  withinAbsolute absThreshold baseline actual

-- | Check if actual is faster than baseline by at least the given percentage.
--
-- @
-- mustImprove 10.0 baseline actual  -- must be ≥10% faster
-- @
mustImprove :: Double -> Double -> Double -> Bool
mustImprove minPercent baseline actual =
  let pct = percentDiff baseline actual
  in pct <= negate minPercent  -- Negative percentage = improvement

-- | Check if actual is slower than baseline by at least the given percentage.
--
-- @
-- mustRegress 5.0 baseline actual  -- must be ≥5% slower
-- @
mustRegress :: Double -> Double -> Double -> Bool
mustRegress minPercent baseline actual =
  let pct = percentDiff baseline actual
  in pct >= minPercent  -- Positive percentage = regression

-- -----------------------------------------------------------------------------
-- Infix Operators
-- -----------------------------------------------------------------------------

-- | Infix operator for percentage tolerance check.
--
-- @
-- baseline \@~ 15.0 $ actual  -- within ±15%
-- @
(@~) :: Double -> Double -> Double -> Bool
(@~) baseline tolerance actual = withinPercent tolerance baseline actual

infixl 4 @~

-- | Infix operator for absolute tolerance check.
--
-- @
-- baseline \@< 0.01 $ actual  -- within ±0.01ms
-- @
(@<) :: Double -> Double -> Double -> Bool
(@<) baseline threshold actual = withinAbsolute threshold baseline actual

infixl 4 @<

-- | Infix operator for "must improve" check.
--
-- @
-- baseline \@<< 10.0 $ actual  -- must be ≥10% faster
-- @
(@<<) :: Double -> Double -> Double -> Bool
(@<<) baseline minPercent actual = mustImprove minPercent baseline actual

infixl 4 @<<

-- | Infix operator for "must regress" check.
--
-- @
-- baseline \@>> 5.0 $ actual  -- must be ≥5% slower  
-- @
(@>>) :: Double -> Double -> Double -> Bool
(@>>) baseline minPercent actual = mustRegress minPercent baseline actual

infixl 4 @>>

-- -----------------------------------------------------------------------------
-- Boolean Composition
-- -----------------------------------------------------------------------------

-- | AND composition of expectations (both must pass).
--
-- @
-- expect _statsMean (Percent 15.0) &&~ expect _statsMAD (Percent 50.0)
-- @
(&&~) :: Expectation -> Expectation -> Expectation
(&&~) = And

infixr 3 &&~

-- | OR composition of expectations (either can pass).
--
-- @
-- expect _statsMedian (Percent 10.0) ||~ expect _statsMin (Absolute 0.01)
-- @
(||~) :: Expectation -> Expectation -> Expectation
(||~) = Or

infixr 2 ||~

-- -----------------------------------------------------------------------------
-- Utilities
-- -----------------------------------------------------------------------------

-- | Calculate percentage difference between baseline and actual.
--
-- Returns: @((actual - baseline) / baseline) * 100@
--
-- * Positive = regression (slower)
-- * Negative = improvement (faster)
-- * Zero = no change
percentDiff :: Double -> Double -> Double
percentDiff baseline actual
  | baseline == 0 = if actual == 0 then 0 else 100
  | otherwise = ((actual - baseline) / baseline) * 100

-- | Calculate absolute difference between baseline and actual.
--
-- Returns: @abs(actual - baseline)@
absDiff :: Double -> Double -> Double
absDiff baseline actual = abs (actual - baseline)
