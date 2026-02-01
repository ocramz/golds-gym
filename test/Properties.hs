{-# LANGUAGE ScopedTypeVariables #-}

-- | Property-based tests for statistical computations in Runner module
module Main (main) where

import Data.List (sort)
import qualified Data.Text as T
import qualified Data.Vector.Unboxed as V
import qualified Statistics.Sample as Stats
import Test.Hspec
import Test.QuickCheck

import Test.Hspec.BenchGolden.Runner
import Test.Hspec.BenchGolden.Types

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "calculateTrimmedMean" $ do
    it "returns value within min and max bounds for non-empty vectors" $ property $
      \(PositiveVector vec) (ValidTrimPercent pct) ->
        V.length vec > 0 ==>
        let result = calculateTrimmedMean pct vec
            minVal = V.minimum vec
            maxVal = V.maximum vec
        in result >= minVal && result <= maxVal

    it "converges to regular mean when trim percentage is zero" $ property $
      \(PositiveVector vec) ->
        not (V.null vec) ==>
        let trimmed = calculateTrimmedMean 0.0 vec
            regular = Stats.mean vec
        in abs (trimmed - regular) < 1e-10

    it "falls back to regular mean for trim percentage >= 50" $ property $
      \(PositiveVector vec) (Positive pct) ->
        V.length vec > 0 ==>
        let trimPct = 50.0 + pct
            trimmed = calculateTrimmedMean trimPct vec
            regular = Stats.mean vec
        in abs (trimmed - regular) < 1e-10

    it "returns zero for empty vector" $
      calculateTrimmedMean 10.0 V.empty `shouldBe` 0.0

    it "is less affected by outliers than regular mean" $ property $
      \(PositiveVector vec) ->
        V.length vec > 4 ==>
        let withOutlier = V.snoc vec (V.maximum vec * 100)  -- Add extreme outlier
            trimmed = calculateTrimmedMean 10.0 withOutlier
            regular = Stats.mean withOutlier
            originalMean = Stats.mean vec
        in abs (trimmed - originalMean) < abs (regular - originalMean)

  describe "calculateMAD" $ do
    it "is always non-negative" $ property $
      \(PositiveVector vec) ->
        let median' = if V.null vec then 0 else V.unsafeIndex vec (V.length vec `div` 2)
            mad = calculateMAD vec median'
        in mad >= 0

    it "returns zero for empty vector" $
      calculateMAD V.empty 0.0 `shouldBe` 0.0

    it "returns zero for constant data" $ property $
      \(Positive (constant :: Double)) (Positive (n :: Int)) ->
        n < 100 ==>
        let vec = V.replicate n constant
            median' = constant
            mad = calculateMAD vec median'
        in mad == 0.0

    it "scales linearly with data (scale invariance)" $ property $
      \(PositiveVector vec) (Positive scale) ->
        V.length vec > 0 ==>
        let median' = V.unsafeIndex vec (V.length vec `div` 2)
            mad1 = calculateMAD vec median'
            scaledVec = V.map (* scale) vec
            scaledMedian = median' * scale
            mad2 = calculateMAD scaledVec scaledMedian
        in abs (mad2 - mad1 * scale) < 1e-6

    it "is typically less than or equal to standard deviation" $ property $
      \(PositiveVector vec) ->
        V.length vec > 2 ==>
        let median' = V.unsafeIndex vec (V.length vec `div` 2)
            mad = calculateMAD vec median'
            stddev = Stats.stdDev vec
        in mad <= stddev * 1.5  -- Allow some tolerance for small samples

  describe "calculateIQR" $ do
    it "is always non-negative" $ property $
      \(PositiveVector vec) ->
        calculateIQR vec >= 0

    it "returns zero for vectors with less than 4 elements" $
      let vec1 = V.fromList [1.0]
          vec2 = V.fromList [1.0, 2.0]
          vec3 = V.fromList [1.0, 2.0, 3.0]
      in calculateIQR vec1 == 0.0 &&
         calculateIQR vec2 == 0.0 &&
         calculateIQR vec3 == 0.0

    it "returns zero for constant data" $ property $
      \(Positive (constant :: Double)) (Positive (n :: Int)) ->
        n < 100 && n >= 4 ==>
        let vec = V.replicate n constant
            iqr = calculateIQR vec
        in iqr == 0.0

    it "approximates 1.349 * stddev for normal-ish distributions" $ property $
      \(PositiveVector vec) ->
        V.length vec >= 20 ==>  -- Need sufficient sample size
        let iqr = calculateIQR vec
            stddev = Stats.stdDev vec
            ratio = if stddev == 0 then 0 else iqr / stddev
        in ratio >= 0.5 && ratio <= 2.5  -- Loose bounds for non-normal data

  describe "detectOutliers" $ do
    it "returns empty list for empty vector" $
      detectOutliers 3.0 V.empty 0.0 0.0 `shouldBe` []

    it "returns empty list when MAD is zero" $ property $
      \(Positive (constant :: Double)) (Positive (n :: Int)) (Positive thresh) ->
        n < 100 ==>
        let vec = V.replicate n constant
            outliers = detectOutliers thresh vec constant 0.0
        in null outliers

    it "fewer outliers with higher threshold (monotonicity)" $ property $
      \(PositiveVector vec) (Positive thresh1) (Positive thresh2) ->
        not (V.null vec) && thresh1 < thresh2 ==>
        let median' = V.unsafeIndex vec (V.length vec `div` 2)
            mad = calculateMAD vec median'
            outliers1 = detectOutliers thresh1 vec median' mad
            outliers2 = detectOutliers thresh2 vec median' mad
        in length outliers1 >= length outliers2

    it "all outliers satisfy distance criterion" $ property $
      \(PositiveVector vec) (Positive thresh) ->
        not (V.null vec) ==>
        let median' = V.unsafeIndex vec (V.length vec `div` 2)
            mad = calculateMAD vec median'
            outliers = detectOutliers thresh vec median' mad
        in all (\x -> abs (x - median') > thresh * mad || mad == 0) outliers

    it "all outliers are from original vector" $ property $
      \(PositiveVector vec) (Positive thresh) ->
        not (V.null vec) ==>
        let median' = V.unsafeIndex vec (V.length vec `div` 2)
            mad = calculateMAD vec median'
            outliers = detectOutliers thresh vec median' mad
            vecList = V.toList vec
        in all (`elem` vecList) outliers

  describe "calculateRobustStats" $ do
    it "returns consistent tuple structure" $ property $
      \(PositiveVector vec) ->
        let config = defaultBenchConfig { useRobustStatistics = True }
            median' = if V.null vec then 0 else V.unsafeIndex vec (V.length vec `div` 2)
            (tm, mad, iqr, outliers) = calculateRobustStats config vec median'
        in tm >= 0 && mad >= 0 && iqr >= 0 && length outliers >= 0

  describe "compareStats (tolerance logic)" $ do
    it "self-comparison always passes" $ property $
      \(SmallPositive mean') (SmallPositive stddev') ->
        let stats = makeGoldenStats mean' stddev'
            config = defaultBenchConfig { useRobustStatistics = False }
            result = compareStats config stats stats
        in case result of
             Pass _ _ _ -> True
             _          -> False

    it "passes if within percentage tolerance" $ property $
      \(SmallPositive golden) ->
        golden > 0.01 ==>  -- Avoid very small baselines
        let delta = golden * 0.08  -- 8% change, well within 15%
            goldenStats = makeGoldenStats golden 0.1
            actualStats = makeGoldenStats (golden + delta) 0.1
            config = defaultBenchConfig { tolerancePercent = 15.0, absoluteToleranceMs = Nothing }
            result = compareStats config goldenStats actualStats
        in case result of
             Pass _ _ _ -> True
             _          -> False

    it "passes if within absolute tolerance even with high percentage" $
      let golden = 0.002  -- 2 microseconds - very small
          actual = 0.004  -- 4 microseconds - 100% increase
          goldenStats = makeGoldenStats golden 0.0001
          actualStats = makeGoldenStats actual 0.0001
          config = defaultBenchConfig { absoluteToleranceMs = Just 0.01 }  -- 10 microseconds
          result = compareStats config goldenStats actualStats
      in case result of
           Pass _ _ _ -> True
           _          -> False

    it "regresses when exceeding both tolerances" $ property $
      \(SmallPositive golden) ->
        golden > 0.1 ==>  -- Large enough baseline
        let goldenStats = makeGoldenStats golden 0.01
            actualStats = makeGoldenStats (golden * 2) 0.01  -- 100% increase
            config = defaultBenchConfig { tolerancePercent = 10.0, absoluteToleranceMs = Just 0.01 }
            result = compareStats config goldenStats actualStats
        in case result of
             Regression _ _ _ _ _ -> True
             _                    -> False

    it "handles zero golden value gracefully" $
      let goldenStats = makeGoldenStats 0.0 0.0
          actualStats = makeGoldenStats 1.0 0.1
          config = defaultBenchConfig
          result = compareStats config goldenStats actualStats
      in case result of
           Regression _ _ pct _ _ -> pct == 100.0
           _                      -> False

  describe "checkVariance" $ do
    it "returns only variance warnings when enabled" $ property $
      \(SmallPositive m1) (SmallPositive m2) (SmallPositive s1) (SmallPositive s2) ->
        let config = defaultBenchConfig { warnOnVarianceChange = True, useRobustStatistics = False }
            golden = makeGoldenStats m1 s1
            actual = makeGoldenStats m2 s2
            warnings = checkVariance config golden actual
            -- Filter to only variance-related warnings (not outliers)
            varianceWarnings = filter isVarianceWarning warnings
        in varianceWarnings == warnings  -- All warnings should be variance-related

    it "detects high coefficient of variation" $ property $
      \(SmallPositive mean') ->
        mean' > 0.01 ==>
        let stddev' = mean' * 2  -- CV = 200%
            config = defaultBenchConfig { warnOnVarianceChange = True }
            stats = makeGoldenStats mean' stddev'
            warnings = checkVariance config stats stats
        in any isHighVarianceWarning warnings

    it "detects variance increase beyond tolerance" $ property $
      \(SmallPositive mean') (SmallPositive s1) ->
        s1 > 0 ==>
        let s2 = s1 * 3  -- 200% increase
            config = defaultBenchConfig { warnOnVarianceChange = True, varianceTolerancePercent = 50.0 }
            golden = makeGoldenStats mean' s1
            actual = makeGoldenStats mean' s2
            warnings = checkVariance config golden actual
        in any isVarianceIncreasedWarning warnings

-- -----------------------------------------------------------------------------
-- Custom Generators and Helper Types
-- -----------------------------------------------------------------------------

-- | Vector of positive doubles (simulating timing data)
newtype PositiveVector = PositiveVector (V.Vector Double)
  deriving (Show)

instance Arbitrary PositiveVector where
  arbitrary = do
    n <- choose (0, 50)  -- Reasonable vector size
    values <- vectorOf n (choose (0.001, 100.0))  -- 1μs to 100ms range
    return $ PositiveVector $ V.fromList $ sort values
  shrink (PositiveVector vec)
    | V.null vec = []
    | otherwise = [PositiveVector (V.take (V.length vec `div` 2) vec)]

-- | Valid trim percentage (0-50)
newtype ValidTrimPercent = ValidTrimPercent Double
  deriving (Show)

instance Arbitrary ValidTrimPercent where
  arbitrary = ValidTrimPercent <$> choose (0.0, 49.9)

-- | Small positive number for timing tests
newtype SmallPositive = SmallPositive Double
  deriving (Show)

instance Arbitrary SmallPositive where
  arbitrary = SmallPositive <$> choose (0.001, 10.0)

-- -----------------------------------------------------------------------------
-- Helper Functions
-- -----------------------------------------------------------------------------

-- | Create minimal GoldenStats for testing
makeGoldenStats :: Double -> Double -> GoldenStats
makeGoldenStats mean' stddev' = GoldenStats
  { statsMean = mean'
  , statsStddev = stddev'
  , statsMedian = mean'
  , statsMin = mean' - stddev'
  , statsMax = mean' + stddev'
  , statsPercentiles = []
  , statsArch = T.pack "test-arch"
  , statsTimestamp = read "2026-01-01 00:00:00 UTC"
  , statsTrimmedMean = mean'
  , statsMAD = stddev' * 0.7  -- Approximate relationship
  , statsIQR = stddev' * 1.35
  , statsOutliers = []
  }

-- | Check if warning is HighVariance
isHighVarianceWarning :: Warning -> Bool
isHighVarianceWarning (HighVariance _) = True
isHighVarianceWarning _ = False

-- | Check if warning is VarianceIncreased
isVarianceIncreasedWarning :: Warning -> Bool
isVarianceIncreasedWarning (VarianceIncreased _ _ _ _) = True
isVarianceIncreasedWarning _ = False

-- | Check if warning is any variance-related warning
isVarianceWarning :: Warning -> Bool
isVarianceWarning (VarianceIncreased _ _ _ _) = True
isVarianceWarning (VarianceDecreased _ _ _ _) = True
isVarianceWarning (HighVariance _) = True
isVarianceWarning _ = False
