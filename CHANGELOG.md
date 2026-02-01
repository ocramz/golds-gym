# Changelog

## [0.4.0]

### Added

- **QuickCheck property tests** for statistical computations in Runner module
  - 28 property tests covering mathematical invariants and edge cases
  - Tests for `calculateTrimmedMean`, `calculateMAD`, `calculateIQR`, `detectOutliers`
  - Tests for `compareStats` tolerance logic (hybrid percentage/absolute)
  - Tests for `checkVariance` warning generation
  - Custom generators for valid timing data and benchmark configurations
  - New test suite: `golds-gym-properties`

- **Evaluation strategy combinators** for proper laziness handling
  - `nf` - Force result to normal form (deep evaluation)
  - `whnf` - Force result to weak head normal form (shallow evaluation)
  - `nfIO` - Normal form for IO actions
  - `whnfIO` - Weak head normal form for IO actions
  - `nfAppIO` - Normal form for functions returning IO
  - `whnfAppIO` - WHNF for functions returning IO
  - `io` - Plain IO action (for backward compatibility)
  - Vendored evaluation loops from tasty-bench with attribution

- **`BenchAction` type** wrapping `Word64 -> IO ()` for benchmarkable actions
  - Enables direct benchmarking of pure functions without manual `evaluate` calls

### Changed

- **BREAKING:** All benchmark API functions now accept `BenchAction` instead of `IO ()`
  - `benchGolden :: String -> BenchAction -> Spec`
  - `benchGoldenWith :: BenchConfig -> String -> BenchAction -> Spec`
  - `benchGoldenWithExpectation :: String -> BenchConfig -> [Expectation] -> BenchAction -> Spec`
  - Migration: wrap existing `IO ()` actions with `io` combinator
  - New: use `nf`/`whnf` for pure functions instead of manual `evaluate`

- **Output format:** Baseline now appears before Actual in comparison tables for easier left-to-right reading

- **Error messages:** Tolerance values in failure messages now extracted from expectations rather than config defaults

### Fixed

- Evaluation strategy bug where GHC could share computation across benchmark iterations
- Error message wording for performance improvements (now says "decreased by" instead of "increased by -X%")
- Flaky micro-benchmarks stabilized by increasing iteration counts (500-2000 iterations)

### Dependencies

- Added `deepseq >= 1.4 && < 2` for `NFData` constraint
- Added `QuickCheck >= 2.14 && < 3` for property tests (test suite only)

## [0.3.0]

### Added

- **Lens-based expectation combinators** for custom performance assertions
  - New `Test.Hspec.BenchGolden.Lenses` module with van Laarhoven lenses for `GoldenStats` fields
  - Lenses: `_statsMean`, `_statsMedian`, `_statsTrimmedMean`, `_statsStddev`, `_statsMAD`, `_statsIQR`, `_statsMin`, `_statsMax`
  - Smart metric selectors: `metricFor` and `varianceFor` automatically choose appropriate lens based on `BenchConfig`
  - `Expectation` type for composable performance assertions
  - `Tolerance` variants: `Percent`, `Absolute`, `Hybrid`, `MustImprove`, `MustRegress`
  - Boolean composition operators: `(&&~)` for AND, `(||~)` for OR
  - Infix operators: `(@~)` for percentage, `(@<)` for absolute, `(@<<)` for must-improve, `(@>>)` for must-regress
  - `benchGoldenWithExpectation` combinator for custom lens-based expectations
  - Enables assertions like "must be 10% faster", "median within 10%", "IQR < 0.1ms"

- **Tolerance helper functions** for manual comparison
  - `withinPercent`, `withinAbsolute`, `withinHybrid` for tolerance checking
  - `mustImprove`, `mustRegress` for directional performance expectations
  - `percentDiff`, `absDiff` utilities

### Changed

- **Refactored comparison logic to use lenses internally** (non-breaking)
  - `compareStats` now uses `metricFor` instead of if/else branching
  - `checkVariance` now uses `varianceFor` for cleaner metric selection

### Dependencies

- Added `microlens >= 0.4 && < 0.6`

## [0.2.0] - 2026-01-30

### Added

- **Hybrid tolerance mechanism** to prevent false failures from measurement noise
  - New `absoluteToleranceMs` field in `BenchConfig` (default: `Just 0.01` ms)
  - Benchmarks now pass if EITHER percentage tolerance OR absolute tolerance is satisfied
  - Eliminates random failures for sub-millisecond operations where measurement noise causes large percentage variations despite negligible absolute differences
  
- **Enhanced failure messages** showing both tolerance thresholds
  - Regression/improvement messages now display: `"tolerance: 15.0% or 0.010 ms"` when absolute tolerance is configured
  
- **Example benchmarks** demonstrating tolerance configurations
  - Hybrid tolerance (default)
  - Percentage-only tolerance
  - Strict absolute tolerance
  - Relaxed tolerance for CI environments

### Changed

- Default `BenchConfig` now includes `absoluteToleranceMs = Just 0.01` (10 microseconds)
- `BenchResult` constructors (`Regression`, `Improvement`) now include `Maybe Double` for absolute tolerance
- Updated "sort already sorted" example to use robust statistics to handle outliers
- Increased tolerance for percentage-only example to 30% to reduce false failures

### Fixed

- Random benchmark failures for fast operations (< 1ms) due to measurement noise
- False regressions when absolute time differences are negligible but percentage variations are large
- Inconsistent test results across runs for sub-millisecond operations

## [0.1.0] - 2026-01-30

### Added

- Initial release of golds-gym
- Golden testing framework for performance benchmarks
- Architecture-specific golden files
- Integration with hspec and benchpress
- Configurable tolerance for mean time comparison
- Robust statistics mode (trimmed mean, MAD, outlier detection)
- Variance warnings
- Environment variables for accepting/skipping benchmarks
- Support for both standard and robust statistical methods
