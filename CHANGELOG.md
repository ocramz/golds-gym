# Changelog

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
