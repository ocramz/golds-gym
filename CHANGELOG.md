# Changelog

## [Unreleased]

### Added

- **Enhanced architecture detection** to include RAM size and hardware thread count
  - Architecture identifiers now include RAM (e.g., "16GB") and hardware threads (e.g., "12cpus")
  - Example: `x86_64-linux-Intel_Core_i7_8700K-16GB-12cpus`
  - Prevents invalid benchmark comparisons between machines with different RAM or thread counts
  - Addresses concerns about coarse architecture identifiers (e.g., "x86_64-linux-Intel_Core_i7")
  - Ensures benchmarks are only compared on truly equivalent hardware
  - Thread count represents hardware threads (logical CPUs), not physical cores

### Changed

- **Breaking change**: All `ArchConfig` fields are now required (non-Maybe)
  - Renamed `archCPUCores` to `archThreadCount` to clarify it represents hardware threads
  - `archModel`, `archRAM`, and `archThreadCount` are now `!Text`, `!Text`, and `!Int` respectively
  - Fallback values used when detection fails: "unknown" for model/RAM, 1 for thread count
- `buildArchId` function now takes non-optional parameters
- Architecture detection uses safe parsing to avoid crashes from unexpected system command output
- JSON encoding of `ArchConfig` includes all fields (no optional fields)
- Architecture detection works on macOS (via `sysctl`), Linux (via `/proc/meminfo`, `nproc`), and Windows (via `wmic`)

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
