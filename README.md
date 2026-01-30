# golds-gym 🏋️

[![CI](https://github.com/ocramz/golds-gym/actions/workflows/ci.yml/badge.svg)](https://github.com/ocramz/golds-gym/actions/workflows/ci.yml)

A Haskell golden testing framework for performance benchmarks.

## Overview

`golds-gym` allows you to define timing benchmarks that are saved to golden files the first time they run. On subsequent runs, new benchmark results are compared against the golden baselines using configurable tolerance thresholds.

**Key Features:**
- Architecture-specific golden files (different baselines per CPU/OS)
- Configurable tolerance for mean time comparison
- **Robust statistics** mode (trimmed mean, MAD, outlier detection)
- Optional variance (stddev) warnings
- Configurable warm-up iterations
- JSON-based golden files for easy inspection
- Seamless integration with hspec

## Installation

Add to your `.cabal` file:

```cabal
test-suite my-benchmarks
    build-depends:
        golds-gym,
        hspec
```

## Usage

```haskell
import Test.Hspec
import Test.Hspec.BenchGolden

main :: IO ()
main = hspec $ do
  describe "Performance" $ do
    -- Simple benchmark with defaults (100 iterations, 15% tolerance)
    benchGolden "list append" $
      return $ [1..1000] ++ [1..1000]

    -- Benchmark with custom configuration
    benchGoldenWith defaultBenchConfig
      { iterations = 500
      , tolerancePercent = 10.0
      , warmupIterations = 10
      , warnOnVarianceChange = True
      }
      "sorting" $
      return $ sort [1000, 999..1]

    -- Robust statistics mode (outlier detection, trimmed mean)
    benchGoldenWith defaultBenchConfig
      { useRobustStatistics = True
      , trimPercent = 10.0
      , outlierThreshold = 3.0
      , tolerancePercent = 10.0
      }
      "robust benchmark" $
      return $ expensiveComputation input

    -- IO benchmark
    benchGolden "file operations" $ do
      writeFile "/tmp/test" "hello"
      readFile "/tmp/test"
```

## Golden Files

Golden files are stored in `.golden/<architecture>/` with the following structure:

```
.golden/
├── aarch64-darwin-Apple_M1/
│   ├── list-append.golden
│   ├── list-append.actual
│   └── sorting.golden
└── x86_64-linux-Intel_Core_i7/
    └── list-append.golden
```

Each `.golden` file contains JSON with timing statistics:

```json
{
  "mean": 1.234,
  "stddev": 0.056,
  "median": 1.201,
  "min": 1.100,
  "max": 1.456,
  "percentiles": [[50, 1.201], [90, 1.350], [99, 1.440]],
  "architecture": "aarch64-darwin-Apple_M1",
  "timestamp": "2026-01-30T12:00:00Z",
  "trimmedMean": 1.220,
  "mad": 0.042,
  "iqr": 0.085,
  "outliers": [1.456]
}
```

## Updating Baselines

To regenerate golden files (after intentional performance changes):

```bash
GOLDS_GYM_ACCEPT=1 cabal test
# Or with stack:
GOLDS_GYM_ACCEPT=1 stack test
```

## Configuration

### BenchConfig Options

| Field | Default | Description |
|-------|---------|-------------|
| `iterations` | 100 | Number of benchmark iterations |
| `warmupIterations` | 5 | Warm-up runs (discarded) |
| `tolerancePercent` | 15.0 | Allowed mean time deviation (%) |
| `warnOnVarianceChange` | True | Warn if stddev changes significantly |
| `varianceTolerancePercent` | 50.0 | Allowed stddev deviation (%) |
| `outputDir` | ".golden" | Directory for golden files |
| `failOnFirstRun` | False | Fail if no baseline exists |
| `useRobustStatistics` | False | Use robust statistics (trimmed mean, MAD) |
| `trimPercent` | 10.0 | Percentage to trim from each tail (%) |
| `outlierThreshold` | 3.0 | MAD multiplier for outlier detection |

## Architecture Detection

The framework automatically detects:
- CPU architecture (x86_64, aarch64)
- Operating system (darwin, linux, windows)
- CPU model (Apple M1, Intel Core i7, etc.)

This ensures benchmarks are only compared against baselines from equivalent hardware.

## Robust Statistics

**New in 0.1.0**: Robust statistical methods for more reliable benchmark comparisons.

### Why Use Robust Statistics?

Standard mean and standard deviation are sensitive to outliers. A single anomalous timing (e.g., from GC, OS scheduling) can skew results. Robust statistics provide:

- **Trimmed Mean**: Removes extreme values before averaging
- **MAD (Median Absolute Deviation)**: Outlier-resistant measure of variance
- **Outlier Detection**: Identifies and reports anomalous timings
- **IQR (Interquartile Range)**: Spread of the middle 50% of data

### Enabling Robust Mode

```haskell
benchGoldenWith defaultBenchConfig
  { useRobustStatistics = True  -- Enable robust statistics
  , trimPercent = 10.0          -- Trim 10% from each tail
  , outlierThreshold = 3.0      -- Outliers are 3+ MADs from median
  , tolerancePercent = 10.0     -- Compare trimmed means
  }
  "my benchmark" $ do
  -- your code here
```

### How It Works

1. **Trimmed Mean**: Sorts all timing measurements, removes the top and bottom `trimPercent`, then computes the mean of remaining values.

2. **MAD Calculation**: Computes `median(|x - median(x)|)` - more robust than standard deviation.

3. **Outlier Detection**: Any measurement where `|x - median| > outlierThreshold * MAD` is flagged as an outlier.

4. **Comparison**: When enabled, uses trimmed mean instead of mean for regression detection, and MAD instead of stddev for variance checks.

### Outlier Warnings

When outliers are detected, you'll see warnings in test output:

```
Warnings:
  ⚠ 3 outlier(s) detected: 2.1ms 2.3ms 2.5ms
```

Outliers are reported but **not removed** - they're preserved in golden files for analysis.

### When to Use Robust Statistics

✅ **Use robust statistics when:**
- Benchmarking in noisy environments (shared CI runners)
- Operations subject to GC pauses
- Fast operations (< 1ms) with high relative variance
- You need more stable baselines

❌ **Standard statistics may be better when:**
- Benchmarking isolated, long-running operations
- You have dedicated benchmark hardware
- Outliers are legitimate and should be tracked

## Integration with CI

In CI environments, you may want to:


3. **Increase tolerance** (for noisy CI environments):
   ```haskell
   benchGoldenWith defaultBenchConfig { tolerancePercent = 25.0 } ...
   ```

## License

MIT
