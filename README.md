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
├── aarch64-darwin-Apple_M1-16GB-8cpus/
│   ├── list-append.golden
│   ├── list-append.actual
│   └── sorting.golden
└── x86_64-linux-Intel_Core_i7_8700K-16GB-12cpus/
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
  "architecture": "aarch64-darwin-Apple_M1-16GB-8cpus",
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
| `absoluteToleranceMs` | Just 0.01 | Minimum absolute tolerance in milliseconds (hybrid tolerance) |
| `warnOnVarianceChange` | True | Warn if stddev changes significantly |
| `varianceTolerancePercent` | 50.0 | Allowed stddev deviation (%) |
| `outputDir` | ".golden" | Directory for golden files |
| `failOnFirstRun` | False | Fail if no baseline exists |
| `useRobustStatistics` | False | Use robust statistics (trimmed mean, MAD) |
| `trimPercent` | 10.0 | Percentage to trim from each tail (%) |
| `outlierThreshold` | 3.0 | MAD multiplier for outlier detection |

### Hybrid Tolerance Strategy

**New in v0.2.0**: Hybrid tolerance prevents false failures from measurement noise.

The framework uses BOTH percentage and absolute tolerance by default:

```
Benchmark passes if:
  (mean_change <= ±15%) OR (abs_time_diff <= 0.01ms)
```

#### Why Hybrid Tolerance?

For extremely fast operations (< 1ms), tiny measurement noise causes huge percentage variations:

- **Baseline**: 0.001 ms
- **Actual**: 0.0015 ms  
- **Percentage difference**: +50% ❌ (fails with 15% tolerance)
- **Absolute difference**: +0.0005 ms ✅ (negligible, within 0.01ms tolerance)

The hybrid approach automatically handles this:

- **Fast operations (< 1ms)**: Absolute tolerance dominates → noise ignored
- **Slow operations (> 1ms)**: Percentage tolerance dominates → regressions caught

#### Configuration Examples

**Default (hybrid tolerance)**:
```haskell
benchGolden "fast operation" $ do
  return $ sum [1..100]
```
Passes if within ±15% **or** ±0.01ms (10 microseconds).

**Percentage-only (disable absolute tolerance)**:
```haskell
benchGoldenWith defaultBenchConfig
  { absoluteToleranceMs = Nothing
  , tolerancePercent = 20.0
  }
  "long operation" $ do
  return $ expensiveComputation input
```
Traditional percentage-only comparison.

**Strict absolute tolerance**:
```haskell
benchGoldenWith defaultBenchConfig
  { absoluteToleranceMs = Just 0.001  -- 1 microsecond
  , tolerancePercent = 10.0
  }
  "performance-critical" $ do
  return $ criticalPath data
```
Very strict for performance-critical code.

**Relaxed tolerance for noisy CI**:
```haskell
benchGoldenWith defaultBenchConfig
  { absoluteToleranceMs = Just 0.1  -- 100 microseconds
  , tolerancePercent = 25.0
  }
  "ci benchmark" $ do
  return $ computation input
```
More forgiving for shared CI runners.

## Architecture Detection

The framework automatically detects:
- CPU architecture (x86_64, aarch64)
- Operating system (darwin, linux, windows)
- CPU model (Apple M1, Intel Core i7, etc.)
- RAM size (16GB, 32GB, etc.)
- Hardware thread count (logical CPUs)

This ensures benchmarks are only compared against baselines from equivalent hardware.

**Example architecture identifier**: `x86_64-linux-Intel_Core_i7_8700K-16GB-12cpus`

This detailed identification prevents benchmark comparisons between machines with different:
- CPU models (e.g., different Core i7 generations)
- RAM configurations (which affect caching behavior)
- Hardware thread counts (affecting parallel workload performance)

Note: The thread count represents hardware threads (logical CPUs), not physical cores, as this is what affects benchmark performance for parallel workloads.

**Fallback values**: If system detection fails, the following defaults are used:
- CPU model: "unknown"
- RAM size: "unknown"
- Thread count: 1

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
- Operations subject to GC pauses or OS scheduling variability
- Fast operations (< 1ms) with high relative variance (CV > 50%)
- Sorting already-sorted data or other operations with occasional slowdowns
- You see large max/stddev values with small mean times
- You need more stable baselines across runs

❌ **Standard statistics may be better when:**
- Benchmarking isolated, long-running operations
- You have dedicated benchmark hardware
- Outliers are legitimate and should be tracked

## Integration with CI

In CI environments, you may want to:

1. **Skip benchmarks** (if CI is too noisy):
   ```bash
   GOLDS_GYM_SKIP=1 cabal test
   ```

2. **Use relaxed tolerance** (for shared CI runners):
   ```haskell
   benchGoldenWith defaultBenchConfig
     { tolerancePercent = 25.0
     , absoluteToleranceMs = Just 0.1  -- 100 microseconds
     }
     "benchmark" $ ...
   ```

3. **Enable robust statistics** (outlier detection):
   ```haskell
   benchGoldenWith defaultBenchConfig
     { useRobustStatistics = True
     , tolerancePercent = 20.0
     }
     "benchmark" $ ...
   ```

## Troubleshooting

### Random Test Failures Due to Measurement Noise

**Symptom**: Tests fail intermittently with small percentage increases despite negligible absolute time differences:

```
Mean time increased by 35.5% (tolerance: 15.0%)

Metric    Actual  Baseline    Diff
------    ------  --------    ----
Mean    0.001 ms  0.000 ms  +35.5%
```

**Root Cause**: Operations taking < 1ms have high relative measurement noise. A 0.0005ms difference is negligible but represents 50% variation.

**Solutions**:

1. **Use hybrid tolerance (default since v0.2.0)**:
   ```haskell
   benchGolden "fast operation" $ ...
   ```
   The default `absoluteToleranceMs = Just 0.01` prevents these failures.

2. **Adjust absolute tolerance threshold**:
   ```haskell
   benchGoldenWith defaultBenchConfig
     { absoluteToleranceMs = Just 0.001  -- Stricter: 1 microsecond
     }
     "very fast operation" $ ...
   ```

3. **Increase iterations for stability**:
   ```haskell
   benchGoldenWith defaultBenchConfig
     { iterations = 500  -- More samples reduce noise
     }
     "noisy operation" $ ...
   ```

4. **Use robust statistics**:
   ```haskell
   benchGoldenWith defaultBenchConfig
     { useRobustStatistics = True  -- Outlier-resistant
     , trimPercent = 10.0
     }
     "operation with outliers" $ ...
   ```

### High Variance Warnings

**Symptom**: Warnings about variance changes despite passing benchmarks:

```
Warnings:
  ⚠ Variance increased by 65.2% (0.001 ms -> 0.002 ms, tolerance: 50.0%)
```

**Solutions**:

1. **Disable variance warnings** (if not critical):
   ```haskell
   benchGoldenWith defaultBenchConfig
     { warnOnVarianceChange = False
     }
     "benchmark" $ ...
   ```

2. **Increase variance tolerance**:
   ```haskell
   benchGoldenWith defaultBenchConfig
     { varianceTolerancePercent = 100.0  -- Allow ±100% stddev change
     }
     "benchmark" $ ...
   ```

3. **Use robust statistics** (MAD instead of stddev):
   ```haskell
   benchGoldenWith defaultBenchConfig
     { useRobustStatistics = True  -- Uses MAD, more stable
     }
     "benchmark" $ ...
   ```

### Outlier Warnings

**Symptom**: Outliers detected in benchmark runs:

```
Warnings:
  ⚠ 3 outlier(s) detected: 2.1ms 2.3ms 2.5ms
```

**Causes**:
- Garbage collection pauses
- OS scheduling interruptions
- CPU thermal throttling
- Background processes

**Solutions**:

1. **Increase outlier threshold** (less sensitive):
   ```haskell
   benchGoldenWith defaultBenchConfig
     { useRobustStatistics = True
     , outlierThreshold = 5.0  -- More forgiving (default: 3.0)
     }
     "benchmark" $ ...
   ```

2. **Increase warm-up iterations**:
   ```haskell
   benchGoldenWith defaultBenchConfig
     { warmupIterations = 20  -- Stabilize before measurement
     }
     "benchmark" $ ...
   ```

3. **Minimize system load**:
   - Close background applications
   - Disable system services during benchmarking
   - Use dedicated benchmark hardware

### Benchmarks Pass Locally But Fail in CI

**Cause**: Different architecture or noisier environment.

**Solutions**:

1. **Architecture-specific baselines**: Golden files are already per-architecture. Check that your CI architecture ID matches:
   ```bash
   GOLDS_GYM_ARCH=custom-ci-id cabal test
   ```

2. **Relaxed CI configuration**:
   ```haskell
   #ifdef CI_BUILD
   ciConfig :: BenchConfig
   ciConfig = defaultBenchConfig
     { tolerancePercent = 30.0
     , absoluteToleranceMs = Just 0.2
     , useRobustStatistics = True
     }
   #endif
   ```

3. **Skip benchmarks in CI**:
   ```yaml
   # .github/workflows/ci.yml
   - name: Run tests
     run: GOLDS_GYM_SKIP=1 stack test
   ```

### Regenerating Golden Files

**When to regenerate**:
- Intentional performance improvements/changes
- Compiler upgrades affecting code generation
- Architecture changes

**How**:
```bash
GOLDS_GYM_ACCEPT=1 stack test
```

**Warning**: Only regenerate when you've verified the performance change is expected!

## License

MIT
