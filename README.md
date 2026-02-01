# golds-gym 🏋️

[![CI](https://github.com/ocramz/golds-gym/actions/workflows/ci.yml/badge.svg)](https://github.com/ocramz/golds-gym/actions/workflows/ci.yml) [![golds-gym](https://img.shields.io/hackage/v/golds-gym)](https://hackage.haskell.org/package/golds-gym)

Golden testing for performance benchmarks. Save timing baselines on first run, compare against them on subsequent runs.

**Key Features:**
- Architecture-specific baselines (different hardware = different golden files)
- Hybrid tolerance (handles both fast <1ms and slow operations)
- Robust statistics mode (outlier detection, trimmed mean)
- Lens-based custom expectations (assert "must be faster", compare by median, etc.)

## Quick Start

```haskell
import Test.Hspec
import Test.Hspec.BenchGolden
import Data.List (sort)

main :: IO ()
main = hspec $ do
  describe "Performance" $ do
    -- Pure function with normal form evaluation (deep, full evaluation)
    benchGolden "list append" $
      nf (\xs -> xs ++ xs) [1..1000]

    -- Weak head normal form (shallow, outermost constructor only)
    benchGolden "replicate" $
      whnf (replicate 1000) 42

    -- Custom configuration
    benchGoldenWith defaultBenchConfig
      { iterations = 500
      , tolerancePercent = 10.0
      }
      "sorting" $
      nf sort [1000, 999..1]
```

**Evaluation strategies** (required - specify how values are forced):
- `nf f x` - Force result of `f x` to **normal form** (deep, full evaluation)
- `whnf f x` - Force result of `f x` to **weak head normal form** (shallow, outermost constructor only)
- `nfIO action` - Execute IO action and force result to normal form
- `whnfIO action` - Execute IO action and force result to WHNF
- `nfAppIO f x` - Apply function, execute resulting IO, force result to normal form
- `whnfAppIO f x` - Apply function, execute resulting IO, force result to WHNF
- `io action` - Plain IO action without additional forcing

**Why evaluation strategies matter**: Without forcing, GHC may optimize away computations or share results across iterations, making benchmarks meaningless. Use `nf` for most cases unless you specifically want lazy evaluation (`whnf`).

**First run** creates `.golden/<arch>/list-append.golden` with baseline stats.  
**Subsequent runs** compare against baseline. Test fails if mean time changes beyond tolerance (default: ±15% OR ±0.01ms).

**Output format** shows baseline (expected) before actual (measured) for easy left-to-right reading:
```
Metric  Baseline    Actual      Diff
------  --------    ------      ----
Mean    0.150 ms  0.170 ms   +13.3%
```

**Update baselines** after intentional changes:
```bash
GOLDS_GYM_ACCEPT=1 stack test
```

## How It Works

Golden files store timing statistics per architecture (e.g., `.golden/aarch64-darwin-Apple_M1/`):

```json
{
  "mean": 1.234,
  "stddev": 0.056,
  "median": 1.201,
  "architecture": "aarch64-darwin-Apple_M1",
  "timestamp": "2026-01-30T12:00:00Z"
}
```

**Hybrid tolerance** (default) prevents false failures: benchmarks pass if within **±15% OR ±0.01ms**. This handles measurement noise for fast operations (<1ms) while catching real regressions for slower code.

## Configuration

Key `BenchConfig` options:

| Field | Default | Description |
|-------|---------|-------------|
| `iterations` | 100 | Number of benchmark iterations |
| `tolerancePercent` | 15.0 | Allowed mean time deviation (%) |
| `absoluteToleranceMs` | Just 0.01 | Absolute tolerance (ms) - enables hybrid mode |
| `useRobustStatistics` | False | Use trimmed mean/MAD instead of mean/stddev |
| `warmupIterations` | 5 | Warm-up runs before measurement |

See `BenchConfig` type for all options.

**Environment variables:**
- `GOLDS_GYM_ACCEPT=1` - Regenerate all golden files
- `GOLDS_GYM_SKIP=1` - Skip benchmarks entirely (useful in CI)

## Advanced: Robust Statistics

Standard mean/stddev are sensitive to outliers (GC pauses, OS scheduling). Robust statistics provide outlier-resistant comparisons:

```haskell
benchGoldenWith defaultBenchConfig
  { useRobustStatistics = True  -- Use trimmed mean + MAD
  , trimPercent = 10.0          -- Remove top/bottom 10%
  , outlierThreshold = 3.0      -- Flag outliers >3 MADs from median
  }
  "noisy benchmark" $
  nf computation input
```

**When to use:**
- Benchmarking in noisy environments (shared CI, development machines)
- Operations with occasional GC pauses or system interruptions
- Fast operations (<1ms) with high variance
- You see outliers in test output warnings

## Advanced: Lens-Based Expectations

For fine-grained control, use lens-based expectations to assert custom performance requirements:

```haskell
import Test.Hspec.BenchGolden.Lenses

-- Compare by median instead of mean (more robust)
benchGoldenWithExpectation "median comparison" defaultBenchConfig
  [expect _statsMedian (Percent 10.0)]
  (nf myAlgorithm input)

-- Compose multiple requirements (both must pass)
benchGoldenWithExpectation "strict requirements" defaultBenchConfig
  [ expect _statsMean (Percent 15.0) &&~
    expect _statsIQR (Absolute 0.1)     -- Low variance required
  ]
  (nf criticalFunction data)
```

**Available lenses:** `_statsMean`, `_statsMedian`, `_statsTrimmedMean`, `_statsStddev`, `_statsMAD`, `_statsIQR`, `_statsMin`, `_statsMax`

**Tolerance types:**
- `Percent 15.0` - Within ±15%
- `Absolute 0.01` - Within ±0.01ms
- `Hybrid 15.0 0.01` - Within ±15% OR ±0.01ms
- `MustImprove 10.0` - Must be ≥10% faster (for testing optimizations)
- `MustRegress 5.0` - Must be ≥5% slower (for accepting controlled regressions)

**Composition:** `(&&~)` for AND, `(||~)` for OR

## Documentation

- [API documentation](https://hackage.haskell.org/package/golds-gym) - Full Haddock docs
- [Example benchmarks](example/Spec.hs) - Comprehensive usage examples
- [CHANGELOG](CHANGELOG.md) - Version history and migration guides

## License

MIT
