# Copilot Instructions for golds-gym

## Project Overview

`golds-gym` is a Haskell golden testing framework for performance benchmarks, built on hspec and benchpress. It saves benchmark timing baselines to architecture-specific JSON files and compares subsequent runs against these baselines with configurable tolerance.

## Architecture

```
Test.Hspec.BenchGolden          -- Main API: benchGolden, benchGoldenWith combinators
├── Types.hs                    -- Core types: BenchConfig, GoldenStats, BenchResult
├── Arch.hs                     -- Architecture detection (CPU/OS/model → unique ID)
└── Runner.hs                   -- Benchmark execution, golden file I/O, comparison logic
```

**Data flow**: User calls `benchGolden "name" action` → Runner executes with benchpress → Stats saved to `.golden/<arch-id>/<name>.golden` (JSON) → Future runs compare against baseline using `tolerancePercent`.

## Build & Test Commands

```bash
make build          # Build with stack
make test           # Run benchmark tests
make docs           # Generate Haddock documentation
```

## Code Conventions

### Haskell Style
- Explicit export lists in all modules
- Haddock documentation for all public functions with `-- |` and usage examples
- `{-# LANGUAGE OverloadedStrings #-}` for Text literals

### Config Records Pattern
Provide `defaultXxxConfig` with sensible defaults, allow customization via record update:
```haskell
benchGoldenWith defaultBenchConfig
  { iterations = 500, tolerancePercent = 10.0 }
  "name" action
```

## Golden File Structure

Files stored at `.golden/<arch-id>/<test-name>.golden`:
```json
{
  "mean": 1.234, "stddev": 0.056, "median": 1.201,
  "min": 1.100, "max": 1.456,
  "architecture": "aarch64-darwin-Apple_M1",
  "timestamp": "2026-01-30T12:00:00Z"
}
```
Also writes `.actual` files for debugging failed comparisons.

## Testing

Example tests in [example/Spec.hs](example/Spec.hs) demonstrate:
- Basic `benchGolden` usage with pure computations
- Custom config with `benchGoldenWith`
- IO benchmarks with `benchGoldenIO`
- Using `evaluate` to force lazy thunks

## Key Dependencies

- **hspec-core** (2.10+): Test framework integration
- **benchpress** (0.2+): Timing measurement (`BP.benchmark`, `BP.mean`, etc.)
- **aeson**: JSON serialization of golden files
