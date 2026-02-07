{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

-- |
-- Module      : Test.Hspec.BenchGolden.CSV
-- Description : CSV export for parameter sweep benchmark results
-- Copyright   : (c) 2026
-- License     : MIT
-- Maintainer  : @ocramz
--
-- This module provides CSV export functionality for parameter sweep benchmarks.
-- CSV files are written alongside golden files for convenient analysis and plotting.
--
-- = CSV Format
--
-- The CSV includes columns for:
--
-- * @timestamp@ - When the benchmark was run (ISO 8601 format)
-- * @param_name@ - The name of the sweep parameter
-- * @param_value@ - The value of the parameter for this row
-- * @mean_ms@ - Mean execution time in milliseconds
-- * @stddev_ms@ - Standard deviation in milliseconds
-- * @median_ms@ - Median execution time in milliseconds
-- * @min_ms@ - Minimum execution time in milliseconds
-- * @max_ms@ - Maximum execution time in milliseconds
-- * @trimmed_mean_ms@ - Trimmed mean (if robust statistics enabled)
-- * @mad_ms@ - Median absolute deviation (if robust statistics enabled)
-- * @iqr_ms@ - Interquartile range (if robust statistics enabled)
--
-- = Usage
--
-- CSV files are automatically generated when using 'benchGoldenSweep' or
-- 'benchGoldenSweepWith'. The file is written to:
--
-- @
-- \<outputDir\>/\<sweep-name\>-\<arch-id\>.csv
-- @
--
-- For example: @.golden/sort-scaling-aarch64-darwin-Apple_M1.csv@

module Test.Hspec.BenchGolden.CSV
  ( -- * CSV Generation
    csvHeader
  , csvRow
  , buildCSV

    -- * File I/O
  , writeSweepCSV
  , getSweepCSVPath
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Lazy.Builder (Builder)
import qualified Data.Text.Lazy.Builder as B
import qualified Data.Text.Lazy.Builder.RealFloat as B
import qualified Data.Text.Lazy.IO as TL
import Data.Time.Format.ISO8601 (iso8601Show)
import System.Directory (createDirectoryIfMissing)
import System.FilePath ((</>), (<.>))

import Test.Hspec.BenchGolden.Arch (sanitizeForFilename)
import Test.Hspec.BenchGolden.Types (GoldenStats(..))

-- | Generate CSV header row.
--
-- The header includes all standard statistics columns plus a parameter column.
csvHeader :: Text -> Builder
csvHeader paramName = mconcat
  [ "timestamp,"
  , B.fromText paramName, ","
  , "mean_ms,stddev_ms,median_ms,min_ms,max_ms,"
  , "trimmed_mean_ms,mad_ms,iqr_ms\n"
  ]

-- | Generate a single CSV data row from benchmark stats.
--
-- The timestamp is taken from the 'GoldenStats', and the parameter value
-- is provided separately as a string representation.
csvRow :: Text -> GoldenStats -> Builder
csvRow paramValue GoldenStats{..} = mconcat
  [ B.fromString (iso8601Show statsTimestamp), ","
  , B.fromText paramValue, ","
  , B.realFloat statsMean, ","
  , B.realFloat statsStddev, ","
  , B.realFloat statsMedian, ","
  , B.realFloat statsMin, ","
  , B.realFloat statsMax, ","
  , B.realFloat statsTrimmedMean, ","
  , B.realFloat statsMAD, ","
  , B.realFloat statsIQR, "\n"
  ]

-- | Build complete CSV content from a list of (param value, stats) pairs.
--
-- Example:
--
-- @
-- let results = [("1000", stats1), ("5000", stats2), ("10000", stats3)]
-- let csv = buildCSV "n" results
-- @
buildCSV :: Text -> [(Text, GoldenStats)] -> Builder
buildCSV paramName rows =
  csvHeader paramName <> mconcat (map (uncurry csvRow) rows)

-- | Get the path for a sweep CSV file.
--
-- The file is placed in the output directory with the architecture ID
-- included in the filename (not as a subdirectory) for easy comparison
-- across architectures.
--
-- Example: @.golden/sort-scaling-aarch64-darwin-Apple_M1.csv@
getSweepCSVPath :: FilePath -> Text -> String -> FilePath
getSweepCSVPath outDir archId sweepName =
  let sanitizedName = T.unpack $ sanitizeForFilename (T.pack sweepName)
      sanitizedArch = T.unpack $ sanitizeForFilename archId
  in outDir </> (sanitizedName <> "-" <> sanitizedArch) <.> "csv"

-- | Write sweep results to a CSV file.
--
-- This creates the output directory if it doesn't exist and writes
-- the complete CSV content atomically.
writeSweepCSV :: FilePath -> Text -> String -> Text -> [(Text, GoldenStats)] -> IO ()
writeSweepCSV outDir archId sweepName paramName rows = do
  createDirectoryIfMissing True outDir
  let path = getSweepCSVPath outDir archId sweepName
      content = B.toLazyText $ buildCSV paramName rows
  TL.writeFile path content
