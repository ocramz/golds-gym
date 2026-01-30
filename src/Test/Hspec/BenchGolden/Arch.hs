{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      : Test.Hspec.BenchGolden.Arch
-- Description : Architecture detection for machine-specific golden files
-- Copyright   : (c) 2026
-- License     : MIT
-- Maintainer  : your.email@example.com
--
-- This module provides functions for detecting the current machine's
-- architecture, which is used to create architecture-specific golden files.
--
-- The architecture identifier includes:
--
-- * CPU architecture (x86_64, aarch64, etc.)
-- * Operating system (darwin, linux, windows)
-- * CPU model when available (Apple M1, Intel Core i7, etc.)

module Test.Hspec.BenchGolden.Arch
  ( -- * Architecture Detection
    detectArchitecture
  , getArchId

    -- * Environment Overrides
  , getArchFromEnv

    -- * Utilities
  , sanitizeForFilename
  ) where

import Control.Exception (catch, SomeException)
import Data.Char (isAlphaNum)
import Data.Text (Text)
import qualified Data.Text as T
import System.Environment (lookupEnv)
import System.Info (arch, os)
import System.Process (readProcess)

import Test.Hspec.BenchGolden.Types (ArchConfig(..))

-- | Detect the current machine's architecture.
--
-- This function queries the system for CPU architecture, OS, and CPU model.
-- The resulting 'ArchConfig' can be used to generate architecture-specific
-- golden file paths.
--
-- The architecture can be overridden by setting the @GOLDS_GYM_ARCH@
-- environment variable.
detectArchitecture :: IO ArchConfig
detectArchitecture = do
  envArch <- getArchFromEnv
  case envArch of
    Just customArch -> return $ ArchConfig
      { archId    = customArch
      , archOS    = T.pack os
      , archCPU   = T.pack arch
      , archModel = Just customArch
      }
    Nothing -> do
      model <- getCPUModel
      let archConfig = ArchConfig
            { archId    = buildArchId (T.pack arch) (T.pack os) model
            , archOS    = T.pack os
            , archCPU   = T.pack arch
            , archModel = model
            }
      return archConfig

-- | Build an architecture identifier from components.
buildArchId :: Text -> Text -> Maybe Text -> Text
buildArchId cpu osName maybeModel =
  let base = cpu <> "-" <> osName
  in case maybeModel of
       Nothing    -> base
       Just model -> base <> "-" <> sanitizeForFilename model

-- | Get the architecture identifier string.
--
-- This is a convenience function that returns just the ID string
-- suitable for use in file paths.
getArchId :: IO Text
getArchId = archId <$> detectArchitecture

-- | Check for architecture override from environment.
--
-- Users can set @GOLDS_GYM_ARCH@ to force a specific architecture
-- identifier, useful for CI environments with consistent hardware.
getArchFromEnv :: IO (Maybe Text)
getArchFromEnv = fmap T.pack <$> lookupEnv "GOLDS_GYM_ARCH"

-- | Get the CPU model name.
--
-- This is platform-specific:
--
-- * macOS: Uses @sysctl -n machdep.cpu.brand_string@
-- * Linux: Parses @\/proc\/cpuinfo@
-- * Windows: Uses @wmic cpu get name@
-- * Other: Returns 'Nothing'
getCPUModel :: IO (Maybe Text)
getCPUModel = do
#if defined(darwin_HOST_OS)
  getDarwinCPUModel
#elif defined(linux_HOST_OS)
  getLinuxCPUModel
#elif defined(mingw32_HOST_OS)
  getWindowsCPUModel
#else
  return Nothing
#endif

#if defined(darwin_HOST_OS)
-- | Get CPU model on macOS using sysctl.
getDarwinCPUModel :: IO (Maybe Text)
getDarwinCPUModel = do
  result <- safeReadProcess "sysctl" ["-n", "machdep.cpu.brand_string"] ""
  case result of
    Nothing -> do
      -- Apple Silicon doesn't have brand_string, try chip info
      chipResult <- safeReadProcess "sysctl" ["-n", "machdep.cpu.brand"] ""
      case chipResult of
        Nothing -> do
          -- Last resort: check if it's Apple Silicon
          armResult <- safeReadProcess "uname" ["-m"] ""
          case armResult of
            Just m | "arm" `T.isInfixOf` T.toLower m -> return $ Just "Apple_Silicon"
            _ -> return Nothing
        Just chip -> return $ Just $ cleanCPUName chip
    Just name -> return $ Just $ cleanCPUName name
#endif

#if defined(linux_HOST_OS)
-- | Get CPU model on Linux by parsing /proc/cpuinfo.
getLinuxCPUModel :: IO (Maybe Text)
getLinuxCPUModel = do
  result <- safeReadProcess "grep" ["-m1", "model name", "/proc/cpuinfo"] ""
  case result of
    Nothing -> return Nothing
    Just line ->
      let parts = T.splitOn ":" line
      in case parts of
           [_, name] -> return $ Just $ cleanCPUName name
           _         -> return Nothing
#endif

#if defined(mingw32_HOST_OS)
-- | Get CPU model on Windows using WMIC.
getWindowsCPUModel :: IO (Maybe Text)
getWindowsCPUModel = do
  result <- safeReadProcess "wmic" ["cpu", "get", "name"] ""
  case result of
    Nothing -> return Nothing
    Just output ->
      let ls = filter (not . T.null) $ T.lines output
      in case drop 1 ls of  -- Skip header line
           (name:_) -> return $ Just $ cleanCPUName name
           _        -> return Nothing
#endif

-- | Safely run a process, returning Nothing on failure.
safeReadProcess :: FilePath -> [String] -> String -> IO (Maybe Text)
safeReadProcess cmd args input =
  (Just . T.pack <$> readProcess cmd args input)
    `catch` (\(_ :: SomeException) -> return Nothing)

-- | Clean up a CPU name for use as an identifier.
cleanCPUName :: Text -> Text
cleanCPUName = T.strip . T.unwords . T.words

-- | Sanitize a string for use in filenames.
--
-- Replaces spaces with underscores and removes problematic characters.
sanitizeForFilename :: Text -> Text
sanitizeForFilename = T.map sanitizeChar
  where
    sanitizeChar c
      | isAlphaNum c = c
      | c == '-'     = c
      | c == '_'     = c
      | c == ' '     = '_'
      | otherwise    = '_'
