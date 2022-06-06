-- |
-- Copyright: (c) 2021 Hugh JF Chen
-- License: MIT
-- Maintainer: Hugh JF Chen <hugh.jf.chen@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- FromConfig instance for oddjobs stopArgs
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE GADTs #-}
module FromConfig.OddJobsConfig () where

import Conferer.FromConfig

import Data.Dynamic

import OddJobs.Types (Seconds(..), Config(..), ConcurrencyControl(..), JobErrHandler(..))
import OddJobs.ConfigBuilder (mkConfig)

-- wrapper for cfgOnJobFailed
newtype JobFailedHandlers = JobFailedHandlers [JobErrHandler]

-- | Deconstruct a 'OddJobs.Cli.Config' into a many key/dynamic pairs to
-- provide valid defaults for downstream 'fetchFromConfig'
deconstructOddJobsConfigToDefaults :: Config -> [(Key, Dynamic)]
deconstructOddJobsConfigToDefaults Config{..} =
  [ ("tableName", toDyn cfgTableName)
  , ("jobRunner", toDyn cfgJobRunner)
  , ("defaultMaxAttempts", toDyn cfgDefaultMaxAttempts)
  , ("concurrencyControl", toDyn cfgConcurrencyControl)
  , ("dbPool", toDyn cfgDbPool)
  , ("pollingInterval", toDyn cfgPollingInterval)
  , ("onJobSuccess", toDyn cfgOnJobSuccess)
  , ("onJobFailed", toDyn $ JobFailedHandlers cfgOnJobFailed)
  , ("onJobStart", toDyn cfgOnJobStart)
  , ("onJobTimeout", toDyn cfgOnJobTimeout)
  -- , ("pidFile", toDyn cfgPidFile)
  , ("logger", toDyn cfgLogger)
  , ("jobType", toDyn cfgJobType)
  -- , ("jobTypeSql", toDyn cfgJobTypeSql)
  , ("defaultJobTimeout", toDyn cfgDefaultJobTimeout)
  -- , ("jobToHtml", toDyn cfgJobToHtml)
  -- , ("allJobTypes", toDyn cfgAllJobTypes)
  ]

instance DefaultConfig Config where
  configDef = mkConfig (\_ _ -> pass) "jobs" (error "Not set the database pool yet") (MaxConcurrentJobs 1) (const pass) id

instance FromConfig Config where
  fromConfig key originalConfig = do
    config <- addDefaultsAfterDeconstructingToDefaults deconstructOddJobsConfigToDefaults key originalConfig

    -- cfgTableNameStr <- fetchFromConfig (key /. "tableName") config :: IO ByteString
    cfgTableNameStr <- fetchFromConfig (key /. "tableName") config
    cfgTableName <- pure $ fromString cfgTableNameStr
    cfgJobRunner <- fetchFromConfig (key /. "jobRunner") config
    cfgDefaultMaxAttempts <- fetchFromConfig (key /. "defaultMaxAttempts") config
    cfgConcurrencyControlInt <- fetchFromConfig (key /. "concurrencyControl") config
    cfgConcurrencyControl <- pure $ MaxConcurrentJobs cfgConcurrencyControlInt
    cfgDbPool <- fetchFromConfig (key /. "dbPool") config
    cfgPollingIntervalInt <- fetchFromConfig (key /. "pollingInterval") config
    cfgPollingInterval <- pure $ Seconds cfgPollingIntervalInt
    cfgOnJobSuccess <- fetchFromConfig (key /. "onJobSuccess") config
    (JobFailedHandlers cfgOnJobFailed) <- fetchFromConfig (key /. "onJobFailed") config
    cfgOnJobStart <- fetchFromConfig (key /. "onJobStart") config
    cfgOnJobTimeout <- fetchFromConfig (key /. "onJobTimeout") config
    -- cfgPidFile <- fetchFromConfig (key /. "pidFile") config
    cfgLogger <- fetchFromConfig (key /. "logger") config
    cfgJobType <- fetchFromConfig (key /. "jobType") config
    -- cfgJobTypeSql <- fetchFromConfig (key /. "jobTypeSql") config
    cfgDefaultJobTimeoutInt <- fetchFromConfig (key /. "defaultJobTimeout") config
    cfgDefaultJobTimeout <- pure $ Seconds cfgDefaultJobTimeoutInt
    -- cfgJobToHtml <- fetchFromConfig (key /. "jobToHtml") config
    -- cfgAllJobTypes <- fetchFromConfig (key /. "allJobTypes") config

    pure Config{..}
