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
module FromConfig.OddJobsStopArgs () where

import Conferer.FromConfig

import Data.Dynamic

import OddJobs.Cli      (StopArgs(..))

-- | Deconstruct a 'OddJobs.Cli.StopArgs' into a many key/dynamic pairs to
-- provide valid defaults for downstream 'fetchFromConfig'
deconstructStopArgsToDefaults :: StopArgs -> [(Key, Dynamic)]
deconstructStopArgsToDefaults StopArgs{..} =
  [ ("timeout", toDyn shutTimeout)
  , ("pidFile", toDyn shutPidFile)

  ]

instance DefaultConfig StopArgs where
  configDef = StopArgs { shutTimeout = 30
                       , shutPidFile = "job-runner.pid"
                       }

instance FromConfig StopArgs where
  fromConfig key originalConfig = do
    config <- addDefaultsAfterDeconstructingToDefaults deconstructStopArgsToDefaults key originalConfig

    shutTimeout <- fetchFromConfig (key /. "timeout") config
    shutPidFile <- fetchFromConfig (key /. "pidFile") config

    pure StopArgs{..}
