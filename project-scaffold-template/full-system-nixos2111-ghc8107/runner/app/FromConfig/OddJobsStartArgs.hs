-- |
-- Copyright: (c) 2021 Hugh JF Chen
-- License: MIT
-- Maintainer: Hugh JF Chen <hugh.jf.chen@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- FromConfig instance for resource-pool
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
module FromConfig.OddJobsStartArgs () where

import Conferer.FromConfig

import Data.Dynamic

import OddJobs.Cli      (CommonStartArgs(..))

-- | Deconstruct a 'OddJobs.Cli.StartArgs' into a many key/dynamic pairs to
-- provide valid defaults for downstream 'fetchFromConfig'
deconstructStartArgsToDefaults :: CommonStartArgs -> [(Key, Dynamic)]
deconstructStartArgsToDefaults CommonStartArgs{..} =
  [("daemonize", toDyn startDaemonize)
  , ("pidFile", toDyn startPidFile)
  ]

instance DefaultConfig CommonStartArgs where
  configDef = CommonStartArgs { startDaemonize = True
                       , startPidFile = "job-runner.pid"
                       }

instance FromConfig CommonStartArgs where
  fromConfig key originalConfig = do
    config <- addDefaultsAfterDeconstructingToDefaults deconstructStartArgsToDefaults key originalConfig

    startDaemonize <- fetchFromConfig (key /. "daemonize") config
    startPidFile <- fetchFromConfig (key /. "pidFile") config

    pure CommonStartArgs{..}
