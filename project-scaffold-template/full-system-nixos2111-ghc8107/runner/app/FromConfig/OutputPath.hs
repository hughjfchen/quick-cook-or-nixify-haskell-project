{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

{- |
 Copyright: (c) 2021 Hugh JF Chen
 License: MIT
 Maintainer: Hugh JF Chen <hugh.jf.chen@gmail.com>
 Stability: stable
 Portability: portable

 FromConfig instance for resource-pool
-}
module FromConfig.OutputPath () where

import Conferer.FromConfig

import Data.Dynamic

import Core.Types (OutputPath (..))

{- | Deconstruct a 'Core.Types.OutputPath' into a many key/dynamic pairs to
 provide valid defaults for downstream 'fetchFromConfig'
-}
deconstructOutputPathToDefaults :: OutputPath -> [(Key, Dynamic)]
deconstructOutputPathToDefaults OutputPath {..} =
  [ ("fetchedDumpHome", toDyn outputFetchedDumpHome)
  , ("jcaPreProcessorHome", toDyn outputJCAPreProcessorHome)
  , ("matPreProcessorHome", toDyn outputMATPreProcessorHome)
  , ("gcmvPreProcessorHome", toDyn outputGCMVPreProcessorHome)
  , ("jcaReportHome", toDyn outputJCAReportHome)
  , ("matReportHome", toDyn outputMATReportHome)
  , ("gcmvReportHome", toDyn outputGCMVReportHome)
  , ("jcaPostProcessorHome", toDyn outputJCAPostProcessorHome)
  , ("matPostProcessorHome", toDyn outputMATPostProcessorHome)
  , ("gcmvPostProcessorHome", toDyn outputGCMVPostProcessorHome)
  ]

instance DefaultConfig OutputPath where
  configDef =
    OutputPath
      { outputFetchedDumpHome = "/tmp/raw_dump_files"
      , outputJCAPreProcessorHome = "/tmp/preprocessed_report_jca"
      , outputMATPreProcessorHome = "/tmp/preprocessed_report_mat"
      , outputGCMVPreProcessorHome = "/tmp/preprocessed_report_gcmv"
      , outputJCAReportHome = "/tmp/parsed_report_jca"
      , outputMATReportHome = "/tmp/parsed_report_mat"
      , outputGCMVReportHome = "/tmp/parsed_report_gcmv"
      , outputJCAPostProcessorHome = "/tmp/postprocessed_report_jca"
      , outputMATPostProcessorHome = "/tmp/postprocessed_report_mat"
      , outputGCMVPostProcessorHome = "/tmp/postprocessed_report_gcmv"
      }

instance FromConfig OutputPath where
  fromConfig key originalConfig = do
    config <- addDefaultsAfterDeconstructingToDefaults deconstructOutputPathToDefaults key originalConfig

    outputFetchedDumpHome <- fetchFromConfig (key /. "fetchedDumpHome") config
    outputJCAPreProcessorHome <- fetchFromConfig (key /. "jcaPreProcessorHome") config
    outputMATPreProcessorHome <- fetchFromConfig (key /. "matPreProcessorHome") config
    outputGCMVPreProcessorHome <- fetchFromConfig (key /. "gcmvPreProcessorHome") config
    outputJCAReportHome <- fetchFromConfig (key /. "jcaReportHome") config
    outputMATReportHome <- fetchFromConfig (key /. "matReportHome") config
    outputGCMVReportHome <- fetchFromConfig (key /. "gcmvReportHome") config
    outputJCAPostProcessorHome <- fetchFromConfig (key /. "jcaPostProcessorHome") config
    outputMATPostProcessorHome <- fetchFromConfig (key /. "matPostProcessorHome") config
    outputGCMVPostProcessorHome <- fetchFromConfig (key /. "gcmvPostProcessorHome") config

    pure OutputPath {..}
