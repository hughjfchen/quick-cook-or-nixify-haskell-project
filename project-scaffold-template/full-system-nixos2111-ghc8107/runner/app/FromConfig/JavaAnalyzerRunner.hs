-- |
-- Copyright: (c) 2021 Hugh JF Chen
-- License: MIT
-- Maintainer: Hugh JF Chen <hugh.jf.chen@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- FromConfig instance for JavaAnalyzerRunner
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
module FromConfig.JavaAnalyzerRunner () where

import Conferer.FromConfig

import Data.Dynamic

import Core.Types (JavaAnalyzerRunner(..))

-- | Deconstruct a 'Core.Types.JavaAnalyzerRunner' into a many key/dynamic pairs to
-- provide valid defaults for downstream 'fetchFromConfig'
deconstructJavaAnalyzerRunnerToDefaults :: JavaAnalyzerRunner -> [(Key, Dynamic)]
deconstructJavaAnalyzerRunnerToDefaults JavaAnalyzerRunner{..} =
  [ ("field1", toDyn field1)
  , ("field2", toDyn field2)
  ]

instance DefaultConfig JavaAnalyzerRunner where
  configDef = JavaAnalyzerRunner { field1 = 0
                                , field2 = "field2"
                                }

instance FromConfig JavaAnalyzerRunner where
  fromConfig key originalConfig = do
    config <- addDefaultsAfterDeconstructingToDefaults deconstructJavaAnalyzerRunnerToDefaults key originalConfig

    field1 <- fetchFromConfig (key /. "field1") config
    field2 <- fetchFromConfig (key /. "field2") config

    pure JavaAnalyzerRunner{..}

