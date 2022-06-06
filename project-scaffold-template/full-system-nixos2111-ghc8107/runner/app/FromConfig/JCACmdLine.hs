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
module FromConfig.JCACmdLine () where

import Conferer.FromConfig

import Data.Dynamic

import Core.Types (JCACmdLineOptions(..))

-- | Deconstruct a 'Core.Types.JCACmdLineOptions' into a many key/dynamic pairs to
-- provide valid defaults for downstream 'fetchFromConfig'
deconstructJCACmdLineOptionsToDefaults :: JCACmdLineOptions -> [(Key, Dynamic)]
deconstructJCACmdLineOptionsToDefaults JCACmdLineOptions{..} =
  [ ("Xmx", toDyn jcaCmdLineXmx)
  ]

instance DefaultConfig JCACmdLineOptions where
  configDef = JCACmdLineOptions { jcaCmdLineXmx = "-Xmx2048M"
                       }

instance FromConfig JCACmdLineOptions where
  fromConfig key originalConfig = do
    config <- addDefaultsAfterDeconstructingToDefaults deconstructJCACmdLineOptionsToDefaults key originalConfig

    jcaCmdLineXmx <- fetchFromConfig (key /. "Xmx") config

    pure JCACmdLineOptions{..}
