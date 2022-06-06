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
module FromConfig.MATCmdLine () where

import Conferer.FromConfig

import Data.Dynamic

import Core.Types (MATCmdLineOptions(..))

-- | Deconstruct a 'Core.Types.MATCmdLineOptions' into a many key/dynamic pairs to
-- provide valid defaults for downstream 'fetchFromConfig'
deconstructMATCmdLineOptionsToDefaults :: MATCmdLineOptions -> [(Key, Dynamic)]
deconstructMATCmdLineOptionsToDefaults MATCmdLineOptions{..} =
  [ ("Xmx", toDyn matCmdLineXmx)
  ]

instance DefaultConfig MATCmdLineOptions where
  configDef = MATCmdLineOptions { matCmdLineXmx= "-Xmx2048M"
                       }

instance FromConfig MATCmdLineOptions where
  fromConfig key originalConfig = do
    config <- addDefaultsAfterDeconstructingToDefaults deconstructMATCmdLineOptionsToDefaults key originalConfig

    matCmdLineXmx <- fetchFromConfig (key /. "Xmx") config

    pure MATCmdLineOptions{..}
