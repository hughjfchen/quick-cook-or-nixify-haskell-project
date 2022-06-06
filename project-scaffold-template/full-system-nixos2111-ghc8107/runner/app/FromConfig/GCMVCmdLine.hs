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
module FromConfig.GCMVCmdLine () where

import Conferer.FromConfig

import Data.Dynamic

import Core.Types (GCMVCmdLineOptions (..))

{- | Deconstruct a 'Core.Types.GCMVCmdLineOptions' into a many key/dynamic pairs to
 provide valid defaults for downstream 'fetchFromConfig'
-}
deconstructGCMVCmdLineOptionsToDefaults :: GCMVCmdLineOptions -> [(Key, Dynamic)]
deconstructGCMVCmdLineOptionsToDefaults GCMVCmdLineOptions {..} =
  [ ("Xmx", toDyn gcmvCmdLineXmx)
  , ("jvm", toDyn gcmvJVMPath)
  , ("preference", toDyn gcmvPreference)
  ]

instance DefaultConfig GCMVCmdLineOptions where
  configDef =
    GCMVCmdLineOptions
      { gcmvCmdLineXmx = "-Xmx2048M"
      , gcmvJVMPath = "java"
      , gcmvPreference = ""
      }

instance FromConfig GCMVCmdLineOptions where
  fromConfig key originalConfig = do
    config <- addDefaultsAfterDeconstructingToDefaults deconstructGCMVCmdLineOptionsToDefaults key originalConfig

    gcmvCmdLineXmx <- fetchFromConfig (key /. "Xmx") config
    gcmvJVMPath <- fetchFromConfig (key /. "jvm") config
    gcmvPreference <- fetchFromConfig (key /. "preference") config

    pure GCMVCmdLineOptions {..}
