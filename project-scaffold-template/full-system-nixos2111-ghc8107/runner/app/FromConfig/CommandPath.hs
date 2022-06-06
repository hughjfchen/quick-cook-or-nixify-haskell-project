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
module FromConfig.CommandPath () where

import Conferer.FromConfig

import Data.Dynamic

import Core.Types (CommandPath (..))

{- | Deconstruct a 'Core.Types.CommandPath' into a many key/dynamic pairs to
 provide valid defaults for downstream 'fetchFromConfig'
-}
deconstructCommandPathToDefaults :: CommandPath -> [(Key, Dynamic)]
deconstructCommandPathToDefaults CommandPath {..} =
  [ ("xvfbPath", toDyn cmdXvfbPath)
  , ("wgetPath", toDyn cmdJavaPath)
  , ("curlPath", toDyn cmdJavaPath)
  , ("javaPath", toDyn cmdJavaPath)
  , ("parseDumpShPath", toDyn cmdParseDumpShPath)
  , ("jcaPath", toDyn cmdJCAPath)
  , ("gcmvPath", toDyn cmdGCMVPath)
  ]

instance DefaultConfig CommandPath where
  configDef =
    CommandPath
      { cmdXvfbPath = "xvfb"
      , cmdWgetPath = "wget"
      , cmdCurlPath = "curl"
      , cmdJavaPath = "java"
      , cmdParseDumpShPath = "parseDump.sh"
      , cmdJCAPath = "jca467.jar"
      , cmdGCMVPath = "gcmv"
      }

instance FromConfig CommandPath where
  fromConfig key originalConfig = do
    config <- addDefaultsAfterDeconstructingToDefaults deconstructCommandPathToDefaults key originalConfig

    cmdXvfbPath <- fetchFromConfig (key /. "xvfbPath") config
    cmdWgetPath <- fetchFromConfig (key /. "wgetPath") config
    cmdCurlPath <- fetchFromConfig (key /. "curlPath") config
    cmdJavaPath <- fetchFromConfig (key /. "javaPath") config
    cmdParseDumpShPath <- fetchFromConfig (key /. "parseDumpShPath") config
    cmdJCAPath <- fetchFromConfig (key /. "jcaPath") config
    cmdGCMVPath <- fetchFromConfig (key /. "gcmvPath") config

    pure CommandPath {..}
