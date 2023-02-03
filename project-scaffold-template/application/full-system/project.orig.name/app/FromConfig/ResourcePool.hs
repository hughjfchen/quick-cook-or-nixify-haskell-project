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
module FromConfig.ResourcePool
( PoolInfo(..)
  ) where

import Conferer.FromConfig

import Data.Time.Clock  (NominalDiffTime)
import Data.Dynamic

data PoolInfo = PoolInfo {
  poolStripe :: !Int
  , poolIdleTime :: !NominalDiffTime
  , poolSize :: !Int
  } deriving stock (Eq, Show, Generic, Typeable)

-- | Deconstruct a 'Data.Pool' into a many key/dynamic pairs to
-- provide valid defaults for downstream 'fetchFromConfig'
deconstructPoolInfoToDefaults :: PoolInfo -> [(Key, Dynamic)]
deconstructPoolInfoToDefaults PoolInfo{..} =
  [ ("stripe", toDyn poolStripe)
  , ("idleTime", toDyn poolIdleTime)
  , ("size", toDyn poolSize)

  ]

instance DefaultConfig PoolInfo where
  configDef = PoolInfo { poolStripe = 1
                       , poolIdleTime = 1800
                       , poolSize = 10
                       }

instance FromConfig PoolInfo where
  fromConfig key originalConfig = do
    config <- addDefaultsAfterDeconstructingToDefaults deconstructPoolInfoToDefaults key originalConfig

    poolStripe <- fetchFromConfig (key /. "stripe") config
    poolIdleTimeInt <- fetchFromConfig (key /. "idleTime") config :: IO Int
    poolIdleTime <- pure $ fromIntegral poolIdleTimeInt
    poolSize <- fetchFromConfig (key /. "size") config

    pure PoolInfo{..}
