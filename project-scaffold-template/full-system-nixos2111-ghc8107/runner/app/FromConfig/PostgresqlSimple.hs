-- |
-- Copyright: (c) 2021 Hugh JF Chen
-- License: MIT
-- Maintainer: Hugh JF Chen <hugh.jf.chen@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- FromConfig instance for postgresql-simple
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
module FromConfig.PostgresqlSimple () where

import Conferer.FromConfig

import qualified Database.PostgreSQL.Simple as PGS
import Data.Dynamic


-- | Deconstruct a 'Database.PostgreSQL.Simple.ConnectInfo' into a many key/dynamic pairs to
-- provide valid defaults for downstream 'fetchFromConfig'
deconstructConnInfoToDefaults :: PGS.ConnectInfo -> [(Key, Dynamic)]
deconstructConnInfoToDefaults PGS.ConnectInfo{..} =
  [ ("host", toDyn connectHost)
  , ("port", toDyn connectPort)
  , ("user", toDyn connectUser)
  , ("password", toDyn connectPassword)
  , ("database", toDyn connectDatabase)

  ]

instance DefaultConfig PGS.ConnectInfo where
  configDef = PGS.defaultConnectInfo

instance FromConfig PGS.ConnectInfo where
  fromConfig key originalConfig = do
    config <- addDefaultsAfterDeconstructingToDefaults deconstructConnInfoToDefaults key originalConfig

    connectHost <- fetchFromConfig (key /. "host") config
    connectPortInt <- fetchFromConfig (key /. "port") config :: IO Int
    connectPort <- pure $ fromIntegral connectPortInt
    connectUser <- fetchFromConfig (key /. "user") config
    connectPassword <- fetchFromConfig (key /. "password") config
    connectDatabase <- fetchFromConfig (key /. "database") config

    pure $ PGS.ConnectInfo{..}
