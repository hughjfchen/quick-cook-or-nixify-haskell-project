-- | This module define the Env for application

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}

module AppEnv (
  Command(..)
  , AppEnv(..)
  ) where

import Has

import Core.Types

import qualified Conferer as Conf
import Conferer.FromConfig

import qualified Database.PostgreSQL.Simple as PGS
import qualified FromConfig.ResourcePool as RP

import FromConfig.PostgresqlSimple ()
import FromConfig.ResourcePool ()
import FromConfig.{{name|toPascal}} ()

data Command = Start
        | Stop
        | Status
        | Unknown
        deriving stock (Eq, Show, Read, Generic)
instance FromConfig Command where
  fromConfig = fetchFromConfigByRead

-- | following is just for example
-- | you must put your own env
data AppEnv = AppEnv {
  appEnvCommand :: Command
  , appEnvDatabase :: PGS.ConnectInfo
  , appEnvPool :: RP.PoolInfo
  , appEnv{{name|toPascal}} :: {{name|toPascal}}
  } deriving stock (Generic)
        deriving (Has {{name|toPascal}}) via Field "appEnv{{name|toPascal}}" AppEnv
instance FromConfig AppEnv

instance Conf.DefaultConfig AppEnv where
  configDef = AppEnv {
    appEnvCommand = Start
    , appEnvDatabase = Conf.configDef
    , appEnvPool = Conf.configDef
    , appEnv{{name|toPascal}} = Conf.configDef
    }

