-- | This module define the Env for application

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}

module AppEnv (
  AppEnv(..)
  , mkAppEnv
  ) where

import Has

import CmdLine (CmdOptions(..))

-- | following is just for example
-- | you must put your own env
data AppEnv = AppEnv {
	envConnectionInfo :: !Text
  }
    deriving (Has ConnectionInfo) via Field "envConnectionInfo" AppEnv


mkAppEnv :: (MonadIO m) => CmdOptions -> m AppEnv
mkAppEnv cmdOptions = do
  pure $ AppEnv {
    envConnectionInfo = "Test"
    }
