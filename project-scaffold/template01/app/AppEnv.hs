-- | This module define the Env for application

{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DataKinds #-}

module AppEnv (
  AppEnv(..)
  , RedirectedUris
  , mkAppEnv
  ) where

import Core.ConnectionInfo
import Core.AuthInfo
import Core.MyCookieJar
import Has

import Text.URI (URI)
import CmdLine (CmdOptions(..))

type RedirectedUris = IORef [URI]

data AppEnv = AppEnv {
  envConnectionInfo :: ConnectionInfo
  , envAuthInfo :: AuthInfo
  , envMyCookieJar :: MyCookieJar
  , envRedirectedUris :: RedirectedUris
  }
    deriving (Has ConnectionInfo) via Field "envConnectionInfo" AppEnv
    deriving (Has AuthInfo) via Field "envAuthInfo" AppEnv
    deriving (Has MyCookieJar) via Field "envMyCookieJar" AppEnv
    deriving (Has RedirectedUris) via Field "envRedirectedUris" AppEnv


mkAppEnv :: (MonadIO m) => CmdOptions -> m AppEnv
mkAppEnv cmdOptions = do
  empCJ <- emptyCookieJar
  empRedirectUris <- newIORef []
  pure $ AppEnv {
    envConnectionInfo = ConnectionInfo {
        ciHost = cmdHost cmdOptions
        , ciPort = cmdPort cmdOptions }
    , envAuthInfo = AuthInfo {
        aiAdminUser = cmdUserName cmdOptions
        , aiAdminPassword = cmdPassword cmdOptions
        }
    , envMyCookieJar = empCJ
    , envRedirectedUris = empRedirectUris
    }
