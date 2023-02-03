{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module define specs for domain business actions
module AppCapability.AppHostInfo (
  HostInfoM (..),
) where

import AppM (AppM)
import As
import Capability.HostInfo
import Core.MyError
import Error
import Propellor (readCreateProcessTrailed)
import Propellor.Base (shell)

instance HostInfoM AppM where
  getHostName ::
    ( WithError err m
    , As err MyError
    , MonadIO m
    ) =>
    m Text
  getHostName = liftIO $ readCreateProcessTrailed (shell "hostname") "" <&> toText
