{-# LANGUAGE FlexibleContexts #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module define specs for domain business actions
module AppCapability.AppTimeStamp
  ( TimeStampM (..),
  )
where

import AppM (AppM)
import As
import Capability.TimeStamp
import Core.MyError
import Data.Time
import Error

instance TimeStampM AppM where
  getCurrentTimeStamp ::
    ( WithError err m,
      As err MyError,
      MonadIO m
    ) =>
    m Text
  getCurrentTimeStamp = show <$> liftIO getZonedTime
