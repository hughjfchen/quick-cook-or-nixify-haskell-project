{-# LANGUAGE FlexibleContexts #-}

-- | This module define specs for domain business actions
module Capability.HostInfo
  ( HostInfoM (..),
  )
where

import As
import Core.MyError
import Error

class (Monad m) => HostInfoM m where
  getHostName ::
    ( WithError err m,
      As err MyError
    ) =>
    m Text
