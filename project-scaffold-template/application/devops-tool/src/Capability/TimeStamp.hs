{-# LANGUAGE FlexibleContexts #-}

-- | This module define specs for domain business actions
module Capability.TimeStamp
  ( TimeStampM (..),
  )
where

import As
import Core.MyError
import Error

class (Monad m) => TimeStampM m where
  getCurrentTimeStamp ::
    ( WithError err m,
      As err MyError
    ) =>
    m Text
