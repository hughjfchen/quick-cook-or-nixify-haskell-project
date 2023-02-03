{-# LANGUAGE FlexibleContexts #-}

-- | This module define specs for domain business actions
module Capability.KeepNginxCompliance
  ( KeepNginxComplianceM (..),
  )
where

import As
import Core.MyError
import Core.Types
import Error

class (Monad m) => KeepNginxComplianceM m where
  check ::
    ( WithError err m,
      As err MyError
    ) =>
    Instruction ->
    [Target] ->
    m [[NgCheckResult]]

  comply ::
    ( WithError err m,
      As err MyError
    ) =>
    Instruction ->
    [Target] ->
    m ()
