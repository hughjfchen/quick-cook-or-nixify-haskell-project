-- | This module define the type to uploadreports to the web site for viewing/downloading

module Capability.ReportUploader
  ( ReportUploaderM(..)
  ) where

import Has
import As
import Error
import Core.MyError

import Core.Types

import Path

class (Monad m) => ReportUploaderM m where
  uploadReport :: (WithError err m, As err MyError, MonadReader env m, Has CommandPath' env) => Int -> Path Abs File -> m ()
