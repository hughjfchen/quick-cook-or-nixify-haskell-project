-- | This module define the type class to fetch the dumps
-- | It could fetch via local, http or s3.

module Capability.DumpFetchor
  ( DumpFetchorM(..)
  ) where

import As
import Error
import Core.MyError
import Core.Types (Location')

import Path
import Control.Monad.Catch (MonadThrow)

class (Monad m) => DumpFetchorM m where
  fetchDump :: (WithError err m, As err MyError, MonadThrow m) => Path Rel File -> Path Rel Dir -> Location' -> m (Path Abs File)
