-- | This module construct the application monad by specializing the
-- general monad stack

module AppM
  ( AppM
  , runAppM
  )
where

import MonadStack

import AppEnv
import AppError

type AppM = MonadStack AppError AppEnv

runAppM :: AppEnv -> AppM a -> IO a
runAppM = runMonadStack
