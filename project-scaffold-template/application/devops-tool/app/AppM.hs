-- | This module construct the application monad by specializing the
-- general monad stack
module AppM
  ( AppM,
    runAppM,
  )
where

import AppEnv
import AppError
import MonadStack

type AppM = MonadStack AppError AppEnv

runAppM :: AppEnv -> AppM a -> IO a
runAppM = runMonadStack
