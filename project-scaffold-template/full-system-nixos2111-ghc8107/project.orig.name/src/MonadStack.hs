-- | This module construct the application monad

module MonadStack
( MonadStack(..)
  , runMonadStack
  , runMonadStackAsIO
 )
where

import Control.Exception (try, throwIO, catch)
import Control.Monad.Except (MonadError(..))
import Relude.Extra.Bifunctor (firstF)
import Error

newtype MonadStack err env a = MonadStack { unMonadStack :: ReaderT env IO a }
                        deriving newtype (Functor, Applicative, Monad
                                        , MonadFail, MonadReader env
                                        , MonadIO)

{- | This instance allows to throw and catch errors that are visible in type
definitions. The implementation relies on underlying 'IO' machinery.
Use 'CakeSlayer.Error.throwError' and 'CakeSlayer.Error.catchError': these
functions automatically attach source code positions to errors.
-}
instance (Show err, Typeable err)
    => MonadError (ErrorWithSource err) (MonadStack err env)
  where
    throwError :: ErrorWithSource err -> MonadStack err env a
    throwError = liftIO . throwIO . AppException
    {-# INLINE throwError #-}

    catchError
        :: MonadStack err env a
        -> (ErrorWithSource err -> MonadStack err env a)
        -> MonadStack err env a
    catchError action handler = MonadStack $ ReaderT $ \env -> do
        let ioAction = runMonadStack env action
        ioAction `catch` \(AppException e) -> runMonadStack env $ handler e
    {-# INLINE catchError #-}

{- | Run application by providing environment.
Throws 'AppException' if application has unhandled 'throwError'. Use
'runMonadStackAsIO' to handle exceptions as well.
-}
runMonadStack :: env -> MonadStack err env a -> IO a
runMonadStack env = usingReaderT env . unMonadStack
{-# INLINE runMonadStack #-}

{- | Like 'runMonadStack' but also catches 'AppException' and unwraps 'ErrorWithSource'
from it. Use this function to handle errors outside 'App' monad.
-}
runMonadStackAsIO
    :: (Show err, Typeable err)
    => env
    -> MonadStack err env a
    -> IO (Either (ErrorWithSource err) a)
runMonadStackAsIO env = firstF unAppException . try . runMonadStack env
{-# INLINE runMonadStackAsIO #-}
