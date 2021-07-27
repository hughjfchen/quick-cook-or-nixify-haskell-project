-- | This module defines the application implementation related errors.

module AppError
( AppError(..)
  ) where

import As
import Core.MyError
import Network.HTTP.Client

data AppError = MyError MyError
              | AppInvalidUrlError Text
              | AppHttpError Request HttpExceptionContent
              | AppJsonError Text
              deriving stock (Show, Typeable)

instance As AppError MyError where
  as = MyError
  match (MyError mye) = Just mye
  match _ = Nothing
