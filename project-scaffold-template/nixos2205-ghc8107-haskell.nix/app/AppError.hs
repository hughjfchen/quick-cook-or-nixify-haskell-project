-- | This module defines the application implementation related errors.

module AppError
( AppError(..)
  ) where

import As
import Core.MyError

data AppError = MyError MyError
              | YourAppError Text
              deriving stock (Show, Typeable)

instance As AppError MyError where
  as = MyError
  match (MyError mye) = Just mye
  match _ = Nothing
