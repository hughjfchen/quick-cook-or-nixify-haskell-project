-- | This module defines the core domain related business errors.

module Core.MyError
( MyError(..)
  ) where

data MyError = NotFound !Text
  deriving stock (Show, Typeable)
