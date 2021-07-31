-- | This module defines a type class As to support
-- convert an error type to another error type

module As
( As(..)
  ) where

class As s a where
  as :: a -> s
  match :: s -> Maybe a

instance As s s where
  as = id
  match = Just
