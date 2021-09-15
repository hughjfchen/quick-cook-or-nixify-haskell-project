-- | This module defines the core domain types which shared by lib and exe

module Core.Types(
  {{name|toPascal}}(..)
            ) where

data {{name|toPascal}} = {{name|toPascal}} { field1 :: Int
                                           , field2 :: Text
                                           } deriving stock (Eq, Show, Typeable, Generic)
