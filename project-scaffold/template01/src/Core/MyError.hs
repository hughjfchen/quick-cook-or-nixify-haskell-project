-- | This module defines the core domain related business errors.

module Core.MyError
( MyError(..)
  ) where

data MyError = NotSecure Text
              | NotFound Text
              | UserNameEmpty
              | PasswordEmpty Text
              | PasswordNotMatch Text
              | UserAlreadyLogined Text
              | PreSessionChangeNotSaved Text
              | ChangeNotSaved Text
              | JVMParametersExist Text
              deriving stock (Show, Typeable)
