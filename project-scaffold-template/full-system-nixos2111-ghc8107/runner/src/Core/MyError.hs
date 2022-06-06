-- | This module defines the core domain related business errors.

module Core.MyError
( MyError(..)
 ) where

data MyError = InvalidLocation !Text
             | InvalidConfigParameters !Text
             | CommandNotFound !Text
             | PathNotFound !FilePath
             | FailedToDownloadDumpFile !Text
             | NotExecutable !FilePath
             | NotImplementedYet !Text
             | ExecutionTimeOut !Text
             | ParseReportError !Text
  deriving stock (Show, Typeable)
