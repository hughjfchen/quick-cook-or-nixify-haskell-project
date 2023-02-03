-- | This module defines the core domain related business errors.
module Core.MyError (
  MyError (..),
) where

data MyError
  = InvalidDeploymentSiteName !Text
  | InvalidDeploymentPhaseName !Text
  | InvalidDeploymentTarget !Text
  | InvalidPushRef !Text
  | InvalidPushEventRef !Text
  | InvalidConfigParameters !Text
  | InvalidNginxCommandLine !Text
  | CommandNotFound !Text
  | PathNotFound !FilePath
  | FailedToDownloadDumpFile !Text
  | NotExecutable !FilePath
  | NotImplementedYet !Text
  | NotSupportedAction !Text
  | ExecutionTimeOut !Text
  | ParseReportError !Text
  | GeneralLogicError !Text
  deriving stock (Eq, Ord, Show, Typeable)
