-- | This module defines the core domain types which shared by lib and exe
module Core.Types (
  Location (..),
  Location' (..),
  Report (..),
  MyJob (..),
  CommandPath (..),
  CommandPath' (..),
  OutputPath (..),
  OutputPath' (..),
  JCACmdLineOptions (..),
  JCACmdLineOptions' (..),
  MATCmdLineOptions (..),
  MATCmdLineOptions' (..),
  GCMVCmdLineOptions (..),
  GCMVCmdLineOptions' (..),
  CurlCmdLineOptions (..),
  CurlCmdLineOptions' (..),
  flowIn,
  flowOut,
  commandPathConfigIn,
  outputPathConfigIn,
  jcaCmdLineOptionsConfigIn,
  matCmdLineOptionsConfigIn,
  curlCmdLineOptionsConfigIn,
  gcmvCmdLineOptionsConfigIn,
  JavaAnalyzerRunner (..),
) where

import Control.Monad.Catch
import Data.Aeson (FromJSON, ToJSON)
import Path.Posix (Abs, Dir, File, Path, SomeBase, fromSomeFile, parseSomeDir, parseSomeFile)
import Text.URI (URI (..))
import qualified Text.URI as URI

import Data.Password.Types
import Data.Password.Validate

import As
import Core.MyError
import Error

data Location
  = Local FilePath
  | HttpUrl !Text
  | S3Path !Text
  deriving stock (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (FromJSON, ToJSON)
data Location'
  = Local' (SomeBase File)
  | HttpUrl' !URI
  | S3Path' !Text
  deriving stock (Eq, Ord, Show, Typeable, Generic)

flowIn :: (WithError err m, As err MyError, MonadThrow m) => Location -> m Location'
-- flowIn (Local f) = parseSomeFile f <&> Local'
flowIn (Local f) = Local' <$> parseSomeFile f
flowIn (HttpUrl url) = URI.mkURI url <&> HttpUrl'
flowIn (S3Path _) = throwError $ as $ NotImplementedYet "Support for S3 path not implemented yet."

flowOut :: Location' -> Location
flowOut (Local' f) = Local $ fromSomeFile f
flowOut (HttpUrl' url) = HttpUrl $ URI.render url
flowOut (S3Path' l) = S3Path l

newtype Report = Report {unReport :: Path Abs File} deriving stock (Eq, Ord, Show, Typeable, Generic)

data MyJob
  = ParseJavaCore Location
  | ParseHeapDump Location
  | ParseGC Location
  deriving stock (Eq, Ord, Show, Typeable, Generic)
  deriving anyclass (FromJSON, ToJSON)

data CommandPath = CommandPath
  { cmdXvfbPath :: !FilePath
  , cmdWgetPath :: !FilePath
  , cmdCurlPath :: !FilePath
  , cmdJavaPath :: !FilePath
  , cmdParseDumpShPath :: !FilePath
  , cmdJCAPath :: !FilePath
  , cmdGCMVPath :: !FilePath
  }
  deriving stock (Eq, Ord, Show, Typeable, Generic)

data CommandPath' = CommandPath'
  { cmdXvfbPath' :: SomeBase File
  , cmdWgetPath' :: SomeBase File
  , cmdCurlPath' :: SomeBase File
  , cmdJavaPath' :: SomeBase File
  , cmdParseDumpShPath' :: SomeBase File
  , cmdJCAPath' :: SomeBase File
  , cmdGCMVPath' :: SomeBase File
  }
  deriving stock (Eq, Ord, Show, Typeable, Generic)

commandPathConfigIn :: (MonadThrow m) => CommandPath -> m CommandPath'
commandPathConfigIn CommandPath {..} = do
  xvfb' <- parseSomeFile cmdXvfbPath
  wget' <- parseSomeFile cmdWgetPath
  curl' <- parseSomeFile cmdCurlPath
  java' <- parseSomeFile cmdJavaPath
  parseDump' <- parseSomeFile cmdParseDumpShPath
  jca' <- parseSomeFile cmdJCAPath
  gcmv' <- parseSomeFile cmdGCMVPath
  pure $
    CommandPath'
      { cmdXvfbPath' = xvfb'
      , cmdWgetPath' = wget'
      , cmdCurlPath' = curl'
      , cmdJavaPath' = java'
      , cmdParseDumpShPath' = parseDump'
      , cmdJCAPath' = jca'
      , cmdGCMVPath' = gcmv'
      }

data OutputPath = OutputPath
  { outputFetchedDumpHome :: !FilePath
  , outputJCAPreProcessorHome :: !FilePath
  , outputMATPreProcessorHome :: !FilePath
  , outputGCMVPreProcessorHome :: !FilePath
  , outputJCAReportHome :: !FilePath
  , outputMATReportHome :: !FilePath
  , outputGCMVReportHome :: !FilePath
  , outputJCAPostProcessorHome :: !FilePath
  , outputMATPostProcessorHome :: !FilePath
  , outputGCMVPostProcessorHome :: !FilePath
  }
  deriving stock (Eq, Ord, Show, Typeable, Generic)

data OutputPath' = OutputPath'
  { outputFetchedDumpHome' :: SomeBase Dir
  , outputJCAPreProcessorHome' :: SomeBase Dir
  , outputMATPreProcessorHome' :: SomeBase Dir
  , outputGCMVPreProcessorHome' :: SomeBase Dir
  , outputJCAReportHome' :: SomeBase Dir
  , outputMATReportHome' :: SomeBase Dir
  , outputGCMVReportHome' :: SomeBase Dir
  , outputJCAPostProcessorHome' :: SomeBase Dir
  , outputMATPostProcessorHome' :: SomeBase Dir
  , outputGCMVPostProcessorHome' :: SomeBase Dir
  }
  deriving stock (Eq, Ord, Show, Typeable, Generic)

outputPathConfigIn :: (MonadThrow m) => OutputPath -> m OutputPath'
outputPathConfigIn OutputPath {..} = do
  fetchedDumpHome' <- parseSomeDir outputFetchedDumpHome
  jcaPreProcessHome' <- parseSomeDir outputJCAPreProcessorHome
  matPreProcessHome' <- parseSomeDir outputMATPreProcessorHome
  gcmvPreProcessHome' <- parseSomeDir outputMATPreProcessorHome
  jcaReportHome' <- parseSomeDir outputJCAReportHome
  matReportHome' <- parseSomeDir outputMATReportHome
  gcmvReportHome' <- parseSomeDir outputMATReportHome
  jcaPostProcessHome' <- parseSomeDir outputJCAPostProcessorHome
  matPostProcessHome' <- parseSomeDir outputMATPostProcessorHome
  gcmvPostProcessHome' <- parseSomeDir outputMATPostProcessorHome
  pure $
    OutputPath'
      { outputFetchedDumpHome' = fetchedDumpHome'
      , outputJCAPreProcessorHome' = jcaPreProcessHome'
      , outputMATPreProcessorHome' = matPreProcessHome'
      , outputGCMVPreProcessorHome' = gcmvPreProcessHome'
      , outputJCAReportHome' = jcaReportHome'
      , outputMATReportHome' = matReportHome'
      , outputGCMVReportHome' = gcmvReportHome'
      , outputJCAPostProcessorHome' = jcaPostProcessHome'
      , outputMATPostProcessorHome' = matPostProcessHome'
      , outputGCMVPostProcessorHome' = gcmvPostProcessHome'
      }

data JCACmdLineOptions = JCACmdLineOptions {jcaCmdLineXmx :: !Text}
  deriving stock (Eq, Ord, Read, Show, Typeable, Generic)

data JCACmdLineOptions' = JCACmdLineOptions' {jcaCmdLineXmx' :: !Int}
  deriving stock (Eq, Ord, Show, Typeable, Generic)

jcaCmdLineOptionsConfigIn ::
  (WithError err m, As err MyError) => JCACmdLineOptions -> m JCACmdLineOptions'
jcaCmdLineOptionsConfigIn JCACmdLineOptions {..} = do
  xmx' <- pure $ readMaybe $ toString jcaCmdLineXmx
  case xmx' of
    Nothing ->
      throwError $
        as $
          InvalidConfigParameters "the jcaCmdLineXmx must be an Int between 1024 to 4096."
    Just v -> pure $ JCACmdLineOptions' {jcaCmdLineXmx' = v}

data MATCmdLineOptions = MATCmdLineOptions {matCmdLineXmx :: !Text}
  deriving stock (Eq, Ord, Show, Typeable, Generic)
data MATCmdLineOptions' = MATCmdLineOptions' {matCmdLineXmx' :: !Int}
  deriving stock (Eq, Ord, Show, Typeable, Generic)
matCmdLineOptionsConfigIn ::
  (WithError err m, As err MyError) => MATCmdLineOptions -> m MATCmdLineOptions'
matCmdLineOptionsConfigIn MATCmdLineOptions {..} = do
  xmx' <- pure $ readMaybe $ toString matCmdLineXmx
  case xmx' of
    Nothing ->
      throwError $
        as $
          InvalidConfigParameters "the matCmdLineXmx must be an Int between 1024 to 4096."
    Just v -> pure $ MATCmdLineOptions' {matCmdLineXmx' = v}

data GCMVCmdLineOptions = GCMVCmdLineOptions
  { gcmvCmdLineXmx :: !Text
  , gcmvJVMPath :: !FilePath
  , gcmvPreference :: !FilePath
  }
  deriving stock (Eq, Ord, Show, Typeable, Generic)
data GCMVCmdLineOptions' = GCMVCmdLineOptions'
  { gcmvCmdLineXmx' :: !Int
  , gcmvJVMPath' :: SomeBase Dir
  , gcmvPreference' :: SomeBase File
  }
  deriving stock (Eq, Ord, Show, Typeable, Generic)
gcmvCmdLineOptionsConfigIn ::
  (WithError err m, As err MyError, MonadThrow m) => GCMVCmdLineOptions -> m GCMVCmdLineOptions'
gcmvCmdLineOptionsConfigIn GCMVCmdLineOptions {..} = do
  xmx' <- pure $ readMaybe $ toString gcmvCmdLineXmx
  gcmvJvm' <- parseSomeDir gcmvJVMPath
  gcmvPref' <- parseSomeFile gcmvPreference
  case xmx' of
    Nothing ->
      throwError $
        as $
          InvalidConfigParameters "the gcmvCmdLineXmx must be an Int between 1024 to 4096."
    Just v -> pure $ GCMVCmdLineOptions' {gcmvCmdLineXmx' = v, gcmvJVMPath' = gcmvJvm', gcmvPreference' = gcmvPref'}

data CurlCmdLineOptions = CurlCmdLineOptions
  { curlCmdLineLoginUser :: !Text
  , curlCmdLineLoginPIN :: !Text
  , curlCmdLineLoginUrl :: !Text
  , curlCmdLineUploadUrl :: !Text
  }
  deriving stock (Show, Typeable, Generic)

data CurlCmdLineOptions' = CurlCmdLineOptions'
  { curlCmdLineLoginUser' :: !Password
  , curlCmdLineLoginPIN' :: !Password
  , curlCmdLineLoginUrl' :: !URI
  , curlCmdLineUploadUrl' :: !URI
  }
  deriving stock (Show, Typeable, Generic)

curlCmdLineOptionsConfigIn ::
  (WithError err m, As err MyError, MonadThrow m) => CurlCmdLineOptions -> m CurlCmdLineOptions'
curlCmdLineOptionsConfigIn CurlCmdLineOptions {..} = do
  loginUser' <- pure $ mkPassword curlCmdLineLoginUser
  loginPIN' <- pure $ mkPassword curlCmdLineLoginPIN
  if isValidPassword defaultPasswordPolicy_ loginPIN'
    then pass
    else
      throwError $
        as $
          InvalidConfigParameters "The password from config is not valid. Maybe it is too short."
  loginUrl' <- URI.mkURI curlCmdLineLoginUrl
  uploadUrl' <- URI.mkURI curlCmdLineUploadUrl
  pure $
    CurlCmdLineOptions'
      { curlCmdLineLoginUser' = loginUser'
      , curlCmdLineLoginPIN' = loginPIN'
      , curlCmdLineLoginUrl' = loginUrl'
      , curlCmdLineUploadUrl' = uploadUrl'
      }

data JavaAnalyzerRunner = JavaAnalyzerRunner
  { field1 :: !Int
  , field2 :: !Text
  }
