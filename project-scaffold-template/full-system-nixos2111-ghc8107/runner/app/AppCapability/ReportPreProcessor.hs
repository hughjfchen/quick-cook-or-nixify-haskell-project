{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module implement the type Capability.ReportPreProcessor for the app
module AppCapability.ReportPreProcessor
  ( preProcessJavaCoreReport,
    preProcessHeapDumpReport,
  )
where

import AppError
import AppM
import As
import Capability.ReportPreProcessor
import Control.Monad.Catch
import Core.JavaAnalyzerRunner
import Core.Types
import Error
import Has
import Path
import Path.IO
import System.Process.Typed
import Utils

unCompressDump ::
  (MonadThrow m, MonadIO m) =>
  Path Abs Dir ->
  Path Abs File ->
  m ()
unCompressDump home rawFile = do
  ensureDir home
  runProcess_ $ setWorkingDir (toFilePath home) $ proc "jar" ["xf", toFilePath rawFile]

threadDumpExtP :: (MonadThrow m) => Path Abs File -> m Bool
threadDumpExtP f =
  splitExtension f <&> \(fname, fext) ->
    fext == ".txt"
      || fext == ".threaddump"
      || isScriptGenThreadDump fname

heapDumpExtP :: (MonadThrow m) => Path Abs File -> m Bool
heapDumpExtP f =
  splitExtension f <&> \(fname, fext) ->
    fext == ".phd"
      || fext == ".hprof"
      || isScriptGenHeapDump fname

gcExtP :: (MonadThrow m) => Path Abs File -> m Bool
gcExtP f =
  splitExtension f <&> \(fname, fext) ->
    fext == ".log"
      || isWASGCLog fname

preProcessDump' ::
  ( WithError err m,
    As err AppError,
    MonadThrow m,
    MonadIO m,
    MonadReader env m,
    Has OutputPath' env
  ) =>
  Path Rel File ->
  Path Rel Dir ->
  Path Abs File ->
  (OutputPath' -> SomeBase Dir) ->
  (Path Abs File -> m Bool) ->
  m (Path Abs File)
preProcessDump' _ dirSuffix rawFile getOrigHomeFn filterFileP = do
  processedAbsHome <- grab @OutputPath' >>= \p -> someDirToAbs $ appendDirToSomeDir (getOrigHomeFn p) dirSuffix
  unCompressDump processedAbsHome rawFile
  -- clean up the file(s) generated from the previous step to save disk space
  removeDirRecur $ parent rawFile
  findOneFileInDir processedAbsHome filterFileP

preProcessJavaCoreDump ::
  ( WithError err m,
    As err AppError,
    MonadThrow m,
    MonadIO m,
    MonadReader env m,
    Has OutputPath' env
  ) =>
  Path Rel File ->
  Path Rel Dir ->
  Path Abs File ->
  String ->
  m (Path Abs File)
preProcessJavaCoreDump file dirSuffix rawFile ".zip" = preProcessDump' file dirSuffix rawFile outputJCAPreProcessorHome' threadDumpExtP
preProcessJavaCoreDump file dirSuffix rawFile ".jar" = preProcessDump' file dirSuffix rawFile outputJCAPreProcessorHome' threadDumpExtP
preProcessJavaCoreDump _ _ rawFile ".txt" = pure rawFile
preProcessJavaCoreDump _ _ rawFile ".threaddump" = pure rawFile
preProcessJavaCoreDump _ _ _ ext = throwError $ as $ NoSupportedDumpFile $ "The upload file is not supported. It must be zip, jar, txt or threaddump but it is " <> toText ext <> "."

preProcessHeapDumpDump ::
  ( WithError err m,
    As err AppError,
    MonadThrow m,
    MonadIO m,
    MonadReader env m,
    Has OutputPath' env
  ) =>
  Path Rel File ->
  Path Rel Dir ->
  Path Abs File ->
  String ->
  m (Path Abs File)
preProcessHeapDumpDump file dirSuffix rawFile ".zip" = preProcessDump' file dirSuffix rawFile outputMATPreProcessorHome' heapDumpExtP
preProcessHeapDumpDump file dirSuffix rawFile ".jar" = preProcessDump' file dirSuffix rawFile outputMATPreProcessorHome' heapDumpExtP
preProcessHeapDumpDump _ _ rawFile ".phd" = pure rawFile
preProcessHeapDumpDump _ _ rawFile ".hprof" = pure rawFile
preProcessHeapDumpDump _ _ _ ext = throwError $ as $ NoSupportedDumpFile $ "The upload file is not supported. It must be zip, jar, phd or hprof but it is " <> toText ext <> "."

preProcessGCDump ::
  ( WithError err m,
    As err AppError,
    MonadThrow m,
    MonadIO m,
    MonadReader env m,
    Has OutputPath' env
  ) =>
  Path Rel File ->
  Path Rel Dir ->
  Path Abs File ->
  String ->
  m (Path Abs File)
preProcessGCDump file dirSuffix rawFile ".zip" = preProcessDump' file dirSuffix rawFile outputGCMVPreProcessorHome' gcExtP
preProcessGCDump file dirSuffix rawFile ".jar" = preProcessDump' file dirSuffix rawFile outputGCMVPreProcessorHome' gcExtP
preProcessGCDump _ _ rawFile ".log" = pure rawFile
preProcessGCDump _ _ _ ext = throwError $ as $ NoSupportedDumpFile $ "The upload file is not supported. It must be zip, jar, log but it is " <> toText ext <> "."

instance ReportPreProcessorM AppM' where
  preProcessJavaCoreReport file dirSuffix rawFile = do
    fileExtension rawFile >>= preProcessJavaCoreDump file dirSuffix rawFile
  preProcessHeapDumpReport file dirSuffix rawFile = do
    fileExtension rawFile >>= preProcessHeapDumpDump file dirSuffix rawFile
  preProcessGCReport file dirSuffix rawFile = do
    fileExtension rawFile >>= preProcessGCDump file dirSuffix rawFile
