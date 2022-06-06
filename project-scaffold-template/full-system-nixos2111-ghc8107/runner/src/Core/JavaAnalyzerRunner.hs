{-# LANGUAGE RankNTypes #-}

{- | This module is the core business logic
Copyright: (c) 2021 Hugh JF Chen
SPDX-License-Identifier: MIT
Maintainer: Hugh JF Chen <hugh.jf.chen@gmail.com>

Core business logic here.
-}
module Core.JavaAnalyzerRunner (
  genJavaCoreReport,
  genHeapDumpReport,
  genGCReport,
  fileFromLocation',
  fileNameAsRelDir,
  -- utilities excerpted from path packages
  prjSomeBase,
  mapSomeBase,
  appendDirToSomeDir,
  appendFileToSomeDir,
  appendDirAndFileToSomeDir,
) where

import Core.Types

import As
import Capability.DumpFetchor
import Capability.JavaAnalyzerRunner
import Capability.ReportPostProcessor
import Capability.ReportPreProcessor
import Capability.ReportUploader
import Core.MyError
import Error
import Has

import Path

import Text.URI (URI (..))
import qualified Text.URI as URI

import Control.Monad.Catch (MonadThrow)

-- extract from path 0.9.2

{- | Helper to project the contents out of a SomeBase object.

 >>> prjSomeBase toFilePath (Abs [absfile|/foo/bar/cow.moo|]) == "/foo/bar/cow.moo"
-}
prjSomeBase :: (forall b. Path b t -> a) -> SomeBase t -> a
prjSomeBase f = \case
  Abs a -> f a
  Rel r -> f r

{- | Helper to apply a function to the SomeBase object

 >>> mapSomeBase parent (Abs [absfile|/foo/bar/cow.moo|]) == Abs [absdir|"/foo/bar"|]
-}
mapSomeBase :: (forall b. Path b t -> Path b t') -> SomeBase t -> SomeBase t'
mapSomeBase f = \case
  Abs a -> Abs $ f a
  Rel r -> Rel $ f r

fileFromLocation' :: (WithError err m, As err MyError, MonadThrow m) => Location' -> m (Path Rel File)
fileFromLocation' (Local' f) = pure $ prjSomeBase filename f
fileFromLocation' (HttpUrl' (URI _ _ Nothing _ _)) = throwError $ as $ InvalidLocation "Path within the URI could not be empty."
fileFromLocation' (HttpUrl' (URI _ _ (Just (True, _)) _ _)) = throwError $ as $ InvalidLocation "The URI path must point to a specifice file."
fileFromLocation' (HttpUrl' (URI _ _ (Just (False, paths)) _ _)) = parseRelFile $ toString $ URI.unRText $ last paths
fileFromLocation' (S3Path' _) = throwError $ as $ InvalidLocation "Should not get to here because it is not implemented yet."

fileNameAsRelDir :: (MonadThrow m) => Path Rel File -> m (Path Rel Dir)
fileNameAsRelDir file = parseRelDir $ toFilePath file

appendDirToSomeDir :: SomeBase Dir -> Path Rel Dir -> SomeBase Dir
appendDirToSomeDir dir file = mapSomeBase (flip (</>) file) dir

appendFileToSomeDir :: SomeBase Dir -> Path Rel File -> SomeBase File
appendFileToSomeDir dir file = mapSomeBase (flip (</>) file) dir

-- | return a tuple with outputHome and output file fullpath
appendDirAndFileToSomeDir :: SomeBase Dir -> Path Rel Dir -> Path Rel File -> (SomeBase Dir, SomeBase File)
appendDirAndFileToSomeDir orig dir file =
  let home' = appendDirToSomeDir orig dir
      file' = appendFileToSomeDir home' file
   in (home', file')

genJavaCoreReport ::
  ( WithError err m
  , As err MyError
  , MonadThrow m
  , MonadReader env m
  , Has CommandPath' env
  , Has OutputPath' env
  , Has JCACmdLineOptions' env
  , Has MATCmdLineOptions' env
  , DumpFetchorM m
  , ReportPreProcessorM m
  , ReportGeneratorM m
  , ReportPostProcessorM m
  , ReportUploaderM m
  ) =>
  Int ->
  Location ->
  m ()
genJavaCoreReport jobId l = genReport jobId l preProcessJavaCoreReport generateJavaCoreReport postProcessJavaCoreReport

genHeapDumpReport ::
  ( WithError err m
  , As err MyError
  , MonadThrow m
  , MonadReader env m
  , Has CommandPath' env
  , Has OutputPath' env
  , Has MATCmdLineOptions' env
  , Has JCACmdLineOptions' env
  , DumpFetchorM m
  , ReportPreProcessorM m
  , ReportGeneratorM m
  , ReportPostProcessorM m
  , ReportUploaderM m
  ) =>
  Int ->
  Location ->
  m ()
genHeapDumpReport jobId l = genReport jobId l preProcessHeapDumpReport generateHeapDumpReport postProcessHeapDumpReport

genGCReport ::
  ( WithError err m
  , As err MyError
  , MonadThrow m
  , MonadReader env m
  , Has CommandPath' env
  , Has OutputPath' env
  , Has JCACmdLineOptions' env
  , Has MATCmdLineOptions' env
  , DumpFetchorM m
  , ReportPreProcessorM m
  , ReportGeneratorM m
  , ReportPostProcessorM m
  , ReportUploaderM m
  ) =>
  Int ->
  Location ->
  m ()
genGCReport jobId l = genReport jobId l preProcessGCReport generateGCReport postProcessGCReport

genReport ::
  ( WithError err m
  , As err MyError
  , MonadThrow m
  , MonadReader env m
  , Has CommandPath' env
  , Has OutputPath' env
  , Has JCACmdLineOptions' env
  , Has MATCmdLineOptions' env
  , DumpFetchorM m
  , ReportGeneratorM m
  , ReportPostProcessorM m
  , ReportUploaderM m
  ) =>
  Int ->
  Location ->
  (Path Rel File -> Path Rel Dir -> Path Abs File -> m (Path Abs File)) ->
  (Path Rel File -> Path Rel Dir -> Path Abs File -> m Report) ->
  (Path Rel File -> Path Rel Dir -> Report -> m (Path Abs File)) ->
  m ()
genReport jobId l preProcessor analyzer postProcessor = do
  l' <- flowIn l
  file <- fileFromLocation' l'
  fileAsDirSuffix <- fileNameAsRelDir file
  fetchDump file fileAsDirSuffix l' >>= preProcessor file fileAsDirSuffix >>= analyzer file fileAsDirSuffix
    >>= postProcessor file fileAsDirSuffix
    >>= uploadReport jobId
