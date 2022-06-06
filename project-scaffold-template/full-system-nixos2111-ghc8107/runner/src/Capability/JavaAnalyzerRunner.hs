{-# LANGUAGE FlexibleContexts #-}

-- | This module define specs for domain business actions
module Capability.JavaAnalyzerRunner (
  ReportGeneratorM (..),
) where

import As
import Core.MyError
import Error
import Has

import Core.Types

import Path

import Control.Monad.Catch (MonadThrow)

class (Monad m) => ReportGeneratorM m where
  generateJavaCoreReport ::
    ( WithError err m
    , As err MyError
    , MonadReader env m
    , Has CommandPath' env
    , Has OutputPath' env
    , Has JCACmdLineOptions' env
    ) =>
    Path Rel File ->
    Path Rel Dir ->
    Path Abs File ->
    m Report
  generateHeapDumpReport ::
    ( WithError err m
    , As err MyError
    , MonadThrow m
    , MonadReader env m
    , Has CommandPath' env
    , Has OutputPath' env
    , Has MATCmdLineOptions' env
    ) =>
    Path Rel File ->
    Path Rel Dir ->
    Path Abs File ->
    m Report
  generateGCReport ::
    ( WithError err m
    , As err MyError
    , MonadThrow m
    , MonadReader env m
    , Has CommandPath' env
    , Has OutputPath' env
    , Has MATCmdLineOptions' env
    ) =>
    Path Rel File ->
    Path Rel Dir ->
    Path Abs File ->
    m Report
