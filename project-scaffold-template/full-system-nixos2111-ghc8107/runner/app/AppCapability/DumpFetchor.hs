-- | This module implement the type Capability.DumpFetcher for App
{-# OPTIONS_GHC -fno-warn-orphans #-}
module AppCapability.DumpFetchor
  ( fetchDump
  ) where

import As
import Has
import Error

import Core.Types
import Core.MyError
import Core.JavaAnalyzerRunner

import Capability.DumpFetchor

import AppM

import System.Process.Typed

import Path
import Path.IO

import Utils

import qualified Text.URI as URI

instance DumpFetchorM AppM' where
  fetchDump file dirSuffix (Local' path) = do
    outDumpHome <- grab @OutputPath' >>= \p -> someDirToAbs $ appendDirToSomeDir (outputFetchedDumpHome' p) dirSuffix
    ensureDir outDumpHome
    someFileToAbs path >>= \from -> copyFile from $ outDumpHome </> file
    pure $ outDumpHome </> file
  fetchDump file dirSuffix (HttpUrl' url) = do
    outDumpHome <- grab @OutputPath' >>= \p -> someDirToAbs $ appendDirToSomeDir (outputFetchedDumpHome' p) dirSuffix
    ensureDir outDumpHome
    cmdPaths <- grab @CommandPath'
    runProcess_ $ proc (fromSomeFile $ cmdWgetPath' cmdPaths) ["--quiet"
                                                              , "--no-proxy"
                                                              , "--continue"
                                                              , "--output-document=" <> toFilePath (outDumpHome </> file)
                                                              , URI.renderStr url
                                                              ]
    pure $ outDumpHome </> file
  fetchDump _ _ (S3Path' _) = throwError $ as $ NotImplementedYet "Should not get to here."
