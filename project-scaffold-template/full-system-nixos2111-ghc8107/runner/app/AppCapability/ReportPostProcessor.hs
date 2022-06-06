{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module implement the type Capability.ReportPostProcessor for the app
module AppCapability.ReportPostProcessor
  ( postProcessJavaCoreReport,
    postProcessHeapDumpReport,
  )
where

import AppM
import Capability.ReportPostProcessor
import Core.JavaAnalyzerRunner
import Core.Types
import Has
import Path
import Path.IO
import System.Process.Typed
import Utils

instance ReportPostProcessorM AppM' where
  postProcessJavaCoreReport file dirSuffix (Report x) = do
    -- pack the generated javacore html and gif into a tgz archieve
    outputPaths <- grab @OutputPath'
    -- the parseOutput is constructred according to the MAT report file generation rule
    -- that is a implity dependency
    processedAbsHome <- someDirToAbs $ appendDirToSomeDir (outputJCAPostProcessorHome' outputPaths) dirSuffix
    processedFile <- replaceExtension ".tgz" file <&> (</>) processedAbsHome
    ensureDir processedAbsHome
    (_, parsedOutFiles) <- listDirRecurRel $ parent x
    runProcess_ $
      setWorkingDir (toFilePath $ parent x) $
        proc "tar" $
          ["zcf", toFilePath processedFile]
            <> (toFilePath <$> parsedOutFiles)
    -- clean up the file(s) generated from the previous step to save disk space
    removeDirRecur $ parent x

    pure processedFile
  postProcessHeapDumpReport file dirSuffix (Report x) = do
    -- have to unpack the generated zip file and repack to tgz
    outputPaths <- grab @OutputPath'
    processedAbsHome <- someDirToAbs $ appendDirToSomeDir (outputMATPostProcessorHome' outputPaths) dirSuffix
    processedUnpackAbsDir <- (</>) processedAbsHome <$> parseRelDir "unpack_zip_for_repack_to_tgz"
    processedFile <- (</>) processedAbsHome <$> replaceExtension ".tgz" file
    ensureDir processedAbsHome
    ensureDir processedUnpackAbsDir
    runProcess_ $ setWorkingDir (toFilePath processedUnpackAbsDir) $ proc "jar" ["xf", toFilePath x]
    (_, zipFilesList) <- listDirRecurRel processedUnpackAbsDir
    runProcess_ $
      setWorkingDir (toFilePath processedUnpackAbsDir) $
        proc "tar" $
          ["zcf", toFilePath processedFile]
            <> (toFilePath <$> zipFilesList)
    removeDirRecur processedUnpackAbsDir

    -- clean up the fiile(s) generated from the previous step to save disk space
    removeDirRecur $ parent x

    pure processedFile

  postProcessGCReport file dirSuffix (Report x) = do
    -- pack the generated javacore html and gif into a tgz archieve
    outputPaths <- grab @OutputPath'
    -- the parseOutput is constructred according to the MAT report file generation rule
    -- that is a implity dependency
    processedAbsHome <- someDirToAbs $ appendDirToSomeDir (outputGCMVPostProcessorHome' outputPaths) dirSuffix
    processedFile <- replaceExtension ".tgz" file <&> (</>) processedAbsHome
    ensureDir processedAbsHome
    (_, parsedOutFiles) <- listDirRecurRel $ parent x
    runProcess_ $
      setWorkingDir (toFilePath $ parent x) $
        proc "tar" $
          ["zcf", toFilePath processedFile]
            <> (toFilePath <$> parsedOutFiles)
    -- clean up the fiile(s) generated from the previous step to save disk space
    removeDirRecur $ parent x

    pure processedFile
