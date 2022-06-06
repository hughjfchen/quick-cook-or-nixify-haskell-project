{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module implements the capability specified within the library src tree
-- | by providing the instances for the class defined within the library src tree
module AppCapability.JavaAnalyzerRunner
  ( generateJavaCoreReport,
    generateHeapDumpReport,
  )
where

import AppM
import Capability.JavaAnalyzerRunner
import Core.JavaAnalyzerRunner (appendDirToSomeDir)
import Core.Types
import Has
import Path
import Path.IO
import System.Process.Typed
import Utils

instance ReportGeneratorM AppM' where
  generateJavaCoreReport _ dirSuffix dumpFile = do
    cmdPaths <- grab @CommandPath'
    jcaCmdLineOpt <- grab @JCACmdLineOptions'
    parsedAbsHome <- grab @OutputPath' >>= \p -> someDirToAbs $ appendDirToSomeDir (outputJCAReportHome' p) dirSuffix
    parsedOutputFullPath <- resolveFile parsedAbsHome "index.html"
    -- runProcess_ $ proc "mkdir" ["-p", parsedOutputHome]
    ensureDir parsedAbsHome
    runProcess_ $
      proc
        (fromSomeFile $ cmdXvfbPath' cmdPaths)
        [ fromSomeFile $ cmdJavaPath' cmdPaths,
          "-Xmx" <> show (jcaCmdLineXmx' jcaCmdLineOpt) <> "M",
          "-jar",
          fromSomeFile $ cmdJCAPath' cmdPaths,
          toFilePath dumpFile,
          toFilePath parsedOutputFullPath
        ]
    -- clean up the generated file(s) from the previous step to save disk space.
    removeDirRecur $ parent dumpFile

    pure $ Report parsedOutputFullPath
  generateHeapDumpReport _ dirSuffix dumpFile = do
    cmdPaths <- grab @CommandPath'
    matCmdLineOpt <- grab @MATCmdLineOptions'
    parsedAbsHome <- grab @OutputPath' >>= \p -> someDirToAbs $ appendDirToSomeDir (outputMATReportHome' p) dirSuffix
    -- MAT doesn't support specify an output desination.
    -- It only generates the report under the same dir of the dump file
    -- So we mv the dump file to our desitination before run the MAT
    ensureDir parsedAbsHome
    copyFile dumpFile $ parsedAbsHome </> filename dumpFile
    runProcess_ $
      setWorkingDir (toFilePath parsedAbsHome) $
        proc
          (fromSomeFile $ cmdParseDumpShPath' cmdPaths)
          [ toFilePath $ parsedAbsHome </> filename dumpFile,
            "org.eclipse.mat.api:suspects",
            "-vmargs",
            "-Xmx" <> show (matCmdLineXmx' matCmdLineOpt) <> "M"
          ]

    -- clean up the file(s) generated from the previous step to save disk space
    removeDirRecur $ parent dumpFile
    -- TODO: maybe also clean up the generatd intermedia files generated from THIS step?
    -- NOTICE: not needed, because this directory would be deleted in the next step

    -- the output report file name is constructred according to the MAT report file generation rule.
    -- It may be different if use other parse command line of MAT. that is a implity dependency
    findOneFileInDir parsedAbsHome (pure . isHeapDumpReport "_Leak_Suspects.zip") <&> Report
  generateGCReport _ dirSuffix dumpFile = do
    cmdPaths <- grab @CommandPath'
    gcmvCmdLineOpt <- grab @GCMVCmdLineOptions'
    parsedAbsHome <- grab @OutputPath' >>= \p -> someDirToAbs $ appendDirToSomeDir (outputGCMVReportHome' p) dirSuffix
    parsedRelReportDir <- parseRelDir "Report"
    parsedOutputFullPath <- resolveFile (parsedAbsHome </> parsedRelReportDir) "index.html"
    -- runProcess_ $ proc "mkdir" ["-p", parsedOutputHome]
    ensureDir parsedAbsHome
    runProcess_ $
      proc
        (fromSomeFile $ cmdXvfbPath' cmdPaths)
        [ fromSomeFile $ cmdGCMVPath' cmdPaths,
          "-vm",
          fromSomeDir $ gcmvJVMPath' gcmvCmdLineOpt,
          "-application",
          "com.ibm.java.diagnostics.visualizer.headless.application",
          "-f",
          toFilePath dumpFile,
          "-o",
          toFilePath parsedAbsHome,
          "-p",
          fromSomeFile $ gcmvPreference' gcmvCmdLineOpt,
          "-vmargs",
          "-Xmx" <> show (gcmvCmdLineXmx' gcmvCmdLineOpt) <> "M"
        ]
    -- clean up the generated file(s) from the previous step to save disk space
    removeFile dumpFile

    pure $ Report parsedOutputFullPath
