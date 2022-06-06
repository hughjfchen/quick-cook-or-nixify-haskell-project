{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | This is the entry point to the main program
module Main (main) where

import AppCapability.DumpFetchor ()
import AppCapability.JavaAnalyzerRunner ()
import AppCapability.ReportPostProcessor ()
import AppCapability.ReportPreProcessor ()
import AppCapability.ReportUploader ()
import AppEnv
import AppM
-- import OddJobs.Migrations (createJobTable)

-- Note: It is not necessary to use fast-logger. You can use any logging library
-- that can give you a logging function in the IO monad.

import qualified Conferer as Conf
import Core.JavaAnalyzerRunner
import Core.Types
import qualified Data.ByteString as BS
import Data.Pool (Pool, createPool, destroyAllResources)
import qualified Database.PostgreSQL.Simple as PGS
import FromConfig.ResourcePool (PoolInfo (..))
import OddJobs.Cli (CliType (..), CommonStartArgs (startPidFile), defaultStartCommand, defaultStopCommand)
import OddJobs.ConfigBuilder (defaultJobType, defaultLogStr, defaultTimedLogger, mkConfig)
import OddJobs.Job (Config (..), Job (..), startJobRunner, throwParsePayload)
import qualified OddJobs.Types as OT
import System.FilePath (takeDirectory, (</>))
import System.Log.FastLogger (LogType' (..), defaultBufSize, withTimedFastLogger)
import System.Log.FastLogger.Date (newTimeCache, simpleTimeFormat)
import UnliftIO (MonadUnliftIO, bracket, withRunInIO)

-- | Copied from the OddJobs to config the pool. Convenience function to create a DB connection-pool with some sensible
-- defaults. Please see the source-code of this function to understand what it's
-- doing.
withConnectionPoolWithConfig ::
  (MonadUnliftIO m) =>
  Either BS.ByteString PGS.ConnectInfo ->
  PoolInfo ->
  (Pool PGS.Connection -> m a) ->
  m a
withConnectionPoolWithConfig connConfig poolInfo action = withRunInIO $ \runInIO ->
  bracket poolCreator destroyAllResources (runInIO . action)
  where
    poolCreator = liftIO $
      case connConfig of
        Left connString ->
          createPool (PGS.connectPostgreSQL connString) PGS.close (poolStripe poolInfo) (poolIdleTime poolInfo) (poolSize poolInfo)
        Right connInfo ->
          createPool (PGS.connect connInfo) PGS.close (poolStripe poolInfo) (poolIdleTime poolInfo) (poolSize poolInfo)

myJobRunner :: AppEnv' -> Job -> IO ()
myJobRunner env' job =
  throwParsePayload job >>= \case
    ParseJavaCore javaCorePath ->
      runAppM' env' $ genJavaCoreReport (jobId job) javaCorePath
    ParseHeapDump heapDumpPath ->
      runAppM' env' $ genHeapDumpReport (jobId job) heapDumpPath
    ParseGC gcLogPath ->
      runAppM' env' $ genGCReport (jobId job) gcLogPath

setOJDefaultTimeout :: AppEnv -> OT.Config -> OT.Config
setOJDefaultTimeout appEnv inConfig = inConfig {cfgDefaultJobTimeout = cfgDefaultJobTimeout $ appEnvOddJobsConfig appEnv}

main :: IO ()
main = do
  config <- Conf.mkConfig "JavaAnalyzerRunner"
  appEnv <- Conf.fetch config :: IO AppEnv

  -- check config data
  -- print $ appEnvDatabase appEnv
  -- print $ appEnvPool appEnv
  -- print $ appEnvOddJobsStartArgs appEnv
  -- print $ appEnvOddJobsStopArgs appEnv
  -- print $ appEnvOddJobsConfig appEnv
  -- print $ cfgTableName $ appEnvOddJobsConfig appEnv
  appEnv' <- runAppM appEnv configIn

  case appEnvCommand appEnv of
    Start -> defaultStartCommand (appEnvOddJobsStartArgs appEnv) Nothing $ CliOnlyJobRunner {cliStartJobRunner = startJobMonitor appEnv appEnv'}
    Stop -> defaultStopCommand (appEnvOddJobsStopArgs appEnv)
    Status -> error "The Status command is not implemented yet."
    Unknown -> error "Unknown command, it must be Start, Stop or Status."
  where
    -- A callback-within-callback function. If the commands-line args contain a
    -- `start` command, this function will be called. Once this function has
    -- constructed the 'Config' (which requires setting up a logging function,
    -- and a DB pool) it needs to execute the `callback` function that is passed
    -- to it.
    startJobMonitor theEnv theEnv' overrideFn =
      -- a utility function provided by `OddJobs.ConfigBuilder` which ensures
      -- that the DB pool is gracefully destroyed upon shutdown.
      withConnectionPoolWithConfig (Right $ appEnvDatabase theEnv) (appEnvPool theEnv) $ \dbPool -> do
        -- setup db
        -- withResource dbPool $ \conn -> createJobTable conn $ cfgTableName $ appEnvOddJobsConfig theEnv
        -- Boilerplate code to setup a TimedFastLogger (from the fast-logger library)
        tcache <- newTimeCache simpleTimeFormat
        withTimedFastLogger tcache (LogFileNoRotate (takeDirectory (startPidFile $ appEnvOddJobsStartArgs theEnv) </> "my-job-runner.log") defaultBufSize) $ \logger -> do
          -- Using the default string-based logging provided by
          -- `OddJobs.ConfigBuilder`. If you want to actually use
          -- structured-logging you'll need to define your own logging function.
          let jobLogger = defaultTimedLogger logger (defaultLogStr defaultJobType)
          startJobRunner $ mkConfig jobLogger (cfgTableName $ appEnvOddJobsConfig theEnv) dbPool (cfgConcurrencyControl $ appEnvOddJobsConfig theEnv) (myJobRunner theEnv') (overrideFn . setOJDefaultTimeout theEnv)
