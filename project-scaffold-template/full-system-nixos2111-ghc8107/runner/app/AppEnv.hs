{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

-- | This module define the Env for application
module AppEnv (
  Command (..),
  AppEnv (..),
  AppEnv' (..),
  configIn,
) where

import Has

import Core.Types

import OddJobs.Cli (CommonStartArgs (..), StopArgs (..))
import OddJobs.Types (Config (..))

import qualified Conferer as Conf
import Conferer.FromConfig

import qualified Database.PostgreSQL.Simple as PGS
import qualified FromConfig.ResourcePool as RP

import FromConfig.CommandPath ()
import FromConfig.CurlCmdLine ()
import FromConfig.GCMVCmdLine ()
import FromConfig.JCACmdLine ()
import FromConfig.MATCmdLine ()
import FromConfig.OddJobsConfig ()
import FromConfig.OddJobsStartArgs ()
import FromConfig.OddJobsStopArgs ()
import FromConfig.OutputPath ()
import FromConfig.PostgresqlSimple ()
import FromConfig.ResourcePool ()

import As
import Core.MyError
import Error (WithError)

import Control.Monad.Catch (MonadThrow)

data Command
  = Start
  | Stop
  | Status
  | Unknown
  deriving stock (Eq, Show, Read, Generic)
instance FromConfig Command where
  fromConfig = fetchFromConfigByRead

{- | following is just for example
 | you must put your own env
-}
data AppEnv = AppEnv
  { appEnvCommand :: Command
  , appEnvDatabase :: PGS.ConnectInfo
  , appEnvPool :: RP.PoolInfo
  , appEnvOddJobsStartArgs :: CommonStartArgs
  , appEnvOddJobsStopArgs :: StopArgs
  , appEnvOddJobsConfig :: Config
  , appEnvCmdPath :: CommandPath
  , appEnvOutputPath :: OutputPath
  , appEnvJCACmdLineOptions :: JCACmdLineOptions
  , appEnvMATCmdLineOptions :: MATCmdLineOptions
  , appEnvGCMVCmdLineOptions :: GCMVCmdLineOptions
  , appEnvCurlCmdLineOptions :: CurlCmdLineOptions
  }
  deriving stock (Generic)
  deriving (Has CommandPath) via Field "appEnvCmdPath" AppEnv
  deriving (Has OutputPath) via Field "appEnvOutputPath" AppEnv
  deriving (Has JCACmdLineOptions) via Field "appEnvJCACmdLineOptions" AppEnv
  deriving (Has MATCmdLineOptions) via Field "appEnvMATCmdLineOptions" AppEnv
  deriving (Has GCMVCmdLineOptions) via Field "appEnvGCMVCmdLineOptions" AppEnv
  deriving (Has CurlCmdLineOptions) via Field "appEnvCurlCmdLineOptions" AppEnv

data AppEnv' = AppEnv'
  { appEnvCmdPath' :: CommandPath'
  , appEnvOutputPath' :: OutputPath'
  , appEnvJCACmdLineOptions' :: JCACmdLineOptions'
  , appEnvMATCmdLineOptions' :: MATCmdLineOptions'
  , appEnvGCMVCmdLineOptions' :: GCMVCmdLineOptions'
  , appEnvCurlCmdLineOptions' :: CurlCmdLineOptions'
  }
  deriving stock (Generic)
  deriving (Has CommandPath') via Field "appEnvCmdPath'" AppEnv'
  deriving (Has OutputPath') via Field "appEnvOutputPath'" AppEnv'
  deriving (Has JCACmdLineOptions') via Field "appEnvJCACmdLineOptions'" AppEnv'
  deriving (Has MATCmdLineOptions') via Field "appEnvMATCmdLineOptions'" AppEnv'
  deriving (Has GCMVCmdLineOptions') via Field "appEnvGCMVCmdLineOptions'" AppEnv'
  deriving (Has CurlCmdLineOptions') via Field "appEnvCurlCmdLineOptions'" AppEnv'

instance FromConfig AppEnv

instance Conf.DefaultConfig AppEnv where
  configDef =
    AppEnv
      { appEnvCommand = Start
      , appEnvDatabase = Conf.configDef
      , appEnvPool = Conf.configDef
      , appEnvOddJobsStartArgs = Conf.configDef
      , appEnvOddJobsStopArgs = Conf.configDef
      , appEnvOddJobsConfig = Conf.configDef
      , appEnvCmdPath = Conf.configDef
      , appEnvOutputPath = Conf.configDef
      , appEnvJCACmdLineOptions = Conf.configDef
      , appEnvMATCmdLineOptions = Conf.configDef
      , appEnvGCMVCmdLineOptions = Conf.configDef
      , appEnvCurlCmdLineOptions = Conf.configDef
      }

{- | convert AppEnv to AppEnv', this function should be running
 | within the AppM monad
-}
configIn ::
  ( WithError err m
  , As err MyError
  , MonadThrow m
  , MonadReader env m
  , Has CommandPath env
  , Has OutputPath env
  , Has JCACmdLineOptions env
  , Has MATCmdLineOptions env
  , Has GCMVCmdLineOptions env
  , Has CurlCmdLineOptions env
  ) =>
  m AppEnv'
configIn = do
  cmd' <- grab @CommandPath >>= commandPathConfigIn
  output' <- grab @OutputPath >>= outputPathConfigIn
  jca' <- grab @JCACmdLineOptions >>= jcaCmdLineOptionsConfigIn
  mat' <- grab @MATCmdLineOptions >>= matCmdLineOptionsConfigIn
  gcmv' <- grab @GCMVCmdLineOptions >>= gcmvCmdLineOptionsConfigIn
  curl' <- grab @CurlCmdLineOptions >>= curlCmdLineOptionsConfigIn
  pure $
    AppEnv'
      { appEnvCmdPath' = cmd'
      , appEnvOutputPath' = output'
      , appEnvJCACmdLineOptions' = jca'
      , appEnvMATCmdLineOptions' = mat'
      , appEnvGCMVCmdLineOptions' = gcmv'
      , appEnvCurlCmdLineOptions' = curl'
      }
