{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DerivingVia #-}

-- | This module define the Env for application
module AppEnv
  ( AppEnv (..),
    initAppEnvFromEnv,
    initAppEnvFromCmd,
    isAppEnvNull,
    initAppEnv,
    env2Target,
    envs2Targets,
    nullAppEnv,
  )
where

import Core.MyError (MyError)
import Core.NginxCmdLineParser
import Core.Types
import Has
import Propellor.Base (getEnvDefault, giveup, proc, readCreateProcessTrailed, shell)

-- | following is just for example
-- | you must put your own env
data AppEnv = AppEnv
  { appEnvNginxPID :: Maybe Text,
    appEnvNginxServiceUnitName :: Maybe Text,
    appEnvNginxFullPathExe :: MyNginxFullPathExe,
    appEnvNginxPrefix :: MyNginxPrefix,
    appEnvNginxUser :: MyNginxUser,
    appEnvNginxConfPath :: MyNginxConfPath,
    appEnvNginxCmdLine :: MyNginxCmdLine,
    appEnvNginxConfigFrom :: ConfigFrom
  }
  deriving stock (Eq, Ord, Show, Typeable, Generic)
  deriving (Has MyNginxFullPathExe) via Field "appEnvNginxFullPathExe" AppEnv
  deriving (Has MyNginxPrefix) via Field "appEnvNginxPrefix" AppEnv
  deriving (Has MyNginxUser) via Field "appEnvNginxUser" AppEnv
  deriving (Has MyNginxConfPath) via Field "appEnvNginxConfPath" AppEnv

nullAppEnv :: IO AppEnv
nullAppEnv =
  pure $
    AppEnv
      { appEnvNginxPID = Nothing,
        appEnvNginxServiceUnitName = Nothing,
        appEnvNginxFullPathExe = MyNginxFullPathExe "",
        appEnvNginxPrefix = MyNginxPrefix "",
        appEnvNginxUser = MyNginxUser "",
        appEnvNginxConfPath = MyNginxConfPath "",
        appEnvNginxCmdLine = MyNginxCmdLine "",
        appEnvNginxConfigFrom = FromDefault
      }

initAppEnvFromEnv :: IO [AppEnv]
initAppEnvFromEnv = do
  ngPID <- lookupEnv "MY_NGINX_PID"
  ngSrv <- lookupEnv "MY_NGINX_SERVICE"
  ngFullPathExe <- getEnvDefault "MY_NGINX_HOME" ""
  ngPrefix <- getEnvDefault "MY_NGINX_PREFIX" ""
  ngUser <- getEnvDefault "MY_NGINX_USER" ""
  ngConfPath <- getEnvDefault "MY_NGINX_CONF_PATH" ""
  ngCmdLine <- getEnvDefault "MY_NGINX_CMD_LINE" ""
  ngConfFrom <- getEnvDefault "MY_NGINX_CONFIG_FROM" "FromDefault"
  pure
    [ AppEnv
        { appEnvNginxPID = toText <$> ngPID,
          appEnvNginxServiceUnitName = toText <$> ngSrv,
          appEnvNginxFullPathExe = MyNginxFullPathExe ngFullPathExe,
          appEnvNginxPrefix = MyNginxPrefix ngPrefix,
          appEnvNginxUser = MyNginxUser ngUser,
          appEnvNginxCmdLine = MyNginxCmdLine ngCmdLine,
          appEnvNginxConfPath = MyNginxConfPath ngConfPath,
          appEnvNginxConfigFrom = fromString ngConfFrom
        }
    ]

hostPIDOnly :: String -> IO Bool
hostPIDOnly pid = do
  let cmd =
        shell $
          intercalate
            "|"
            [ "cat " <> "/proc/" <> pid <> "/cgroup",
              "grep -v grep",
              "awk -F':' '{print $3}'",
              "awk -F'/' '{print $2}'",
              "grep -v '^$'",
              "grep -v 'system.slice'",
              "grep -v 'user.slice'",
              "cat"
            ]
  pidCG <- readCreateProcessTrailed cmd ""
  pure $ null pidCG

nginxPID :: IO [String]
nginxPID = do
  ngPs <-
    flip readCreateProcessTrailed "" $
      shell $
        intercalate
          "|"
          [ "ps -eo pid,cmd",
            "grep -v 'awk '", -- exclude the awk command
            "grep -v ' -?'", -- exclude all the transient command line flags
            "grep -v ' -h'",
            "grep -v ' -t'",
            "grep -v ' -T'",
            "grep -v ' -v'",
            "grep -v ' -V'",
            "grep -v ' -q'",
            "grep -v ' -s'",
            "awk '/nginx: master process/ {print $1\"/\"$5}'",
            "awk -F'/' '$NF==\"openresty\" || $NF==\"nginx\" {print $1}'"
          ]
  let pids = fmap toString $ lines $ toText ngPs
  filterM hostPIDOnly pids

getServiceFromPID :: String -> IO (Maybe String)
getServiceFromPID "" = pure Nothing
getServiceFromPID pid = do
  ngSvc <- readCreateProcessTrailed getServiceFromPIDCmd ""
  pure $ bool (Just ngSvc) Nothing $ null ngSvc
  where
    getServiceFromPIDCmd =
      shell $
        intercalate
          "|"
          [ intercalate
              " "
              [ "systemctl",
                "status",
                "-n0",
                pid,
                "2>/dev/null"
              ],
            "head -n1",
            "awk '{print $2}'"
          ]

nginxFullPathFromPID :: String -> IO FilePath
nginxFullPathFromPID pid =
  flip readCreateProcessTrailed "" $
    proc "readlink" [intercalate "/" ["", "proc", pid, "exe"]]

nginxCmdLineFromPID :: String -> IO FilePath
nginxCmdLineFromPID pid =
  flip readCreateProcessTrailed "" $
    shell $
      intercalate
        "|"
        [ "cat " <> "/proc/" <> pid <> "/cmdline",
          "tr -d '\\000'"
        ]

nginxDefault :: String -> FilePath -> IO String
nginxDefault thepattern ngCmd =
  flip readCreateProcessTrailed "" $
    shell $
      intercalate
        "|"
        [ ngCmd <> " -h 2>&1",
          "awk -F'default: ' '/" <> thepattern <> "/ {print $NF}'",
          "cut -d')' -f1"
        ]

nginxPrefixDefault :: FilePath -> IO String
nginxPrefixDefault = nginxDefault " -p prefix "

nginxConfPathDefault :: FilePath -> IO String
nginxConfPathDefault = nginxDefault " -c filename "

appEnvFromParsedCmd :: Maybe Text -> FilePath -> FilePath -> String -> Either MyError NginxCmd -> IO AppEnv
appEnvFromParsedCmd _ _ _ _ (Left err) = giveup $ show err
appEnvFromParsedCmd pid ngCmd ngCmdLine ngU (Right ngCmd') = do
  ngDefPre <- nginxPrefixDefault ngCmd
  ngDefConfPath <- nginxConfPathDefault ngCmd
  ngSvc <- getServiceFromPID $ toString $ fromMaybe "" pid

  pure $
    AppEnv
      { appEnvNginxPID = pid,
        appEnvNginxServiceUnitName = toText <$> ngSvc,
        appEnvNginxFullPathExe = MyNginxFullPathExe ngCmd,
        appEnvNginxCmdLine = MyNginxCmdLine ngCmdLine,
        appEnvNginxPrefix = fromMaybe (MyNginxPrefix ngDefPre) $ ngxCmdPrefix ngCmd',
        appEnvNginxConfPath =
          fromMaybe (MyNginxConfPath ngDefConfPath) $
            ngxCmdConfPath ngCmd',
        appEnvNginxUser = MyNginxUser ngU,
        appEnvNginxConfigFrom = bool FromCmdLine FromDefault $ isNothing $ ngxCmdConfPath ngCmd'
      }

cmdAppEnv :: String -> IO AppEnv
cmdAppEnv nPID = do
  ngCmd <- nginxFullPathFromPID nPID
  fullCmd <- nginxCmdLineFromPID nPID
  ngUserToBe <- getEnvDefault "MY_NGINX_USER_TO_BE" "root"
  let ngParsedCmd = parseNginxCmd . toText $ fullCmd
  appEnvFromParsedCmd (Just $ toText nPID) ngCmd fullCmd ngUserToBe ngParsedCmd

initAppEnvFromCmd :: IO [AppEnv]
initAppEnvFromCmd = do
  ngxPID <- nginxPID
  bool (sequence $ cmdAppEnv <$> ngxPID) (pure []) $ null ngxPID

initAppEnv :: IO [AppEnv]
initAppEnv = do
  aeCmd <- initAppEnvFromCmd
  aeEnv <- initAppEnvFromEnv
  pure $ filter isAppEnvNull $ aeCmd <> aeEnv

isAppEnvNull :: AppEnv -> Bool
isAppEnvNull
  AppEnv
    { appEnvNginxPrefix = MyNginxPrefix {getMyNginxPrefix = p},
      appEnvNginxConfPath = MyNginxConfPath {getMyNginxConfPath = c}
    } = not (null p) && not (null c)

env2Target :: AppEnv -> Target
env2Target AppEnv {..} =
  Target
    { targetPid = appEnvNginxPID,
      targetServiceUnitName = appEnvNginxServiceUnitName,
      targetFullPathExe = appEnvNginxFullPathExe,
      targetCmdLine = appEnvNginxCmdLine,
      targetPrefix = appEnvNginxPrefix,
      targetConfPath = appEnvNginxConfPath,
      targetUser = appEnvNginxUser,
      targetConfigFrom = appEnvNginxConfigFrom
    }

envs2Targets :: [AppEnv] -> [Target]
envs2Targets aes = env2Target <$> aes
