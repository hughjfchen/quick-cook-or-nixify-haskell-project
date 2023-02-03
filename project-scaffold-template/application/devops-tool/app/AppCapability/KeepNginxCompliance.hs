{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module implements the capability specified within the library src tree
-- | by providing the instances for the class defined within the library src tree
module AppCapability.KeepNginxCompliance
  ( comply,
  )
where

import AppM
import Capability.KeepNginxCompliance
import Core.NginxConfig
import Core.Types
import qualified Core.Types as MyT
import Propellor
import Propellor.Base
  ( debug,
    infoMessage,
    isAbsolute,
    replaceExtension,
    takeDirectory,
    takeFileName,
    warningMessage,
    (</>),
  )
import Propellor.Engine
import Propellor.Property.File (hasContent)
import Propellor.Property.Service (ServiceName)
import Propellor.Property.Systemd (daemonReloaded, enabled)
import Propellor.Property.User
import Propellor.Types.Core (IsProp (toChildProperty))
import Propellor.Types.Info (IsInfo (propagateInfo), PropagateInfo (PropagateInfo))
import Propellor.Types.ResultCheck
  ( PrevCheckResult (CarryOn, NoNeedToCarryOn),
    prevCheck,
    prevCheckResult2Result,
  )
import Propellor.Utilities
import System.FilePath (splitDirectories)
import System.FilePath.Glob (compile, match)

newtype NginxDefAccessLogPath = NginxDefAccessLogPath
  { getNginxDefAccessLogPath :: FilePath
  }
  deriving stock (Eq, Ord, Show, Typeable, Generic)
  deriving newtype (Semigroup, Monoid)

instance IsInfo NginxDefAccessLogPath where
  propagateInfo _ = PropagateInfo False

newtype NginxDefErrorLogPath = NginxDefErrorLogPath
  { getNginxDefErrorLogPath :: String
  }
  deriving stock (Eq, Ord, Show, Typeable, Generic)
  deriving newtype (Semigroup, Monoid)

instance IsInfo NginxDefErrorLogPath where
  propagateInfo _ = PropagateInfo False

newtype NginxConfFiles = NginxConfFiles {getNginxConfFiles :: [FilePath]}
  deriving stock (Eq, Ord, Show, Typeable, Generic)
  deriving newtype (Semigroup, Monoid)

instance IsInfo NginxConfFiles where
  propagateInfo _ = PropagateInfo False

ngDefAccessLogPathInfoProp :: FilePath -> Target -> Property (HasInfo + Linux)
ngDefAccessLogPathInfoProp defALP _ =
  tightenTargets $
    pureInfoProperty "nginx default accesslog path" $
      NginxDefAccessLogPath defALP

getNginxDefAccessLogPathInfo :: Propellor NginxDefAccessLogPath
getNginxDefAccessLogPathInfo = askInfo

ngDefErrorLogPathInfoProp :: FilePath -> Target -> Property (HasInfo + Linux)
ngDefErrorLogPathInfoProp defELP _ =
  tightenTargets $
    pureInfoProperty "nginx default errorlog path " $
      NginxDefErrorLogPath defELP

getNginxDefErrorLogPathInfo :: Propellor NginxDefErrorLogPath
getNginxDefErrorLogPathInfo = askInfo

ngConfFilesInfoProp :: NginxConfFiles -> Property (HasInfo + Linux)
ngConfFilesInfoProp nxCFiles =
  tightenTargets $
    pureInfoProperty "nginx conf files list" nxCFiles

getNginxConfFilesInfo :: Propellor NginxConfFiles
getNginxConfFilesInfo = askInfo

myUnlines :: [String] -> String
myUnlines = toString . unlines . fmap toText

myUnwords :: [String] -> String
myUnwords = toString . unwords . fmap toText

ngServiceName :: Target -> FilePath
ngServiceName Target {..} =
  intercalate "-" $
    (:) "nginx" $
      Propellor.Utilities.tail $
        splitDirectories $
          flip replaceExtension ".service" $
            getMyNginxConfPath targetConfPath

getNgConfFilesCmd :: Target -> CreateProcess
getNgConfFilesCmd Target {..} =
  shell $
    intercalate
      "|"
      [ myUnwords $
          [ getMyNginxFullPathExe targetFullPathExe,
            "-p",
            getMyNginxPrefix targetPrefix,
            "-c",
            getMyNginxConfPath targetConfPath,
            "-T",
            "-q"
          ],
        "grep -v grep",
        "grep -v awk",
        "grep '# configuration file '",
        "awk '{print $NF}'",
        "awk -F':' '{print $1}'"
      ]

getNgConfigValueCmd :: String -> Target -> CreateProcess
getNgConfigValueCmd confKey Target {..} =
  shell $
    intercalate
      "|"
      [ myUnwords
          [ getMyNginxFullPathExe targetFullPathExe,
            "-V",
            "2>&1"
          ],
        "grep -v grep",
        "grep 'configure arguments: '",
        "tr -s ' ' '\n'",
        myUnwords
          [ "grep",
            "--",
            shellEscape confKey
          ],
        "awk -F'=' '{print $2}'"
      ]

ngCheckPathOwner :: FilePath -> String -> Propellor PrevCheckResult
ngCheckPathOwner path u = do
  ownPathCount <-
    liftIO $
      flip readCreateProcessTrailed "" $
        shell $
          myUnwords
            [ "find",
              path,
              "! -user",
              u,
              "|",
              "wc -l"
            ]
  bool (pure CarryOn) (pure $ NoNeedToCarryOn "already has correct owner") $
    ownPathCount == "0"

ngCheckPrefixOwner :: Target -> Propellor PrevCheckResult
ngCheckPrefixOwner Target {..} =
  ngCheckPathOwner (getMyNginxPrefix targetPrefix) (getMyNginxUser targetUser)

ngChangePathOwner :: FilePath -> String -> Propellor Result
ngChangePathOwner path u =
  liftIO $ do
    warningMessage $
      myUnwords
        [ "Inappropriate changing the path",
          "ownership could cause severe security issue.",
          "Double check after applying this action"
        ]
    (Propellor.Group g) <- primaryGroup $ User u
    r1 <-
      boolSystemResultQuiet
        "chown"
        [ Param "-hR",
          Param $ u <> ":" <> g,
          Param path
        ]
    r2 <-
      boolSystemResultQuiet
        "chown"
        [ Param "-HR",
          Param $ u <> ":" <> g,
          Param path
        ]
    pure $ r1 <> r2

pathOwnerProp :: FilePath -> String -> Property (HasInfo + Linux)
pathOwnerProp path u =
  prevCheck (ngCheckPathOwner path u) $
    property
      ( myUnwords
          [ "set the dir ownership to",
            u,
            "for the path",
            path
          ]
      )
      $ ngChangePathOwner path u

ngPrefixOwnerProp :: Target -> Property (HasInfo + Linux)
ngPrefixOwnerProp Target {..} =
  prevCheck
    ( ngCheckPathOwner
        (getMyNginxConfPath targetConfPath)
        (getMyNginxUser targetUser)
    )
    $ property
      ( myUnwords
          [ "set the path ownership to",
            getMyNginxUser targetUser,
            "for nginx config file only",
            getMyNginxConfPath targetConfPath
          ]
      )
      $ ngChangePathOwner
        (getMyNginxConfPath targetConfPath)
        (getMyNginxUser targetUser)

checkNgMasterProcess :: String -> Target -> CreateProcess
checkNgMasterProcess retString Target {targetConfigFrom = FromCmdLine, ..} =
  shell $
    intercalate
      "|"
      [ myUnwords ["ps", "-eo", retString <> "," <> "cmd"],
        "grep -v grep",
        "grep -v awk",
        "grep 'nginx: master process'",
        -- myUnwords ["grep", shellEscape $ getMyNginxFullPathExe targetFullPathExe],
        myUnwords
          [ "grep",
            "--",
            shellEscape $ "-c " <> getMyNginxConfPath targetConfPath
          ],
        "awk '{print $1}'"
      ]
checkNgMasterProcess retString Target {targetConfigFrom = FromDefault} =
  shell $
    intercalate
      "|"
      [ myUnwords ["ps", "-eo", retString <> "," <> "cmd"],
        "grep -v grep",
        "grep -v awk",
        "grep 'nginx: master process'",
        "grep -v ' -p '",
        "grep -v ' -c '",
        "awk '{print $1}'"
      ]

checkNgMasterPid :: Target -> CreateProcess
checkNgMasterPid = checkNgMasterProcess "pid"

checkNgMasterUser :: Target -> CreateProcess
checkNgMasterUser = checkNgMasterProcess "user"

checkNgWorkerProcessWithMasterPID :: String -> String -> Target -> CreateProcess
checkNgWorkerProcessWithMasterPID pid retString _ =
  shell $
    intercalate
      "|"
      [ myUnwords ["ps", "-eo", retString <> "," <> "ppid" <> "," <> "cmd"],
        "grep -v grep",
        "grep -v awk",
        "grep 'nginx: worker process'",
        myUnwords
          [ "awk",
            shellEscape $
              myUnwords
                [ "$2==\"" <> pid <> "\"",
                  "{print $1}"
                ]
          ],
        "head -1"
      ]

checkNgWorkerPid :: String -> Target -> CreateProcess
checkNgWorkerPid pid = checkNgWorkerProcessWithMasterPID pid "pid"

checkNgWorkerUser :: String -> Target -> CreateProcess
checkNgWorkerUser pid = checkNgWorkerProcessWithMasterPID pid "user"

ngCheckIsStopped :: Target -> Propellor PrevCheckResult
ngCheckIsStopped ngx = do
  isStopped <-
    liftIO $
      null
        <$> readCreateProcessTrailed (checkNgMasterPid ngx) ""
  pure $ bool CarryOn (NoNeedToCarryOn "process already stopped") isStopped

ngChangeToStopped :: Target -> Propellor Result
ngChangeToStopped ngx@Target {targetServiceUnitName = Nothing, ..} = do
  liftIO $ do
    curUser <-
      flip readCreateProcessTrailed "" $ checkNgMasterUser ngx
    case curUser of
      "" -> return NoChange
      "root" ->
        boolSystemResultQuiet
          (getMyNginxFullPathExe targetFullPathExe)
          [ Param "-s",
            Param "stop"
          ]
      _ ->
        boolSystemResultQuiet
          "su"
          [ Param "--login",
            Param curUser,
            Param "--command",
            Param $
              shellEscape $
                myUnwords
                  [ getMyNginxFullPathExe targetFullPathExe,
                    "-s",
                    "stop"
                  ]
          ]
ngChangeToStopped Target {targetServiceUnitName = Just svc'} = do
  liftIO $ boolSystemResultQuiet "systemctl" [Param "stop", Param $ toString svc']

ngStoppedProp :: Target -> Property Linux
ngStoppedProp ngx =
  prevCheck (ngCheckIsStopped ngx) $
    property "stop nginx process or service" $
      ngChangeToStopped ngx

ngCheckIsStarted :: Target -> Propellor PrevCheckResult
ngCheckIsStarted ngx = do
  startedPrc <- liftIO $ readCreateProcessTrailed (checkNgMasterPid ngx) ""
  pure $ bool CarryOn (NoNeedToCarryOn "already started") $ not . null $ startedPrc

ngChangeIsStarted :: ServiceName -> Target -> Propellor Result
ngChangeIsStarted svc' ngx = do
  sysctlCmd <-
    liftIO $
      boolSystemQuiet
        "sh"
        [ Param "-c",
          Param "type -p systemctl"
        ]
  liftIO $ debug ["systemctl: ", show sysctlCmd]
  Propellor.Utilities.ifM
    (pure $ not sysctlCmd)
    ( liftIO $ uncurry boolSystemResultQuiet $ startupCmd ngx,
      liftIO $
        boolSystemResultQuiet
          "systemctl"
          [ Param "start",
            Param svc'
          ]
    )
  where
    startupCmd Target {targetConfigFrom = FromDefault, ..} =
      ( "su",
        [ Param "--login",
          Param $ getMyNginxUser targetUser,
          Param "--command",
          Param $ shellEscape $ getMyNginxFullPathExe targetFullPathExe
        ]
      )
    startupCmd Target {targetConfigFrom = FromCmdLine, ..} =
      ( "su",
        [ Param "--login",
          Param $ getMyNginxUser targetUser,
          Param "--command",
          Param $
            shellEscape $
              myUnwords
                [ getMyNginxFullPathExe targetFullPathExe,
                  "-p",
                  getMyNginxPrefix targetPrefix,
                  "-c",
                  getMyNginxConfPath targetConfPath
                ]
        ]
      )

ngStartedProp :: ServiceName -> Target -> Property Linux
ngStartedProp svc' ngx =
  prevCheck (ngCheckIsStarted ngx) $
    property "start nginx" $
      ngChangeIsStarted svc' ngx

ngCheckRunAs :: Target -> Propellor PrevCheckResult
ngCheckRunAs ngx@Target {..} = do
  masterPid <-
    liftIO $
      flip readCreateProcessTrailed "" $ checkNgMasterPid ngx
  isRunAs <-
    liftIO $
      flip readCreateProcessTrailed "" $ checkNgWorkerUser masterPid ngx
  pure $
    bool
      CarryOn
      ( NoNeedToCarryOn $
          "already run as the user "
            <> getMyNginxUser targetUser
      )
      $ (==) isRunAs $ getMyNginxUser targetUser

ngChangeRunAs :: Target -> Propellor Result
ngChangeRunAs _ =
  infoMessage
    [ "Seems no point to change nginx worker process to another",
      "ordinary user. In most cases, it already runs as a normal",
      "user. Skip for now."
    ]
    >> noChange

ngRunAsProp :: Target -> Property (HasInfo + Linux)
ngRunAsProp ngx@Target {..} =
  prevCheck (ngCheckRunAs ngx) $
    property
      ( "change the nginx worker process user to "
          <> getMyNginxUser targetUser
      )
      $ ngChangeRunAs ngx

ngCheckDirListing :: Target -> Propellor PrevCheckResult
ngCheckDirListing ngx = do
  isDirListing <-
    liftIO $
      flip readCreateProcessTrailed "" $ checkDirListCmd ngx
  pure $
    bool CarryOn (NoNeedToCarryOn "already disable dir listing") $
      null isDirListing
  where
    checkDirListCmd Target {..} =
      shell $
        intercalate
          "|"
          [ myUnwords
              [ getMyNginxFullPathExe targetFullPathExe,
                "-p",
                getMyNginxPrefix targetPrefix,
                "-c",
                getMyNginxConfPath targetConfPath,
                "-T",
                "-q"
              ],
            "grep -v grep",
            "grep -w autoindex",
            "grep -v '#'",
            "grep -w on",
            "cat"
          ]

ngChangeDirListing :: Target -> Propellor Result
ngChangeDirListing _ = do
  ngConfFiles <- getNginxConfFilesInfo
  let changeDirListingF = changeNgDirective "autoindex" ["off"]
  void $
    liftIO $
      sequence $
        changeNgDirectiveForFile changeDirListingF
          <$> getNginxConfFiles ngConfFiles
  pure MadeChange

ngDirListingProp :: Target -> Property (HasInfo + Linux)
ngDirListingProp ngx =
  prevCheck (ngCheckDirListing ngx) $
    property "disabled direcory listing" $ ngChangeDirListing ngx

ngCheckVersionMasking :: Target -> Propellor PrevCheckResult
ngCheckVersionMasking ngx = do
  isVersionMasked <-
    liftIO $
      flip readCreateProcessTrailed "" $ checkVersionMaskingCmd ngx
  pure $
    bool CarryOn (NoNeedToCarryOn "already masked version") $
      not $ null isVersionMasked
  where
    checkVersionMaskingCmd Target {..} =
      shell $
        intercalate
          "|"
          [ myUnwords
              [ getMyNginxFullPathExe targetFullPathExe,
                "-p",
                getMyNginxPrefix targetPrefix,
                "-c",
                getMyNginxConfPath targetConfPath,
                "-T",
                "-q"
              ],
            "grep -v grep",
            "grep -w server_tokens",
            "grep -v '#'",
            "grep -w off",
            "cat"
          ]

ngChangeVersionMasking :: Target -> Propellor Result
ngChangeVersionMasking ngx = do
  ngConfFiles <- getNginxConfFilesInfo
  let changeVersionMaskingF = changeNgDirective "server_tokens" ["off"]
      addVersionMaskingF = addNgDirective "server_tokens" ["off"] ["http"]
  void $
    liftIO $ do
      serverTokensTag <-
        flip readCreateProcessTrailed "" $
          checkVersionMaskingCmd ngx
      debug ["serve_tokens: ", show $ not $ null serverTokensTag]
      bool
        ( sequence $
            changeNgDirectiveForFile addVersionMaskingF
              <$> getNginxConfFiles ngConfFiles
        )
        ( sequence $
            changeNgDirectiveForFile changeVersionMaskingF
              <$> getNginxConfFiles ngConfFiles
        )
        $ not $ null serverTokensTag
  pure MadeChange
  where
    checkVersionMaskingCmd Target {..} =
      shell $
        intercalate
          "|"
          [ myUnwords
              [ getMyNginxFullPathExe targetFullPathExe,
                "-p",
                getMyNginxPrefix targetPrefix,
                "-c",
                getMyNginxConfPath targetConfPath,
                "-T",
                "-q"
              ],
            "grep -v grep",
            "grep -w server_tokens",
            "grep -v '#'",
            "cat"
          ]

ngVersionMaskingProp :: Target -> Property (HasInfo + Linux)
ngVersionMaskingProp ngx =
  prevCheck (ngCheckVersionMasking ngx) $
    property "masking the server version info" $ ngChangeVersionMasking ngx

ngCheckService :: Target -> Propellor PrevCheckResult
ngCheckService Target {..} = do
  pure $
    bool CarryOn (NoNeedToCarryOn "already has service") $
      isJust targetServiceUnitName

ngChangeService :: Target -> Propellor Result
ngChangeService _ = pure MadeChange

ngCreateServiceProp :: Target -> Property (HasInfo + Linux)
ngCreateServiceProp ngx@Target {..} =
  tightenTargets $
    hasContent
      ("/etc/systemd/system" </> ngServiceName ngx)
      [ "[Unit]",
        "Description=Nginx instance with config "
          <> getMyNginxConfPath targetConfPath,
        "After=network.target remote-fs.target nss-lookup.target",
        "",
        "[Service]",
        "Type=forking",
        "PIDFile=/run/nginx.pid",
        "# Nginx will fail to start if /run/nginx.pid already exists but has the wrong",
        "# SELinux context. This might happen when running `nginx -t` from the cmdline.",
        "# https://bugzilla.redhat.com/show_bug.cgi?id=1268621",
        "ExecStartPre=/usr/bin/rm -f /run/nginx.pid",
        "ExecStartPre=" <> getMyNginxFullPathExe targetFullPathExe
          <> " -p "
          <> getMyNginxPrefix targetPrefix
          <> " -c "
          <> getMyNginxConfPath targetConfPath
          <> " -t",
        "ExecStart=" <> getMyNginxFullPathExe targetFullPathExe
          <> " -p "
          <> getMyNginxPrefix targetPrefix
          <> " -c "
          <> getMyNginxConfPath targetConfPath,
        "ExecReload=/bin/kill -s HUP $MAINPID",
        "KillSignal=SIGQUIT",
        "TimeoutStopSec=5",
        "KillMode=mixed",
        "PrivateTmp=true",
        "",
        "[Install]",
        "WantedBy=multi-user.target"
      ]
      `describe` ( "create service unit with config "
                     <> getMyNginxConfPath targetConfPath
                 )

ngServiceProp :: Target -> Property (HasInfo + Linux)
ngServiceProp ngx =
  prevCheck (ngCheckService ngx) $
    combineProperties "create a nginx service" $
      props
        Propellor.& ngCreateServiceProp ngx
        Propellor.& daemonReloaded
        Propellor.& enabled (ngServiceName ngx)

ngCheckAccessLog :: Target -> Propellor PrevCheckResult
ngCheckAccessLog ngx = do
  isAccessLoged <-
    liftIO $
      flip readCreateProcessTrailed "" $ checkAccessLogCmd ngx
  pure $
    bool CarryOn (NoNeedToCarryOn "already has access log") $
      null isAccessLoged
  where
    checkAccessLogCmd Target {..} =
      shell $
        intercalate
          "|"
          [ myUnwords
              [ getMyNginxFullPathExe targetFullPathExe,
                "-p",
                getMyNginxPrefix targetPrefix,
                "-c",
                getMyNginxConfPath targetConfPath,
                "-T",
                "-q"
              ],
            "grep -v grep",
            "grep -w access_log",
            "grep -v '#'",
            "grep -w off",
            "cat"
          ]

ngChangeAccessLog :: Target -> Propellor Result
ngChangeAccessLog _ = do
  ngConfFiles <- getNginxConfFilesInfo
  ngDefALP <- getNginxDefAccessLogPathInfo
  let changeAccessLogF =
        changeNgDirective
          "access_log"
          [ toText $ getNginxDefAccessLogPath ngDefALP,
            "combined"
          ]
  void $
    liftIO $
      sequence $
        changeNgDirectiveForFile changeAccessLogF <$> getNginxConfFiles ngConfFiles
  pure MadeChange

ngAccessLogProp :: Target -> Property (HasInfo + Linux)
ngAccessLogProp ngx =
  prevCheck (ngCheckAccessLog ngx) $
    property "enforce access log" $
      ngChangeAccessLog ngx

ngCheckErrorLog :: Target -> Propellor PrevCheckResult
ngCheckErrorLog ngx = do
  isErrorLoged <-
    liftIO $
      flip readCreateProcessTrailed "" $ checkErrorLogCmd ngx
  pure $ bool CarryOn (NoNeedToCarryOn "already has access log") $ null isErrorLoged
  where
    checkErrorLogCmd Target {..} =
      shell $
        intercalate
          "|"
          [ myUnwords
              [ getMyNginxFullPathExe targetFullPathExe,
                "-p",
                getMyNginxPrefix targetPrefix,
                "-c",
                getMyNginxConfPath targetConfPath,
                "-T",
                "-q"
              ],
            "grep -v grep",
            "grep -w error_log",
            "grep -v '#'",
            "grep -w stderr",
            "cat"
          ]

ngChangeErrorLog :: Target -> Propellor Result
ngChangeErrorLog _ = do
  ngConfFiles <- getNginxConfFilesInfo
  ngDefELP <- getNginxDefErrorLogPathInfo
  let changeErrorLogF =
        changeNgDirective
          "error_log"
          [ toText $ getNginxDefErrorLogPath ngDefELP,
            "error"
          ]
  void $
    liftIO $
      sequence $
        changeNgDirectiveForFile changeErrorLogF <$> getNginxConfFiles ngConfFiles
  pure MadeChange

ngErrorLogProp :: Target -> Property (HasInfo + Linux)
ngErrorLogProp ngx =
  prevCheck (ngCheckErrorLog ngx) $
    property "enforce error log" $
      ngChangeErrorLog ngx

ngCheckLogRotate :: Target -> Propellor PrevCheckResult
ngCheckLogRotate ngx = do
  notCoveredPs <- liftIO $ ngNotRotatedPaths ngx
  pure $ bool CarryOn (NoNeedToCarryOn "already has log rotated") $ null notCoveredPs

ngNotRotatedPaths :: Target -> IO [FilePath]
ngNotRotatedPaths ngx = do
  alPaths <-
    flip readCreateProcessTrailed "" $ getLogDirsCmd "access_log" ngx
  elPaths <-
    flip readCreateProcessTrailed "" $ getLogDirsCmd "error_log" ngx
  let turnToAbsPathF = \p ->
        bool (getMyNginxPrefix (targetPrefix ngx) </> p) p $
          isAbsolute p
      allAbsALP = fmap (turnToAbsPathF . toString) (lines $ toText alPaths)
      allAbsELP = fmap (turnToAbsPathF . toString) (lines $ toText elPaths)
      uniqAllP = ordNub $ allAbsALP <> allAbsELP
  liftIO $ debug ["uniaAllP: ", show uniqAllP]
  logRotatedPaths <-
    mapM
      ( fmap (fmap toString . lines . toText)
          . (`readCreateProcessTrailed` "")
          . checkLogRotateCmd
      )
      $ ordNub $ takeDirectory <$> uniqAllP
  liftIO $
    debug
      [ "logRotatedPaths: ",
        show logRotatedPaths
      ]
  liftIO $
    debug
      [ "notCoveredPaths: ",
        show $ notCoveredPaths uniqAllP logRotatedPaths
      ]
  pure $ notCoveredPaths uniqAllP logRotatedPaths
  where
    getLogDirsCmd logType Target {..} =
      shell $
        intercalate
          "|"
          [ myUnwords
              [ getMyNginxFullPathExe targetFullPathExe,
                "-p",
                getMyNginxPrefix targetPrefix,
                "-c",
                getMyNginxConfPath targetConfPath,
                "-T",
                "-q"
              ],
            "grep -v grep",
            "grep -v awk",
            "grep -w " <> logType,
            "grep -v '#'",
            "awk '{print $2}'"
          ]
    checkLogRotateCmd logDir =
      shell $
        intercalate
          "|"
          [ myUnwords ["grep", "-R", shellEscape logDir, "/etc/logrotate.d"],
            myUnwords ["awk", "-F':'", "'{print $2}'"],
            myUnwords ["awk", "'{print $1}'"]
          ]
    notCoveredPaths :: [FilePath] -> [[FilePath]] -> [FilePath]
    notCoveredPaths paths patterns =
      let allPatterns = fmap compile $ ordNub $ concat patterns
          matchP p = or $ fmap (`match` p) allPatterns
          notCovered ps = filter ((/= True) . snd) $ zip ps $ fmap matchP ps
       in fst <$> notCovered paths

ngChangeLogRotate :: Target -> Propellor Result
ngChangeLogRotate ngx@Target {..} = noChange

ngLogRotateProp :: Target -> Property (HasInfo + Linux)
ngLogRotateProp ngx =
  property' "enforce log rotate for nginx" $ \w -> do
    notCoveredPs <- liftIO $ ngNotRotatedPaths ngx
    ensureProperty w $
      propertyList "create log rotate config for log files" $
        toProps $
          ngLogRotateConfProp <$> notCoveredPs
  where
    ngLogRotateConfName p =
      intercalate "-" $
        (:) "nginx" $
          Propellor.Utilities.tail $
            splitDirectories $
              replaceExtension ".conf" p
    ngLogRotateConfProp p =
      let confFileName = "/etc/logrotate.d" </> ngLogRotateConfName p
       in hasContent
            confFileName
            [ p <> " {",
              "create 0664 nginx root",
              "daily",
              "rotate 10",
              "missingok",
              "notifempty",
              "compress",
              "sharedscripts",
              "postrotate",
              "  bin/kill -USR1 `cat /run/nginx.pid 2>/dev/null` 2>/dev/null || true",
              "endscript",
              "}"
            ]

checkNginxHostPropsWithInfo ::
  FilePath ->
  FilePath ->
  [FilePath] ->
  MyT.Instruction ->
  Target ->
  [Property (HasInfo + Linux)]
checkNginxHostPropsWithInfo alp elp cFiles _ ngx =
  [ ngDefAccessLogPathInfoProp alp ngx,
    ngDefErrorLogPathInfoProp elp ngx,
    ngConfFilesInfoProp $ NginxConfFiles cFiles
  ]

enforceNginxHostPropsWithInfo ::
  FilePath ->
  FilePath ->
  [FilePath] ->
  MyT.Instruction ->
  Target ->
  [Property (HasInfo + Linux)]
enforceNginxHostPropsWithInfo alp elp cFiles ins ngx =
  ngDefAccessLogPathInfoProp alp ngx :
  ngDefErrorLogPathInfoProp elp ngx :
  ngConfFilesInfoProp (NginxConfFiles cFiles) :
  ngHostProps ins ngx

checkNginxHost ::
  FilePath ->
  FilePath ->
  [FilePath] ->
  MyT.Instruction ->
  Target ->
  Host
checkNginxHost alp elp cFiles inx ngx =
  host "local" $ toProps $ checkNginxHostPropsWithInfo alp elp cFiles inx ngx

enforceNginxHost ::
  FilePath ->
  FilePath ->
  [FilePath] ->
  MyT.Instruction ->
  Target ->
  Host
enforceNginxHost alp elp cFiles inx ngx =
  host "local" $ toProps $ enforceNginxHostPropsWithInfo alp elp cFiles inx ngx

myNoChange :: Property (HasInfo + Linux)
myNoChange = property "No Change" $ pure NoChange

enforceModuleToProperty ::
  Target ->
  Module ->
  Property (HasInfo + Linux)
enforceModuleToProperty _ Core.Types.All = myNoChange
enforceModuleToProperty ngx Core.Types.Group =
  ngPrefixOwnerProp ngx
enforceModuleToProperty ngx Core.Types.RunAs =
  ngRunAsProp ngx
enforceModuleToProperty ngx Core.Types.AccessLog =
  ngAccessLogProp ngx
enforceModuleToProperty ngx Core.Types.ErrorLog =
  ngErrorLogProp ngx
enforceModuleToProperty ngx Core.Types.ListingsDisabled =
  ngDirListingProp ngx
enforceModuleToProperty ngx Core.Types.VersionInvisible =
  ngVersionMaskingProp ngx
enforceModuleToProperty ngx Core.Types.Service =
  ngServiceProp ngx
enforceModuleToProperty ngx Core.Types.LogRotate =
  ngLogRotateProp ngx

enforceInstructionToProperty ::
  MyT.Instruction ->
  Target ->
  [Property (HasInfo + Linux)]
enforceInstructionToProperty
  MyT.Instruction
    { insAction = Enforce,
      insModule = insMods
    }
  ngx =
    enforceModuleToProperty ngx <$> insMods
enforceInstructionToProperty ins _ =
  giveup $
    myUnwords
      [ "This is only for Enforce",
        " action, however, the action ",
        "is: ",
        show ins
      ]

ngHostProps :: MyT.Instruction -> Target -> [Property (HasInfo + Linux)]
ngHostProps = enforceInstructionToProperty

checkModuleToAction ::
  Target ->
  Module ->
  Propellor Result
checkModuleToAction ngx Core.Types.All = do
  warningMessage $
    myUnwords
      [ "should not get here because All module will be converted to",
        "individual module list before convert to actions. The target",
        "is as following: ",
        show ngx
      ]
  noChange
checkModuleToAction ngx Core.Types.Group =
  prevCheckResult2Result <$> ngCheckPrefixOwner ngx
checkModuleToAction ngx Core.Types.RunAs =
  prevCheckResult2Result <$> ngCheckRunAs ngx
checkModuleToAction ngx Core.Types.AccessLog =
  prevCheckResult2Result <$> ngCheckAccessLog ngx
checkModuleToAction ngx Core.Types.ErrorLog =
  prevCheckResult2Result <$> ngCheckErrorLog ngx
checkModuleToAction ngx Core.Types.ListingsDisabled =
  prevCheckResult2Result <$> ngCheckDirListing ngx
checkModuleToAction ngx Core.Types.VersionInvisible =
  prevCheckResult2Result <$> ngCheckVersionMasking ngx
checkModuleToAction ngx Core.Types.Service =
  prevCheckResult2Result <$> ngCheckService ngx
checkModuleToAction ngx Core.Types.LogRotate =
  prevCheckResult2Result <$> ngCheckLogRotate ngx

checkInstructionToAction ::
  MyT.Instruction ->
  Target ->
  [Propellor Result]
checkInstructionToAction
  MyT.Instruction
    { insAction = Check,
      insModule = insMods
    }
  ngx =
    checkModuleToAction ngx <$> insMods
checkInstructionToAction ins _ =
  giveup $
    myUnwords
      [ "This is only for Check",
        " action, however, the action ",
        "is: ",
        show ins
      ]

checkNginxEnv :: FilePath -> FilePath -> AppM ()
checkNginxEnv prefix confPath = do
  liftIO $ do
    bool pass (giveup "no nginx found, quit") $ prefix == "" || confPath == ""
    isNgPrefixExist <- doesDirectoryExist prefix
    bool
      ( giveup $
          myUnwords
            [ "nginx prefix path(",
              prefix,
              ") does not exist."
            ]
      )
      pass
      isNgPrefixExist
    isNgConfPathExist <- doesFileExist confPath
    bool
      ( giveup $
          myUnwords
            [ "nginx config path(",
              confPath,
              ") does not exist."
            ]
      )
      pass
      isNgConfPathExist

result2NgCheckResult :: Result -> NgCheckResult
result2NgCheckResult MadeChange = NotComplied
result2NgCheckResult NoChange = Complied
result2NgCheckResult FailedChange = Unknown

runCheckActions :: Host -> [Propellor Result] -> IO [NgCheckResult]
runCheckActions h acts = do
  rs <- sequence $ runPropellor h <$> acts
  pure $ result2NgCheckResult <$> rs

instance KeepNginxComplianceM AppM where
  check inx ngxs = do
    sequence $ check1 inx <$> ngxs
    where
      check1 :: Instruction -> Target -> AppM [NgCheckResult]
      check1 inx' ngx@Target {..} = do
        confFiles <-
          liftIO $
            flip readCreateProcessTrailed "" $
              getNgConfFilesCmd ngx
        alp <-
          liftIO $
            flip readCreateProcessTrailed "" $
              getNgConfigValueCmd "--http-log-path=" ngx
        elp <-
          liftIO $
            flip readCreateProcessTrailed "" $
              getNgConfigValueCmd "--error-log-path=" ngx
        let ngxP = getMyNginxPrefix targetPrefix
            ngxC = getMyNginxConfPath targetConfPath
            ngxC' = bool (ngxP </> ngxC) ngxC $ isAbsolute ngxC
            ngx' = ngx {targetConfPath = MyNginxConfPath ngxC'}
            confFiles' =
              filter (\fp -> takeFileName fp /= "mime.types") $
                fmap toString $
                  lines $
                    toText confFiles
            cHost = checkNginxHost alp elp confFiles' inx ngx'
            cActions = checkInstructionToAction inx' ngx'
        checkNginxEnv ngxP ngxC'
        liftIO $ runCheckActions cHost cActions

  comply inx ngxs = do
    liftIO $ debug ["inx to enforce: ", show inx]
    liftIO $ debug ["ngxs to enforce: ", show ngxs]
    eHost <- sequence $ enforce1 inx <$> ngxs
    void $ liftIO $ sequence $ mainProperties <$> eHost
    where
      enforce1 :: Instruction -> Target -> AppM Host
      enforce1 inx' ngx@Target {..} = do
        confFiles <-
          liftIO $
            flip readCreateProcessTrailed "" $
              getNgConfFilesCmd ngx
        liftIO $ debug ["confFiles: ", confFiles]
        alp <-
          liftIO $
            flip readCreateProcessTrailed "" $
              getNgConfigValueCmd "--http-log-path=" ngx
        liftIO $ debug ["defaccesslogpath: ", alp]
        elp <-
          liftIO $
            flip readCreateProcessTrailed "" $
              getNgConfigValueCmd "--error-log-path=" ngx
        liftIO $ debug ["deferrorlogpath: ", elp]
        let ngxP = getMyNginxPrefix targetPrefix
            ngxC = getMyNginxConfPath targetConfPath
            ngxC' = bool (ngxP </> ngxC) ngxC $ isAbsolute ngxC
            ngx' = ngx {targetConfPath = MyNginxConfPath ngxC'}
            confFiles' =
              filter (\fp -> takeFileName fp /= "mime.types") $
                fmap toString $
                  lines $
                    toText confFiles
            cHost = enforceNginxHost alp elp confFiles' inx' ngx'
        liftIO $ debug ["confFiles': ", show confFiles']
        checkNginxEnv ngxP ngxC'
        pure cHost
