-- | This module defines the core domain types which shared by lib, exe and tests
module Core.Types
  ( Action (..),
    Module (..),
    strModule,
    Instruction (..),
    Target (..),
    CheckResultItem (..),
    textResultToBool,
    checkResultItemsToInstructions,
    MyNginxCmdLineParas (..),
    MyNginxFullPathExe (..),
    MyNginxPrefix (..),
    MyNginxUser (..),
    MyNginxConfPath (..),
    MyNginxCmdLine (..),
    NginxCmd (..),
    NgCheckResult (..),
    ConfigFrom (..),
    ngCR2Status,
    allModules,
  )
where

import Data.Data (Data)
import GHC.Base (errorWithoutStackTrace)

-- import Data.Data (Data, Typeable)
-- import Data.Text (Text)
-- import GHC.Generics

data MyNginxCmdLineParas
  = MyCmdLinePrefix FilePath
  | MyCmdLineConfig FilePath
  | MyCmdLineError FilePath
  | MyCmdLineGlobal String
  deriving stock (Eq, Ord, Show, Typeable, Data, Generic)

-- | the nginx EXE with full path
newtype MyNginxFullPathExe = MyNginxFullPathExe {getMyNginxFullPathExe :: FilePath}
  deriving stock (Eq, Ord, Show, Typeable, Data, Generic)

-- | the nginx prefix directory
newtype MyNginxPrefix = MyNginxPrefix {getMyNginxPrefix :: FilePath}
  deriving stock (Eq, Ord, Show, Typeable, Data, Generic)

-- | the nginx user
newtype MyNginxUser = MyNginxUser {getMyNginxUser :: String}
  deriving stock (Eq, Ord, Show, Typeable, Data, Generic)

-- | the path to the nginx conf
newtype MyNginxConfPath = MyNginxConfPath {getMyNginxConfPath :: FilePath}
  deriving stock (Eq, Ord, Show, Typeable, Data, Generic)

-- | the nginx cmdline
newtype MyNginxCmdLine = MyNginxCmdLine {getMyNginxCmdLine :: FilePath}
  deriving stock (Eq, Ord, Show, Typeable, Data, Generic)

-- | nginx command line
data NginxCmd = NginxCmd
  { ngxCmdPrefix :: Maybe MyNginxPrefix,
    ngxCmdConfPath :: Maybe MyNginxConfPath,
    ngxCmdErrorFile :: Maybe FilePath,
    ngxCmdGlobalDirectives :: Maybe String
  }
  deriving stock (Show, Eq, Ord, Typeable, Data, Generic)

type PID = Text

data ConfigFrom = FromCmdLine | FromDefault
  deriving stock (Show, Eq, Ord, Typeable, Data, Generic)

instance IsString ConfigFrom where
  fromString "FromDefault" = FromDefault
  fromString "FromCmdLine" = FromCmdLine
  fromString _ = errorWithoutStackTrace "unkonw config from"

data Target = Target
  { targetPid :: Maybe PID,
    targetServiceUnitName :: Maybe Text,
    targetFullPathExe :: MyNginxFullPathExe,
    targetPrefix :: MyNginxPrefix,
    targetConfPath :: MyNginxConfPath,
    targetUser :: MyNginxUser,
    targetCmdLine :: MyNginxCmdLine,
    targetConfigFrom :: ConfigFrom
  }
  deriving stock (Show, Eq, Ord, Typeable, Data, Generic)

data Action
  = Check
  | Enforce
  | List
  deriving stock (Eq, Ord, Show, Typeable, Data, Generic)

data Module
  = All
  | Group
  | RunAs
  | AccessLog
  | ErrorLog
  | ListingsDisabled
  | VersionInvisible
  | Service
  | LogRotate
  deriving stock (Eq, Ord, Show, Typeable, Data, Generic)

strModule :: String -> Either String Module
strModule "all" = Right Core.Types.All
strModule "group" = Right Group
strModule "runas" = Right RunAs
strModule "accesslog" = Right AccessLog
strModule "errorlog" = Right ErrorLog
strModule "listingsdisabled" = Right ListingsDisabled
strModule "versioninvisible" = Right VersionInvisible
strModule "service" = Right Service
strModule "logrotate" = Right LogRotate
strModule s =
  Left $
    toString
      . unwords
      $ fmap
        toText
        [ "Not supported module,",
          s,
          ", currently only support ",
          "group, runas, accesslog, errorlog, listingsdisabled, versioninvisible, ",
          "service and logrotate"
        ]

allModules :: [Module]
allModules =
  [ Group,
    RunAs,
    AccessLog,
    ErrorLog,
    ListingsDisabled,
    VersionInvisible,
    Service,
    LogRotate
  ]

data Instruction = Instruction
  { insAction :: Action,
    insModule :: [Module]
  }
  deriving stock (Eq, Ord, Show, Typeable, Data, Generic)

data NgCheckResult
  = Complied
  | NotComplied
  | Unknown
  deriving stock (Eq, Ord, Show, Typeable, Data, Generic)

ngCR2Status :: NgCheckResult -> Text
ngCR2Status Complied = "OK"
ngCR2Status _ = "ERROR"

data CheckResultItem = CheckResultItem
  { criHostName :: !Text,
    criInstance :: !Text,
    criNo :: !Text,
    criCheckRule :: !Text,
    criCheckItem :: !Text,
    criCheckResult :: !Text,
    criStatus :: !Text,
    criTimestamp :: !Text
  }
  deriving stock (Eq, Ord, Show, Typeable, Data, Generic)

textResultToBool :: Text -> Bool
textResultToBool "false" = False
textResultToBool _ = True

checkItemToModule :: Text -> Module
checkItemToModule "All" = Core.Types.All
checkItemToModule "Group" = Group
checkItemToModule "RunAs" = RunAs
checkItemToModule "AccessLog" = AccessLog
checkItemToModule "ErrorLog" = ErrorLog
checkItemToModule "ListingsDisabled" = ListingsDisabled
checkItemToModule "VersionInvisible" = VersionInvisible
checkItemToModule "Service" = Service
checkItemToModule "LogRotate" = LogRotate
checkItemToModule s =
  errorWithoutStackTrace $
    "Not supported module"
      <> " "
      <> toString s

checkResultItemsToInstruction :: [CheckResultItem] -> Instruction
checkResultItemsToInstruction cris =
  Instruction
    { insAction = Enforce,
      insModule = checkResultItemsToModules cris
    }
  where
    checkResultItemsToModules cris' =
      checkItemToModule . criCheckItem
        <$> cris'

checkResultItemsToInstructions :: [CheckResultItem] -> Instruction
checkResultItemsToInstructions cris =
  checkResultItemsToInstruction $
    filter ((== "ERROR") . criStatus) cris
