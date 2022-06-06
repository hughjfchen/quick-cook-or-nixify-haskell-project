{-# OPTIONS_GHC -fno-warn-orphans #-}

-- | This module implement the type Capability.ReportUploader for the app
module AppCapability.ReportUploader
  ( uploadReport,
  )
where

import AppError
import AppM
import As
import Capability.ReportUploader
import Core.Types
import Data.Aeson (FromJSON, ToJSON, (.:))
import qualified Data.Aeson as Aeson
-- import Data.Time.Clock   (UTCTime)
-- import OddJobs.Types    (JobRunnerName(..))
-- import Data.Time.Clock   (UTCTime)
-- import OddJobs.Types    (JobRunnerName(..))

import Data.Password.Types (unsafeShowPassword)
import Error
import Has
import Path (fromSomeFile, parent, toFilePath)
import Path.IO (removeDirRecur)
import System.Process.Typed
import qualified Text.URI as URI

data LoginReq = LoginReq
  { loginReqEmail :: !Text,
    loginReqPassword :: !Text
  }
  deriving stock (Eq, Show, Generic)

instance ToJSON LoginReq where
  toJSON LoginReq {..} =
    Aeson.object
      [ "email" Aeson..= loginReqEmail,
        "password" Aeson..= loginReqPassword
      ]

data MeInfo = MeInfo
  { meId :: !Int,
    meName :: !Text,
    meEmail :: !Text,
    meRole :: !Text
  }
  deriving stock (Eq, Show, Generic)

-- getJobRunnerName :: Maybe JobRunnerName -> String
-- getJobRunnerName Nothing = ""
-- getJobRunnerName (Just runner) = toString $ unJobRunnerName runner

-- getLockedAt :: Maybe UTCTime -> String
--- getLockedAt Nothing = ""
-- getLockedAt (Just lockedTime) = show lockedTime

-- instance FromJSON MeInfo where
--   parseJSON = Aeson.withObject "MeInfo" $ \obj -> do
--     meId <- obj Aeson..: "id"
--     meName <- obj Aeson..: "name"
--     meEmail <- obj Aeson..: "email"
--     meRole <- obj Aeson..: "role"
--     pure $ MeInfo{..}
instance FromJSON MeInfo where
  parseJSON (Aeson.Object o) =
    MeInfo <$> (o .: "id")
      <*> (o .: "name")
      <*> (o .: "email")
      <*> (o .: "role")
  parseJSON _ = mzero

data LoginInfo = LoginInfo
  { loginInfoMe :: MeInfo,
    loginInfoToken :: !Text
  }
  deriving stock (Eq, Show, Generic)

instance FromJSON LoginInfo where
  parseJSON (Aeson.Object o) =
    LoginInfo <$> (o .: "me")
      <*> (o .: "token")
  parseJSON _ = mzero

-- >>> Aeson.decode "{\"me\" : {\"id\" : 1, \"name\" : \"chenjf\", \"email\" : \"chenjf@cn.ibm.com\", \"role\" : \"customer\"}, \"token\" : \"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIiA6ICJ3ZWJ1c2VyIiwgInVzZXJfaWQiIDogMSwgImV4cCIgOiAxNjI5NDU4ODA2fQ.s4isQSou2niOUkr-5pg81rEFe_edLy8JZ3uBasoc2Es\"}" :: Maybe LoginInfo
-- Just (LoginInfo {loginInfoMe = MeInfo {meId = 1, meName = "chenjf", meEmail = "chenjf@cn.ibm.com", meRole = "customer"}, loginInfoToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJyb2xlIiA6ICJ3ZWJ1c2VyIiwgInVzZXJfaWQiIDogMSwgImV4cCIgOiAxNjI5NDU4ODA2fQ.s4isQSou2niOUkr-5pg81rEFe_edLy8JZ3uBasoc2Es"})
instance ReportUploaderM AppM' where
  uploadReport jobId reportPath = do
    cmdPaths <- grab @CommandPath'
    curlOptions <- grab @CurlCmdLineOptions'
    loginReq <-
      pure $
        LoginReq
          { loginReqEmail = unsafeShowPassword $ curlCmdLineLoginUser' curlOptions,
            loginReqPassword = unsafeShowPassword $ curlCmdLineLoginPIN' curlOptions
          }
    (loginOut, _) <-
      readProcess_ $
        proc
          (fromSomeFile $ cmdCurlPath' cmdPaths)
          [ "--header",
            "Content-Type: application/json",
            "--header",
            "Accept: application/json",
            "--fail",
            "--data",
            decodeUtf8 $ Aeson.encode loginReq,
            URI.renderStr $ curlCmdLineLoginUrl' curlOptions
          ]
    let loginInfo = Aeson.eitherDecode loginOut
    token <- case loginInfo of
      Right t -> pure $ loginInfoToken t
      Left e -> throwError $ as $ LoginError $ toText e
    runProcess_ $
      proc
        (fromSomeFile $ cmdCurlPath' cmdPaths)
        [ "--header",
          "Authorization: Bearer " <> toString token,
          "--fail",
          "--form",
          "job_id=" <> show jobId,
          -- , "--form", "created_at=" <> show (jobCreatedAt job)
          -- , "--form", "updated_at=" <> show (jobUpdatedAt job)
          -- , "--form", "run_at=" <> show (jobUpdatedAt job)
          -- , "--form", "status=" <> "success"
          -- , "--form", "payload=" <> decodeUtf8 (Aeson.encode (jobPayload job))
          -- , "--form", "last_error=" <> decodeUtf8 (Aeson.encode (jobLastError job))
          -- , "--form", "attempts=" <> show (jobAttempts job)
          -- , "--form", "locked_at=" <> getLockedAt (jobLockedAt job)
          -- , "--form", "locked_by=" <> getJobRunnerName (jobLockedBy job)
          "--form",
          "report=@" <> toFilePath reportPath,
          URI.renderStr $ curlCmdLineUploadUrl' curlOptions
        ]
    -- clean up the file(s) generated from the previous step to save disk space
    removeDirRecur $ parent reportPath
