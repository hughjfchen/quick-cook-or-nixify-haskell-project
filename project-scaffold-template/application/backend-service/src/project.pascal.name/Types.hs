{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE UndecidableInstances #-}

-- |Types
module {{name|toPascal}}.Types where

import qualified Data.Aeson          as A
import           Data.Default
import           RIO
import qualified RIO.Text            as T
import           Servant.Auth.Server
import           System.Envy

data InfoDetail = InfoDetail
  { appName        :: !Text,
    appEnvironment :: !Text,
    appVersion     :: !Text,
    appDescription :: !Text
  }
  deriving (Show, Eq, Generic)

instance FromEnv InfoDetail

instance Default InfoDetail where
  def = InfoDetail "example" "dev" "0.1" "change me"

instance A.ToJSON InfoDetail where
  toJSON =
    A.genericToJSON
      A.defaultOptions {A.fieldLabelModifier = A.camelTo2 '_' . drop 3}

data AuthenticatedUser = AuthenticatedUser
  { aud   :: !Text,
    iss   :: !Text,
    appid :: !Text,
    aio   :: !Text,
    oid   :: !Text,
    sub   :: !Text,
    tid   :: !Text
  }
  deriving (Show, Generic)

instance A.ToJSON AuthenticatedUser

instance A.FromJSON AuthenticatedUser

instance ToJWT AuthenticatedUser

instance FromJWT AuthenticatedUser where
  decodeJWT m = case A.fromJSON . A.toJSON $ m of
    A.Error e   -> Left $ T.pack e
    A.Success a -> Right a
