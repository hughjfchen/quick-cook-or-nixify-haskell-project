{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE NoImplicitPrelude    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

module User (UserAppCtx, UserApp, server, UserAPI, userAPI, newInMemRepo) where

import           {{name|toPascal}}
import           Data.Aeson
import           Data.Has
import qualified Data.HashMap.Strict as B
import           RIO
import           Servant
import           Servant.Auth.Server as SAS

newtype Name = Name Text
  deriving (Eq, Show, Generic)

newtype Email = Email Text
  deriving (Eq, Show, Generic)

data User = User
  { _name  :: !Name,
    _email :: !Email
  }
  deriving (Eq, Show, Generic)

instance ToJSON Name

instance FromJSON Name

instance ToJSON Email

instance FromJSON Email

instance ToJSON User

instance FromJSON User

data UserRepo = UserRepo
  { _getUser     :: !(Text -> IO (Maybe User)),
    _getAllUsers :: !(IO [User]),
    _insertUser  :: !(Text -> User -> IO ())
  }

class HasUserRepo env where
  userRepoL :: Lens' env UserRepo

instance {-# OVERLAPPABLE #-} Has UserRepo m => HasUserRepo m where
  userRepoL = hasLens

type UserAPI =
  "users"
    :> ( Get '[JSON] [User]
           :<|> Capture "id" Text :> Get '[JSON] (Maybe User)
           :<|> ReqBody '[JSON] User :> Post '[JSON] NoContent
       )

userAPI :: Proxy UserAPI
userAPI = Proxy

type UserAppCtx = (ModLogger, InfoDetail, UserRepo)

type UserApp = RIO UserAppCtx

server (SAS.Authenticated user) = getUsers :<|> findUserById :<|> createUser
server _ = throwUnauthorized :<|> const throwUnauthorized :<|> const throwUnauthorized

newInMemRepo :: (MonadUnliftIO m) => m UserRepo
newInMemRepo = do
  store <- liftIO $ atomically $ newTVar (B.fromList defaultUsers)
  return $
    UserRepo
      { _getUser = \uid -> do
          u <- atomically $ readTVar store
          return $ B.lookup uid u,
        _getAllUsers = do
          u <- atomically $ readTVar store
          return $ B.elems u,
        _insertUser = \uid u -> do
          lst <- atomically $ readTVar store
          atomically $ writeTVar store (B.insert uid u lst)
          return ()
      }

defaultUsers :: [(Text, User)]
defaultUsers = [("1234", User (Name "name1") (Email "email@test.com"))]

getUsers :: UserApp [User]
getUsers = do
  logWarn "Fetching users from db"
  repo <- askObj
  users <- liftIO $ _getAllUsers repo
  return users

findUserById :: Text -> UserApp (Maybe User)
findUserById userId = do
  logInfoS "find" ("Find by id = " <> display userId)
  repo <- askObj
  mu <- liftIO $ _getUser repo $ userId
  return mu

createUser :: User -> UserApp NoContent
createUser u = do
  logInfoS "create" "New user created"
  repo <- askObj
  liftIO $ _insertUser repo "33" u
  return NoContent
