{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Main (main) where

import {{name|toPascal}}
import Configuration.Dotenv (defaultConfig, loadFile)
import Data.Version (showVersion)
import qualified Paths_{{name|toSnake}}
import RIO
import qualified RIO.Text as T
import Servant
import Servant.Auth.Server as SAS
import qualified User as U

type HelloRoute = "hello" :> QueryParam "name" Text :> Get '[PlainText] Text

type API auths =
  (SAS.Auth auths AuthenticatedUser :> U.UserAPI)
    :<|> HelloRoute
    :<|> EmptyAPI

hello :: Maybe Text -> U.UserApp Text
hello name = do
  let name' = fromMaybe "Sensei!" name
  logInfo $ "Saying hello to " <> display name'
  return $ "Hello " <> name' <> "!"

appAPI :: Proxy (API '[SAS.JWT])
appAPI = Proxy

main :: IO ()
main = do
  hSetBuffering stdin LineBuffering
  void $ loadFile defaultConfig -- load .env file if available
  -- Load the AppSettings data from ENV variables
  withAppSettingsFromEnv $ \appSettings -> do
    -- Override the version from cabal file
    -- let ver = $(simpleVersion Paths_{{name|toSnake}}.version) -- TH to get cabal project's git sha version
    -- let ver = "0.0.1.0" -- Use string leteral to avoid building fully static exe failed
    let infoDetail =
          appSettings
            { appVersion =
                T.pack $ showVersion $ Paths_{{name|toSnake}}.version
            }
    logFunc <- buildLogger (appEnvironment infoDetail) (appVersion infoDetail)
    userRepo <- U.newInMemRepo
    middlewares <- {{name|toCamel}}Middlewares infoDetail
    jwtCfg <- getJWTAuthSettings
    let cookieCfg = defaultCookieSettings {cookieIsSecure = SAS.NotSecure}
        sctx = cookieCfg :. jwtCfg :. {{name|toCamel}}ErrorFormatters :. EmptyContext
        appServer = U.server :<|> hello :<|> emptyServer
    -- Run API server with JWT auth and in-mem user repo
    run{{name|toPascal}}AppWithMetrics
      middlewares
      sctx
      (logFunc, infoDetail, userRepo)
      appAPI
      appServer
