#!/usr/bin/env stack
{- stack --resolver lts-14.27 runghc
 --package {{name}}
-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

import {{name|toPascal}}
import RIO
import Servant

type HelloRoute = "hello" :> QueryParam "name" Text :> Get '[PlainText] Text

type API = HelloRoute :<|> EmptyAPI

hello :: Maybe Text -> BasicApp Text
hello name = do
  let name' = fromMaybe "Sensei!" name
  logInfo $ "Saying hello to " <> display name'
  return $ "Hello " <> name' <> "!"

main :: IO ()
main = do
  let infoDetail = InfoDetail "{{name}}" "dev" "0.0.1.0" "{{description}}"
      appEnv = appEnvironment infoDetail
      appVer = appVersion infoDetail
      appAPI = Proxy :: Proxy API
      appServer = hello :<|> emptyServer
  logFunc <- buildLogger appEnv appVer
  middlewares <- {{name|toCamel}}Middlewares infoDetail
  run{{name|toPascal}}AppWithMetrics
    middlewares
    EmptyContext
    (logFunc, infoDetail)
    appAPI
    appServer
