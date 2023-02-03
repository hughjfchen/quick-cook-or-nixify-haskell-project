{-# OPTIONS_HADDOCK prune #-}
{-# LANGUAGE Trustworthy #-}

{-|
This module re-exports all functionality of this package for easy use.
Users expected to import this module only.

__Examples:__

@
__import__ "{{name|toPascal}}"
@

== Getting started

To create a bare minimum API service all you need is below:

@
\#!\/usr\/bin\/env stack
\{\- stack --resolver lts-14.27 runghc --package {{name}} \-\}
\{\-\# LANGUAGE NoImplicitPrelude, OverloadedStrings, UnicodeSyntax, DataKinds, TypeOperators \#\-\}
import RIO
import {{name|toPascal}}
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
  middlewares <- {{name}}Middlewares infoDetail
  run{{name|toPascal}}AppWithMetrics
    middlewares
    EmptyContext
    (logFunc, infoDetail)
    appAPI
    appServer
@
-}
module {{name|toPascal}}
  (
    -- $main
    module {{name|toPascal}}.App,
    -- $config
    module {{name|toPascal}}.Config,
    -- $jwt
    module {{name|toPascal}}.JWT,
    -- $logging
    module {{name|toPascal}}.Logging,
    -- $types
    module {{name|toPascal}}.Types,
    -- $util
    module {{name|toPascal}}.Util,
    module {{name|toPascal}},
  )
where

import           {{name|toPascal}}.App
import           {{name|toPascal}}.Config
import           {{name|toPascal}}.JWT
import           {{name|toPascal}}.Logging
import           {{name|toPascal}}.Types
import           {{name|toPascal}}.Util
import           RIO

-- | Basic application context, mostly used in examples.
-- For real life you need to create one for your application
type BasicAppCtx = (ModLogger, InfoDetail)

-- | Nice type synonym to mark your servant handlers
-- For real life you need to create one for your application
type BasicApp = RIO BasicAppCtx


{- $codec
 Main App functionalities
-}

{- $config
 Configuration reading from ENV utility functions
-}

{- $logging
 Logging related functionalities
-}

{- $jwt
 JWT based authentication functionalities
-}

{- $types
 Package defined Types
-}

{- $util
 Assorted conventient functions
-}
