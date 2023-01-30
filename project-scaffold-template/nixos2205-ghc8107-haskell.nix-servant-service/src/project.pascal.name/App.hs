{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE NoImplicitPrelude #-}

-- | Defines convenience functions to run a servant base api in wrap server
module {{name|toPascal}}.App
  ( module {{name|toPascal}}.App,
  )
where

import {{name|toPascal}}.RequestLogging
import qualified {{name|toPascal}}.Types as T (InfoDetail (..))
import {{name|toPascal}}.Util
import Control.Monad.Trans.Except (ExceptT (..))
import Data.Aeson
import Data.Proxy
import Network.Wai
import Network.Wai.Cli
import Network.Wai.Middleware.Health (health)
import Network.Wai.Middleware.Info (info)
import qualified Network.Wai.Middleware.Prometheus as P
import qualified Prometheus as P
import qualified Prometheus.Metric.GHC as P
import RIO
import Servant as X hiding (And, Handler)
import qualified Servant

-- | Setup servant with custom context so that the handers can take custom effects/ctx
{{name|toCamel}}App ::
  forall β χ ψ.
  ( HasServer χ ψ,
    HasContextEntry (ψ .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  -- | Servant Context e.g., EmptyContext
  Context ψ ->
  -- | Application Has stacking in tuple type e.g., (ModLogger,ModHttpClient,UserRepo)
  β ->
  -- | Servant API Proxy
  Proxy χ ->
  -- | Servant api handlers in `RIO β` monad
  ServerT χ (RIO β) ->
  -- | Returns WAI compatiable Application so you can run using wrap
  Application
{{name|toCamel}}App sctx ctx api app = serveWithContext api sctx $ srv ctx
  where
    srv c = hoistServerWithContext api (Proxy @ψ) (run{{name|toPascal}}Handler c) app

-- | Starts the warp server with given middlewares, context, api definition and api server
-- Does not enable/registers GHC internal metrics
run{{name|toPascal}}App ::
  ( MonadIO m,
    HasServer χ ψ,
    HasContextEntry (ψ .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  -- | WAI based middlewares
  Middleware ->
  -- | Servant Context e.g., EmptyContext
  Context ψ ->
  -- | Application Has stacking in tuple type e.g., (ModLogger,ModHttpClient,UserRepo)
  β ->
  -- | Servant API Proxy
  Proxy χ ->
  -- | Servant api handlers in `RIO β` monad
  ServerT χ (RIO β) ->
  -- Runs the resulting WAI application using wai-cli `defWaiMain` function
  m ()
run{{name|toPascal}}App middlewares sctx ctx api apiHandlers =
  liftIO $
    defWaiMain $ middlewares $ {{name|toCamel}}App sctx ctx api apiHandlers

-- | Starts the warp server with given middlewares, context, api definition and api server
-- Enables prometheus metrics (with GHC internal metrics) (Needs -with-rtsopts=-T)
run{{name|toPascal}}AppWithMetrics ::
  ( MonadIO m,
    HasServer χ ψ,
    HasContextEntry (ψ .++ DefaultErrorFormatters) ErrorFormatters
  ) =>
  -- | WAI based middlewares
  Middleware ->
  -- | Servant Context e.g., EmptyContext
  Context ψ ->
  -- | Application Has stacking in tuple type e.g., (ModLogger,ModHttpClient,UserRepo)
  β ->
  -- | Servant API Proxy
  Proxy χ ->
  -- | Servant api handlers in `RIO β` monad
  ServerT χ (RIO β) ->
  -- Runs the resulting WAI application using wai-cli `defWaiMain` function
  m ()
run{{name|toPascal}}AppWithMetrics middlewares sctx ctx api apiHandlers = do
  _ <- registerMetrics
  run{{name|toPascal}}App middlewares sctx ctx api apiHandlers

-- | Return default set of middlewares applied
{{name|toCamel}}Middlewares :: T.InfoDetail -> IO Middleware
{{name|toCamel}}Middlewares infoDetail = do
  logger <-
    jsonRequestLogger (T.appEnvironment infoDetail) (T.appVersion infoDetail)
  return $ logger . P.prometheus P.def . health . info jsonInfoDetail
  where
    jsonInfoDetail = encode infoDetail

-- | Registers GHC runtime metrics so that /metrics endpoint will return rich GHC info
-- Requires `-with-rtsopts=-T`
registerMetrics :: MonadIO m => m P.GHCMetrics
registerMetrics = P.register P.ghcMetrics

-- | Custom Servant Error formatter overrides to return in JSON format
{{name|toCamel}}ErrorFormatters :: ErrorFormatters
{{name|toCamel}}ErrorFormatters =
  defaultErrorFormatters
    { bodyParserErrorFormatter = jsonErrorFormatter,
      notFoundErrorFormatter = notFoundFormatter
    }

-- | Natural transformation to run handlers in RIO monad instead of ServantT
run{{name|toPascal}}Handler :: a -> RIO a h -> Servant.Handler h
run{{name|toPascal}}Handler ctx a = Servant.Handler $ ExceptT $ try $ runReaderT (unRIO a) ctx
