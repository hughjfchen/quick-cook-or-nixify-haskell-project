-- | This module implements the capability specified within the library src tree
-- for executing WAS admin commands

module AppCapability.Exe{{ name | toPascal }}
( welcome
  , login
  , listServers
  , pickServer
  , updateJvmGenericParameter
  , pickJvm
  ) where

import Has
import Error
import AppError

import Core.MyCookieJar (MyCookieJar(..), mergeCookieJar)
import Core.ConnectionInfo (ConnectionInfo(..))
import Core.AuthInfo(AuthInfo(..))
import AppEnv (RedirectedUris)
import Capability.Exe{{ name | toPascal }}

import AppM

import Network.HTTP.Req
import qualified Network.HTTP.Client as HC
import Network.URI (uriToString)
import Text.URI (mkURI)

instance MonadHttp AppM where
  handleHttpException (VanillaHttpException (HC.InvalidUrlException url reason)) = throwError $ AppInvalidUrlError $ toText (url <> reason)
  handleHttpException (VanillaHttpException (HC.HttpExceptionRequest errReq content)) = throwError $ AppHttpError errReq content
  handleHttpException (JsonHttpException errMsg) = throwError $ AppJsonError $ toText $ "Parsing JSON error: " <> errMsg

reqWithRedirectHistories :: (MonadIO m, MonadReader env m, Has RedirectedUris env) => HC.Request -> HC.Manager -> m HC.CookieJar
reqWithRedirectHistories theReq mng = do
  envRU <- grab @RedirectedUris
  his <- liftIO $ HC.responseOpenHistory theReq mng
  let hsRedirects = HC.hrRedirects his
      uris = flip fmap hsRedirects $ \(redReq, _) -> toText $ (uriToString id $ HC.getUri redReq) ""
      cjsList = flip fmap hsRedirects $ \(_, redRes) -> HC.responseCookieJar redRes
      cjs = foldr (<>) (HC.createCookieJar []) cjsList
  mUris <- mapM (liftIO . mkURI) uris
  atomicModifyIORef envRU $ const (mUris, cjs)
-- >>> :hg const
-- unknown command ':hg'
-- use :? for help.
instance AuthM AppM where
  welcome = do
    connInfo <- grab @ConnectionInfo
    mycj <- grab @MyCookieJar
    r <- req' GET
      (http (ciHost connInfo) /: "ibm" /: "console")
      NoReqBody
      (port $ ciPort connInfo)
      reqWithRedirectHistories
    mergeCookieJar r mycj
  login = do
    connInfo <- grab @ConnectionInfo
    mycj <- grab @MyCookieJar
    authInfo <- grab @AuthInfo
    r <- req POST
      (http (ciHost connInfo) /: "ibm" /: "console")
      NoReqBody
      bsResponse $
      port $ ciPort connInfo
    flip mergeCookieJar mycj $ responseCookieJar r
  logout = do
    connInfo <- grab @ConnectionInfo
    mycj <- grab @MyCookieJar
    r <- req GET
      (http (ciHost connInfo) /: (show $ ciPort connInfo) /: "/logout")
      NoReqBody
      bsResponse
      mempty
    flip mergeCookieJar mycj $ responseCookieJar r
-- >>> :i runReq
-- runReq :: MonadIO m => HttpConfig -> Req a -> m a
--   	-- Defined in ‘Network.HTTP.Req’
instance ServerM AppM where
  listServers = pure [Server { cellName =""
                     , cellID = ""
                     , nodeName = ""
                     , nodeID = ""
                     , serverName = ""
                     , serverID = ""
                     }]
  pickServer = pure

instance JVMM AppM where
  updateJvmGenericParameter jvm _ = pure $ Success jvm
  pickJvm _ = pure $ JVM { jvmName = ""
               , jvmID = ""
               }

-- >>> :browse Network.HTTP.Req
-- (/:) :: Url scheme -> Text -> Url scheme
-- (/~) ::
--   http-api-data-0.4.2:Web.Internal.HttpApiData.ToHttpApiData a =>
--   Url scheme -> a -> Url scheme
-- (=:) ::
--   (QueryParam param,
--    http-api-data-0.4.2:Web.Internal.HttpApiData.ToHttpApiData a) =>
--   Text -> a -> param
-- newtype BsResponse
--   = Network.HTTP.Req.BsResponse (Response ByteString)
-- data CONNECT = CONNECT
-- data CanHaveBody = CanHaveBody | NoBody
-- data DELETE = DELETE
-- newtype FormUrlEncodedParam
--   = Network.HTTP.Req.FormUrlEncodedParam [(Text, Maybe Text)]
-- data GET = GET
-- data HEAD = HEAD
-- class HttpBody body where
--   getRequestBody :: body -> RequestBody
--   getRequestContentType :: body -> Maybe ByteString
--   {-# MINIMAL getRequestBody #-}
-- type family HttpBodyAllowed (allowsBody :: CanHaveBody)
--                             (providesBody :: CanHaveBody)
--             :: Constraint where
--     HttpBodyAllowed 'NoBody 'NoBody = () :: Constraint
--     HttpBodyAllowed 'CanHaveBody body = () :: Constraint
--     HttpBodyAllowed 'NoBody 'CanHaveBody = (TypeError ...)
-- data HttpConfig
--   = HttpConfig {httpConfigProxy :: Maybe Network.HTTP.Client.Proxy,
--                 httpConfigRedirectCount :: Int,
--                 httpConfigAltManager :: Maybe Manager,
--                 httpConfigCheckResponse :: forall b.
--                                            Request
--                                            -> Response b
--                                            -> ByteString
--                                            -> Maybe HttpExceptionContent,
--                 httpConfigRetryPolicy :: retry-0.8.1.2:Control.Retry.RetryPolicyM
--                                            IO,
--                 httpConfigRetryJudge :: forall b.
--                                         retry-0.8.1.2:Control.Retry.RetryStatus
--                                         -> Response b -> Bool,
--                 httpConfigRetryJudgeException :: retry-0.8.1.2:Control.Retry.RetryStatus
--                                                  -> SomeException -> Bool,
--                 httpConfigBodyPreviewLength :: forall a. Num a => a}
-- data Network.HTTP.Req.HttpException
--   = VanillaHttpException Network.HTTP.Client.HttpException
--   | JsonHttpException String
-- class HttpMethod a where
--   type family AllowsBody a :: CanHaveBody
--   httpMethodName :: Prelude.Proxy a -> ByteString
--   {-# MINIMAL httpMethodName #-}
-- class HttpResponse response where
--   type family HttpResponseBody response :: *
--   toVanillaResponse :: response
--                        -> Response (HttpResponseBody response)
--   getHttpResponse :: Response BodyReader -> IO response
--   acceptHeader :: Prelude.Proxy response -> Maybe ByteString
--   {-# MINIMAL toVanillaResponse, getHttpResponse #-}
-- newtype IgnoreResponse
--   = Network.HTTP.Req.IgnoreResponse (Response ())
-- newtype JsonResponse a = Network.HTTP.Req.JsonResponse (Response a)
-- newtype LbsResponse
--   = Network.HTTP.Req.LbsResponse (Response
--                                     bytestring-0.10.10.1:Data.ByteString.Lazy.Internal.ByteString)
-- class MonadIO m => MonadHttp (m :: * -> *) where
--   handleHttpException :: Network.HTTP.Req.HttpException -> m a
--   getHttpConfig :: m HttpConfig
--   {-# MINIMAL handleHttpException #-}
-- data NoReqBody = NoReqBody
-- data OPTIONS = OPTIONS
-- type role Network.HTTP.Req.Option phantom
-- data Network.HTTP.Req.Option (scheme :: Scheme)
--   = Network.HTTP.Req.Option (Endo
--                                (http-types-0.12.3:Network.HTTP.Types.URI.QueryText, Request))
--                             (Maybe (Request -> IO Request))
-- data PATCH = PATCH
-- data POST = POST
-- data PUT = PUT
-- type family ProvidesBody body :: CanHaveBody where
--     ProvidesBody NoReqBody = 'NoBody
--     ProvidesBody body = 'CanHaveBody
-- class QueryParam param where
--   queryParam :: http-api-data-0.4.2:Web.Internal.HttpApiData.ToHttpApiData
--                   a =>
--                 Text -> Maybe a -> param
--   {-# MINIMAL queryParam #-}
-- type role Req nominal
-- newtype Req a = Network.HTTP.Req.Req (ReaderT HttpConfig IO a)
-- newtype ReqBodyBs = ReqBodyBs ByteString
-- newtype ReqBodyFile = ReqBodyFile FilePath
-- newtype ReqBodyJson a = ReqBodyJson a
-- newtype ReqBodyLbs
--   = ReqBodyLbs bytestring-0.10.10.1:Data.ByteString.Lazy.Internal.ByteString
-- data ReqBodyMultipart
--   = Network.HTTP.Req.ReqBodyMultipart ByteString RequestBody
-- newtype ReqBodyUrlEnc = ReqBodyUrlEnc FormUrlEncodedParam
-- data Scheme = Http | Https
-- data TRACE = TRACE
-- type role Url nominal
-- data Url (scheme :: Scheme)
--   = Network.HTTP.Req.Url Scheme (NonEmpty Text)
-- attachHeader :: ByteString -> ByteString -> Request -> Request
-- basicAuth ::
--   ByteString -> ByteString -> Network.HTTP.Req.Option 'Https
-- basicAuthUnsafe ::
--   ByteString -> ByteString -> Network.HTTP.Req.Option scheme
-- basicProxyAuth ::
--   ByteString -> ByteString -> Network.HTTP.Req.Option scheme
-- bsResponse :: Prelude.Proxy BsResponse
-- Network.HTTP.Req.cookieJar ::
--   CookieJar -> Network.HTTP.Req.Option scheme
-- customAuth ::
--   (Request -> IO Request) -> Network.HTTP.Req.Option scheme
-- Network.HTTP.Req.decompress ::
--   (ByteString -> Bool) -> Network.HTTP.Req.Option scheme
-- defaultHttpConfig :: HttpConfig
-- header ::
--   ByteString -> ByteString -> Network.HTTP.Req.Option scheme
-- http :: Text -> Url 'Http
-- httpVersion :: Int -> Int -> Network.HTTP.Req.Option scheme
-- https :: Text -> Url 'Https
-- ignoreResponse :: Prelude.Proxy IgnoreResponse
-- jsonResponse :: Prelude.Proxy (JsonResponse a)
-- lbsResponse :: Prelude.Proxy LbsResponse
-- oAuth1 ::
--   ByteString
--   -> ByteString
--   -> ByteString
--   -> ByteString
--   -> Network.HTTP.Req.Option scheme
-- oAuth2Bearer :: ByteString -> Network.HTTP.Req.Option 'Https
-- oAuth2Token :: ByteString -> Network.HTTP.Req.Option 'Https
-- Network.HTTP.Req.port :: Int -> Network.HTTP.Req.Option scheme
-- queryFlag :: QueryParam param => Text -> param
-- renderUrl :: Url scheme -> Text
-- req ::
--   (MonadHttp m, HttpMethod method, HttpBody body,
--    HttpResponse response,
--    HttpBodyAllowed (AllowsBody method) (ProvidesBody body)) =>
--   method
--   -> Url scheme
--   -> body
--   -> Prelude.Proxy response
--   -> Network.HTTP.Req.Option scheme
--   -> m response
-- req' ::
--   (MonadHttp m, HttpMethod method, HttpBody body,
--    HttpBodyAllowed (AllowsBody method) (ProvidesBody body)) =>
--   method
--   -> Url scheme
--   -> body
--   -> Network.HTTP.Req.Option scheme
--   -> (Request -> Manager -> m a)
--   -> m a
-- reqBodyMultipart ::
--   MonadIO m =>
--   [Network.HTTP.Client.MultipartFormData.Part] -> m ReqBodyMultipart
-- reqBr ::
--   (MonadHttp m, HttpMethod method, HttpBody body,
--    HttpBodyAllowed (AllowsBody method) (ProvidesBody body)) =>
--   method
--   -> Url scheme
--   -> body
--   -> Network.HTTP.Req.Option scheme
--   -> (Response BodyReader -> IO a)
--   -> m a
-- reqCb ::
--   (MonadHttp m, HttpMethod method, HttpBody body,
--    HttpResponse response,
--    HttpBodyAllowed (AllowsBody method) (ProvidesBody body)) =>
--   method
--   -> Url scheme
--   -> body
--   -> Prelude.Proxy response
--   -> Network.HTTP.Req.Option scheme
--   -> (Request -> m Request)
--   -> m response
-- Network.HTTP.Req.responseBody ::
--   HttpResponse response => response -> HttpResponseBody response
-- Network.HTTP.Req.responseCookieJar ::
--   HttpResponse response => response -> CookieJar
-- responseHeader ::
--   HttpResponse response => response -> ByteString -> Maybe ByteString
-- responseStatusCode :: HttpResponse response => response -> Int
-- responseStatusMessage ::
--   HttpResponse response => response -> ByteString
-- Network.HTTP.Req.responseTimeout ::
--   Int -> Network.HTTP.Req.Option scheme
-- runReq :: MonadIO m => HttpConfig -> Req a -> m a
-- urlQ ::
--   template-haskell-2.15.0.0:Language.Haskell.TH.Quote.QuasiQuoter
-- useHttpURI ::
--   modern-uri-0.3.3.0:Text.URI.Types.URI
--   -> Maybe (Url 'Http, Network.HTTP.Req.Option scheme)
-- useHttpsURI ::
--   modern-uri-0.3.3.0:Text.URI.Types.URI
--   -> Maybe (Url 'Https, Network.HTTP.Req.Option scheme)
-- useURI ::
--   modern-uri-0.3.3.0:Text.URI.Types.URI
--   -> Maybe
--        (Either
--           (Url 'Http, Network.HTTP.Req.Option scheme0)
--           (Url 'Https, Network.HTTP.Req.Option scheme1))
-- withReqManager :: MonadIO m => (Manager -> m a) -> m a
