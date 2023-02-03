{-# LANGUAGE OverloadedStrings #-}
module Network.Wai.Middleware.Health
       (health)
where

import           Network.HTTP.Types        (methodGet, status200)
import           Network.HTTP.Types.Header (hContentType)
import           Network.Wai

health :: Middleware
health app req sendResponse =
  case rawPathInfo req of
    "/health" ->
      if getReq
        then healthy
        else next
    _ -> next
  where
    next = app req sendResponse
    getReq = requestMethod req == methodGet
    healthy = sendResponse $ responseLBS status200 headers "Healthy"
    headers = [(hContentType, "text/plain")]
