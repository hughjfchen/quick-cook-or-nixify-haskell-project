{-# LANGUAGE NoImplicitPrelude   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |Contains JWT authentication settings
module {{name|toPascal}}.JWT where

import           Crypto.JOSE         (JWK, JWKSet (..))
import           Crypto.JWT          (StringOrURI, string, uri)
import qualified Data.Aeson          as Aeson
import           Network.URI         (parseURI)
import           RIO
import           RIO.ByteString      (readFile)
import qualified RIO.Text            as T
import           Servant.Auth.Server (IsMatch (..), JWTSettings (..),
                                      generateKey)
import           System.Environment  (lookupEnv)

-- |Build JWT settings to be used in Servant Auth context
-- Looks for `JWK_AUDIENCES` and `JWK_PATH` in environment values
-- to load the sig file and value to verify the incoming jwt audience claim
getJWTAuthSettings :: MonadUnliftIO m => m JWTSettings
getJWTAuthSettings = do
  jwkSet <- liftIO acquireJwks
  signKey <- liftIO generateKey
  audienceCfg <- liftIO $ lookupEnv "JWK_AUDIENCES"
  let audMatches = maybe (const Matches) checkAud audienceCfg
      checkAud audConfig = \tokenAud ->
        if RIO.preview uri tokenAud == parseURI audConfig || RIO.preview string tokenAud == Just (T.pack audConfig) then
          Matches else DoesNotMatch
  return $ buildJWTSettings signKey jwkSet audMatches

buildJWTSettings :: JWK -> JWKSet -> (StringOrURI -> IsMatch) -> JWTSettings
buildJWTSettings signKey jwkSet audMatches =
  JWTSettings
    { signingKey = signKey,
      jwtAlg = Nothing,
      validationKeys = vkeys signKey jwkSet,
      audienceMatches = audMatches
    }
  where
    vkeys k (JWKSet x) = JWKSet (x ++ [k])

acquireJwks :: IO JWKSet
acquireJwks = do
  envUrl <- lookupEnv "JWK_PATH"
  let jwkPath = fromMaybe "secrets/jwk.sig" envUrl
  fileContent <- readFile jwkPath
  let parsed = Aeson.eitherDecodeStrict fileContent
  return $ either (\e -> error $ "Invalid JWK file: " <> e) id parsed
