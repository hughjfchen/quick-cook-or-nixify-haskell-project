{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE DeriveGeneric        #-}
{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |Logging utility functions
module {{name|toPascal}}.Logging
  ( LogMessage,
    ModLogger,
    Formatter,
    jsonFormatter,
    newLogger,
    buildLogger,
  )
where

import           Data.Aeson            (ToJSON (toEncoding), defaultOptions,
                                        encode, genericToEncoding)
import           Data.Has              (Has (hasLens))
import           RIO
import           System.Log.FastLogger

type ModLogger = LogFunc

type Formatter = TimedFastLogger -> CallStack -> LogSource -> LogLevel -> Utf8Builder -> IO ()

data LogMessage = LogMessage
  { message        :: !Text,
    logSource      :: !Text,
    callStack      :: !Text,
    timestamp      :: !Text,
    level          :: !Text,
    appVersion     :: !Text,
    appEnvironment :: !Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON LogMessage where
  toEncoding = genericToEncoding defaultOptions

instance ToLogStr LogMessage where
  toLogStr a = (toLogStr . encode $ a) <> "\n"

instance {-# OVERLAPPABLE #-} Has ModLogger a => HasLogFunc a where
  logFuncL = hasLens

-- | Creates a logger module using a given formatting function.
-- | Also returns the underlying TimedFastLogger for use outside of your app (e.g. in some WAI middleware).
newLogger :: LogType -> Formatter -> IO (TimedFastLogger, ModLogger)
newLogger logtype formatter = do
  tc <- newTimeCache simpleTimeFormat'
  (fl, _cleanupAction) <- newTimedFastLogger tc logtype
  -- todo clean up
  return (fl, mkLogFunc $ formatter fl)

-- | Builds LogMessage and json encodes to string
jsonFormatter :: Text -> Text -> Formatter
jsonFormatter envName appVer logger cs src logLvl msg = logger buildJsonLogMsg
  where
    showLevel LevelDebug     = "debug"
    showLevel LevelInfo      = "info"
    showLevel LevelWarn      = "warn"
    showLevel LevelError     = "error"
    showLevel (LevelOther t) = "" <> t <> ""
    buildJsonLogMsg t =
      toLogStr $
        LogMessage
          (utf8BuilderToText msg)
          (utf8BuilderToText . displayBytesUtf8 . fromLogStr . toLogStr $ src)
          (utf8BuilderToText $ displayCallStack cs)
          (utf8BuilderToText . displayBytesUtf8 $  t)
          (showLevel logLvl)
          appVer
          envName

-- | Convenient function to create json formatted logger with appName & appVer values
buildLogger :: Text -> Text -> IO ModLogger
buildLogger envName appVer = do
  (_, lf) <- newLogger (LogStderr defaultBufSize) (jsonFormatter envName appVer)
  return lf
