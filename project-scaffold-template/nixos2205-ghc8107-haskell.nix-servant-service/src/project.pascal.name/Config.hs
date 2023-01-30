-- |Config utility module, uses System.Envy to read ENV values and builds a data
module {{name|toPascal}}.Config where

import qualified System.Envy as Envy
import System.IO (hPutStrLn, stderr)

-- | Read from ENVIRONMENT Variables and return your target `t` data
-- `t` should have a instance of FromEnv
-- Example: 
-- @ 
--    instance FromEnv MyAppSettings
-- @
withAppSettingsFromEnv :: Envy.FromEnv t => (t -> IO ()) -> IO ()
withAppSettingsFromEnv f = Envy.decodeEnv >>= callF
  where
    callF x =
      case x of
        Left e -> hPutStrLn stderr ("Error reading env: " ++ e)
        Right c -> f c
