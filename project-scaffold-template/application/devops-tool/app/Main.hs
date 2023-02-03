-- | This is the entry point to the main program
module Main (main) where

import AppCapability.AppHostInfo ()
import AppCapability.AppTimeStamp ()
import AppCapability.KeepNginxCompliance ()
import AppEnv (envs2Targets, initAppEnv, nullAppEnv)
import AppM (runAppM)
import CmdLine
import Core.KeepNginxCompliance
import Core.Types ()
import Propellor.Base (checkDebugMode, onlyProcess, (</>))
import Propellor.Debug (debug)
import Propellor.Utilities
import System.PosixCompat (getRealUserID)

checkSystemEnv :: IO ()
checkSystemEnv = do
  Propellor.Utilities.ifM
    ((==) 0 <$> getRealUserID)
    ( getHomeDirectory
        >>= \h -> onlyProcess (h </> ".keep-nginx-compliance.lock") pass,
      giveup "this program can only run with root."
    )

main :: IO ()
main = do
  cmd <- cmdOptions
  checkDebugMode
  checkSystemEnv
  myAppEnv <- initAppEnv
  debug ["appEnv: ", show myAppEnv]
  bool pass (giveup "no nginx found, quit") $ null myAppEnv
  inx <- cmdToInstruction cmd
  let ngxs = envs2Targets myAppEnv
  nullAppEnv >>= \nullEnv' -> runAppM nullEnv' $ keepCompliance inx ngxs
