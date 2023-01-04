
module Main (main) where

import qualified Conferer as Conf

import AppEnv
import AppM
import Core.Types
import AppCapability.{{ name | toPascal }}()
import Core.{{ name | toPascal }}

main :: IO ()
main = do
  config <- Conf.mkConfig "{{name|toPascal}}"
  appEnv <- Conf.fetch config :: IO AppEnv

  print $ appEnvDatabase appEnv
  print $ appEnvPool appEnv
  print $ appEnv{{name|toPascal}} appEnv

  case appEnvCommand appEnv of
    Start -> defaultStartCommand
    Stop -> defaultStopCommand
    Status -> defaultStatusCommand
    Unknown -> error "Unknown command, it must be start, stop or status."
  where
    defaultStartCommand = putStrLn "Please implement the start command" 
    defaultStopCommand = putStrLn "Please implement the stop command" 
    defaultStatusCommand = putStrLn "Please implement the status command" 
