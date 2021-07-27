
module Main (main) where

import Options.Applicative
import CmdLine

import AppEnv
import AppM
import Core.Types (JVMCmdLine(..), Property(..))
import AppCapability.Exe{{ name | toPascal }}()
import Core.{{ name | toPascal }} (changeJVMParameters)

main :: IO ()
main = void $ execParser cmdOptions >>= mkAppEnv >>= \env -> runAppM env $ changeJVMParameters $ JVMCmdLineProperty $ Property "M" "MM"
