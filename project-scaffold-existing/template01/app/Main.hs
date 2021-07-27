
module Main (main) where

import Options.Applicative
import CmdLine

import AppEnv
import AppM
import Core.Types
import AppCapability.{{ name | toPascal }}()
import Core.{{ name | toPascal }}

main :: IO ()
main = void $ execParser cmdOptions >>= mkAppEnv >>= \env -> runAppM env $ pure ()
