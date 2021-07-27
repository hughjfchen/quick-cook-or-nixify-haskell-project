{- | This module is the core business logic
Copyright: (c) 2020 Hugh JF Chen
SPDX-License-Identifier: MIT
Maintainer: Hugh JF Chen <hugh.jf.chen@gmail.com>

Take the provision of a WebSphere cell.
-}

module Core.{{ name | toPascal }}
       ( changeJVMParameters
       ) where

import Core.Types (JVMCmdLine(..))

import Has
import As
import Error
import Core.MyError
import Capability.Exe{{ name | toPascal }}

changeJVMParameters :: (WithError err m, As err MyError, MonadReader env m
                       , Has ConnectionInfo env, Has AuthInfo env, JVMM m, Has MyCookieJar env)
                    => JVMCmdLine
                    -> m [JVMUpdateState]
changeJVMParameters jvmCmd = welcome >> login >> listServers >>= changeAllJvmPara
  where changeAllJvmPara = mapM  (\y -> pickServer y >>= pickJvm >>= flip updateJvmGenericParameter jvmCmd)
