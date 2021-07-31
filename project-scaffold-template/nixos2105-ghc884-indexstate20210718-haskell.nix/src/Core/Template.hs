{- | This module is the core business logic
Copyright: (c) 2021 Hugh JF Chen
SPDX-License-Identifier: MIT
Maintainer: Hugh JF Chen <hugh.jf.chen@gmail.com>

Core business logic here.
-}

module Core.{{ name | toPascal }}
       ( 
       ) where

import Core.Types

import Has
import As
import Error
import Core.MyError
import Capability.{{ name | toPascal }}

