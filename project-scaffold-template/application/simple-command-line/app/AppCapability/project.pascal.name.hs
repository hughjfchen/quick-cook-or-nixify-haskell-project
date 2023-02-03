-- | This module implements the capability specified within the library src tree
-- | by providing the instances for the class defined within the library src tree

module AppCapability.{{ name | toPascal }}
( 
  ) where

import Has
import Error
import AppError

import AppEnv
import Capability.{{ name | toPascal }}

import AppM

