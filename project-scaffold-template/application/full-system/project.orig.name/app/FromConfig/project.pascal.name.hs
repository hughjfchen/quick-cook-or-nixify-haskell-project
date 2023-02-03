-- |
-- Copyright: (c) 2021 Hugh JF Chen
-- License: MIT
-- Maintainer: Hugh JF Chen <hugh.jf.chen@gmail.com>
-- Stability: stable
-- Portability: portable
--
-- FromConfig instance for {{ name | toPascal }}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards #-}
module FromConfig.{{ name | toPascal }} where

import Conferer.FromConfig

import Data.Dynamic

import Core.Types ({{ name | toPascal}}(..))

-- | Deconstruct a 'Core.Types.{{name|toPascal}}' into a many key/dynamic pairs to
-- provide valid defaults for downstream 'fetchFromConfig'
deconstruct{{name|toPascal}}ToDefaults :: {{name|toPascal}} -> [(Key, Dynamic)]
deconstruct{{name|toPascal}}ToDefaults {{name|toPascal}}{..} =
  [ ("field1", toDyn field1)
  , ("field2", toDyn field2)
  ]

instance DefaultConfig {{name|toPascal}} where
  configDef = {{name|toPascal}} { field1 = 0
                                , field2 = "field2"
                                }

instance FromConfig {{name|toPascal}} where
  fromConfig key originalConfig = do
    config <- addDefaultsAfterDeconstructingToDefaults deconstruct{{name|toPascal}}ToDefaults key originalConfig

    field1 <- fetchFromConfig (key /. "field1") config
    field2 <- fetchFromConfig (key /. "field2") config

    pure {{name|toPascal}}{..}

