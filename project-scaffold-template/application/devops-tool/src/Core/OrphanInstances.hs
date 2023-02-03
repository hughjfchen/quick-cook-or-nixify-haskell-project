{-# OPTIONS_GHC -fno-warn-orphans #-}

-- This module generates some usefule instances for the Core.Types

module Core.OrphanInstances () where

import Core.Types
import Data.Aeson (FromJSON (..), ToJSON (..), object, withObject, (.:), (.=))

instance FromJSON CheckResultItem where
  parseJSON = withObject "CheckResultItem" $ \v -> do
    theHostName <- v .: "hostName"
    theInstance <- v .: "instance"
    no <- v .: "no"
    checkRule <- v .: "checkRule"
    checkItem <- v .: "checkItem"
    checkResult <- v .: "checkResult"
    status <- v .: "status"
    timestamp <- v .: "timestamp"
    pure $
      CheckResultItem
        { criHostName = theHostName,
          criInstance = theInstance,
          criNo = no,
          criCheckRule = checkRule,
          criCheckItem = checkItem,
          criCheckResult = checkResult,
          criStatus = status,
          criTimestamp = timestamp
        }

instance ToJSON CheckResultItem where
  toJSON CheckResultItem {..} =
    object
      [ "hostName" .= criHostName,
        "instance" .= criInstance,
        "no" .= criNo,
        "checkRule" .= criCheckRule,
        "checkItem" .= criCheckItem,
        "checkResult" .= criCheckResult,
        "status" .= criStatus,
        "timestamp" .= criTimestamp
      ]
