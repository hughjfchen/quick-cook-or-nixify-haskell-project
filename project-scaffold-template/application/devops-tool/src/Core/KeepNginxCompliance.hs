{-# LANGUAGE RankNTypes #-}

-- | This module is the core business logic
-- Copyright: (c) 2021 Hugh JF Chen
-- SPDX-License-Identifier: MIT
-- Maintainer: Hugh JF Chen <hugh.jf.chen@gmail.com>
--
-- Core business logic here.
module Core.KeepNginxCompliance
  ( keepCompliance,
  )
where

import As
import Capability.HostInfo
import Capability.KeepNginxCompliance
import Capability.TimeStamp
import Core.MyError
import Core.OrphanInstances ()
import Core.Types
import Data.Aeson.Encode.Pretty (encodePretty)
import Error

checkToResultItems ::
  ( WithError err m,
    As err MyError,
    KeepNginxComplianceM m,
    HostInfoM m,
    TimeStampM m
  ) =>
  Instruction ->
  [Target] ->
  m [CheckResultItem]
checkToResultItems inx ngxs = do
  hostN <- getHostName
  curTS <- getCurrentTimeStamp
  crss <- check inx ngxs
  pure $ concat $ zipWith (buildUpCRI hostN curTS) ngxs crss
  where
    buildUpCRI h t ng crs =
      zipWith (buildOneNgxCRI h t ng) (insModule inx) crs
    buildOneNgxCRI h' t' ng' m ngcr =
      CheckResultItem
        { criHostName = h',
          criInstance =
            toText $
              getMyNginxFullPathExe (targetFullPathExe ng')
                <> ":"
                <> getMyNginxPrefix (targetPrefix ng')
                <> ":"
                <> getMyNginxConfPath (targetConfPath ng')
                <> ":"
                <> show (targetConfigFrom ng'),
          criNo = "",
          criCheckRule = show m,
          criCheckItem = show m,
          criCheckResult = show ngcr,
          criStatus = ngCR2Status ngcr,
          criTimestamp = t'
        }

keepCompliance' ::
  ( WithError err m,
    As err MyError,
    MonadIO m,
    KeepNginxComplianceM m,
    HostInfoM m,
    TimeStampM m
  ) =>
  Instruction ->
  [Target] ->
  m ()
keepCompliance' inx@Instruction {insAction = Check} ngxs = do
  cris <- checkToResultItems inx ngxs
  putStrLn $
    decodeUtf8 $
      encodePretty cris
keepCompliance' Instruction {insAction = Enforce} ngxs =
  checkToResultItems Instruction {insAction = Check, insModule = allModules} ngxs
    >>= flip comply ngxs . checkResultItemsToInstructions
keepCompliance' Instruction {insAction = List} _ =
  liftIO $
    putTextLn $
      unwords
        [ "Supported modules: all, group, runas, accesslog, errorlog, ",
          "listingsdisabled,",
          " versioninvisible, service and logrotate"
        ]

keepCompliance ::
  ( WithError err m,
    As err MyError,
    MonadIO m,
    KeepNginxComplianceM m,
    HostInfoM m,
    TimeStampM m
  ) =>
  Instruction ->
  [Target] ->
  m ()
keepCompliance = keepCompliance'
