-- | This module define specs for executing WAS admin commands

{-# LANGUAGE FlexibleContexts #-}

module Capability.Exe{{ name | toPascal }}
  ( AuthM
  , ServerM
  , JVMM
  , Server(..)
  , JVM(..)
  , welcome
  , login
  , logout
  , listServers
  , pickServer
  , pickJvm
  , updateJvmGenericParameter
  , WelcomeState(..)
  , AuthState(..)
  , JVMUpdateState(..)
  ) where


import Has
import As
import Error
import Core.MyError

import Core.Types (JVMCmdLine)

type UserName = Text
newtype WelcomeState = Landed Text

newtype AuthState = Authed UserName

type CellName = Text
type NodeName = Text
type ServerName = Text
type CellID = Text
type NodeID = Text
type ServerID = Text

data Server = Server { cellName :: CellName
                     , cellID :: CellID
                     , nodeName :: NodeName
                     , nodeID :: NodeID
                     , serverName :: ServerName
                     , serverID :: ServerID
                     }

type JVMID = Text
type JVMName = Text

data JVM = JVM { jvmName :: JVMName
               , jvmID :: JVMID
               }

newtype JVMUpdateState = Success JVM

class (Monad m) => AuthM m where
  welcome :: (WithError err m, As err MyError, MonadReader env m, Has ConnectionInfo env, Has MyCookieJar env) => m ()
  login :: (WithError err m, As err MyError, MonadReader env m, Has ConnectionInfo env, Has AuthInfo env, Has MyCookieJar env) => m ()
  logout :: (WithError err m, As err MyError, MonadReader env m, Has ConnectionInfo env, Has MyCookieJar env) => m ()

class (AuthM m) => ServerM m where
  listServers :: (MonadReader env m, Has ConnectionInfo env, Has MyCookieJar env) => m [Server]
  pickServer :: (MonadReader env m, Has ConnectionInfo env, Has MyCookieJar env) => Server -> m Server

class (ServerM m) => JVMM m where
  pickJvm :: (MonadReader env m, Has ConnectionInfo env, Has MyCookieJar env) => Server -> m JVM
  updateJvmGenericParameter :: (WithError err m, As err MyError, MonadReader env m, Has ConnectionInfo env, Has MyCookieJar env) =>
                            JVM
                            -> JVMCmdLine
                            -> m JVMUpdateState

