-- | This module parse the command line for the parameters
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

module CmdLine
( CmdOptions(..)
  , cmdOptions
 )
where

import Options.Applicative

import Paths_{{ name | toSnake }} (version)
import Data.Version (showVersion)

-- | put command line options here
-- | following are just for examples
-- | you must put your own cmd line options 
-- | and change the parsing logic
data CmdOptions = CmdOptions { cmdHost :: !Text
                             , cmdPort :: !Int
                             } deriving stock (Show)

versionOptionParser :: Parser (a -> a)
versionOptionParser = infoOption (showVersion version)
                        (long "version" <> short 'v' <> help "Show version")

cmdOptionsParser :: Parser CmdOptions
cmdOptionsParser = CmdOptions
  <$> strOption
               ( long "host"
               <> short 'm'
               <> metavar "HOST"
               <> value "localhost"
               <> showDefault
               <> help "The hostname/IP/DNS name of the target.")
  <*> option auto
               ( long "port"
               <> short 'p'
               <> metavar "PORT"
               <> value 9060
               <> showDefault
               <> help "The port number of the target.")

cmdOptions :: ParserInfo CmdOptions
cmdOptions = info (cmdOptionsParser <**> helper <**> versionOptionParser)
                ( fullDesc
                <> progDesc "{{description}}."
                <> header ("{{name}} " <> showVersion version <> " - {{description}}."))
