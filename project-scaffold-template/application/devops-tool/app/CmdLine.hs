-- | This module parse the command line for the parameters
module CmdLine
  ( cmdOptions,
    CmdLineOpts (..),
    cmdToInstruction,
  )
where

import Core.OrphanInstances ()
import Core.Types
  ( Instruction (Instruction, insModule),
    allModules,
  )
import qualified Core.Types as T
import Data.Version (showVersion)
import Options.Applicative
import Paths_keep_nginx_compliance (version)

newtype CmdLineOpts
  = CmdLineInstruction Instruction
  deriving stock (Eq, Ord, Show, Typeable, Generic)

cmdLineInstructionParser :: Parser CmdLineOpts
cmdLineInstructionParser = CmdLineInstruction <$> appEnvParser

cmdLineOptsParser :: Parser CmdLineOpts
cmdLineOptsParser = cmdLineInstructionParser

moduleParser :: ReadM T.Module
moduleParser = eitherReader T.strModule

versionOptionParser :: Parser (a -> a)
versionOptionParser =
  infoOption
    (showVersion version)
    (long "version" <> short 'v' <> help "Show version")

appEnvParser :: Parser Instruction
appEnvParser =
  Instruction
    <$> subparser
      ( command "check" (info (pure T.Check) (progDesc "check if in compliance with the Operation Guideline"))
          <> command "enforce" (info (pure T.Enforce) (progDesc "enforce to comply with the Operation Guideline"))
          <> command "list" (info (pure T.List) (progDesc "list the supported check/enforce modules"))
      )
    <*> some
      ( argument
          moduleParser
          ( metavar "MODULE"
              -- Comment the following lines out to avoid GHC hang. Not sure why yet
              -- OK, this is a known issue, refer to https://hackage.haskell.org/package/optparse-applicative-0.17.0.0/docs/Options-Applicative.html#value
              -- for more info
              -- <> value T.All
              -- <> showDefault
              <> help "The space-separated module list to be checked or enforced. Use 'list all' command to list all supported modules."
          )
      )

cmdOptionsInfo :: ParserInfo CmdLineOpts
cmdOptionsInfo =
  info
    (cmdLineOptsParser <**> helper <**> versionOptionParser)
    ( fullDesc
        <> progDesc "keep nginx in compliance with the Operation Guideline."
        <> header ("keep-nginx-compliance " <> showVersion version <> " - keep nginx in compliance with the Operation Guideline.")
    )

cmdOptions :: IO CmdLineOpts
cmdOptions = execParser cmdOptionsInfo

cmdToInstruction :: CmdLineOpts -> IO Instruction
cmdToInstruction (CmdLineInstruction instruct) =
  pure $
    bool instruct (instruct {insModule = allModules}) $ insModule instruct == [T.All]
