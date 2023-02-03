-- | This module parse the command line for the parameters
module Core.NginxCmdLineParser
  ( parseNginxCmd,
    nginxCmdLine2NgixCmd,
  )
where

import Core.MyError
import Core.OrphanInstances ()
import Core.Types
  ( MyNginxCmdLineParas (..),
    MyNginxConfPath (MyNginxConfPath),
    MyNginxPrefix (MyNginxPrefix),
    NginxCmd (..),
  )
import Text.Megaparsec
import qualified Text.Megaparsec as P
import Text.Megaparsec.Char

type Parser = Parsec Void Text

pathCharP :: Parser Char
pathCharP =
  alphaNumChar
    <|> char '/'
    <|> char '.'
    <|> char '-'
    <|> char '_'

nginxCommandP :: Parser String
nginxCommandP =
  P.some pathCharP

nginxPCmdParaP :: Parser MyNginxCmdLineParas
nginxPCmdParaP = do
  void $ string " -p "
  pPath <- P.some pathCharP
  pure $ MyCmdLinePrefix pPath

nginxCCmdParaP :: Parser MyNginxCmdLineParas
nginxCCmdParaP = do
  void $ string " -c "
  cPath <- P.some pathCharP
  pure $ MyCmdLineConfig cPath

nginxECmdParaP :: Parser MyNginxCmdLineParas
nginxECmdParaP = do
  void $ string " -e "
  ePath <- P.some pathCharP
  pure $ MyCmdLineError ePath

nginxGCmdParaP :: Parser MyNginxCmdLineParas
nginxGCmdParaP = do
  void $ string " -g "
  gPath <-
    P.someTill
      ( do
          skipMany $ char ' '
          p1 <- P.some pathCharP
          skipMany $ char ' '
          p2 <- P.some pathCharP
          void $ char ';'
          pure $ p1 <> " " <> p2 <> ";"
      )
      $ try $ lookAhead $ string " -"
  pure $ MyCmdLineGlobal $ concat gPath

nginxParser :: Parser NginxCmd
nginxParser = do
  void $ string "nginx: master process "
  void nginxCommandP
  paras <-
    P.many $
      choice
        [ try nginxPCmdParaP,
          try nginxCCmdParaP,
          try nginxECmdParaP,
          try nginxGCmdParaP
        ]
  pure $ nginxCmdLine2NgixCmd paras

defaultNginxCmd :: NginxCmd
defaultNginxCmd =
  NginxCmd
    { ngxCmdPrefix = Nothing,
      ngxCmdConfPath = Nothing,
      ngxCmdErrorFile = Nothing,
      ngxCmdGlobalDirectives = Nothing
    }

nginxCmdPara2Record :: MyNginxCmdLineParas -> NginxCmd -> NginxCmd
nginxCmdPara2Record (MyCmdLinePrefix p) prev =
  prev
    { ngxCmdPrefix = Just $ MyNginxPrefix p
    }
nginxCmdPara2Record (MyCmdLineConfig c) prev =
  prev
    { ngxCmdConfPath = Just $ MyNginxConfPath c
    }
nginxCmdPara2Record (MyCmdLineError e) prev =
  prev
    { ngxCmdErrorFile = Just e
    }
nginxCmdPara2Record (MyCmdLineGlobal g) prev =
  prev
    { ngxCmdGlobalDirectives = Just g
    }

nginxCmdLine2NgixCmd :: [MyNginxCmdLineParas] -> NginxCmd
nginxCmdLine2NgixCmd = foldr nginxCmdPara2Record defaultNginxCmd

parseNginxCmd :: Text -> Either MyError NginxCmd
parseNginxCmd cmd =
  first (InvalidNginxCommandLine . toText . errorBundlePretty) $
    parse nginxParser "" cmd
