-- | This module parse the nginx help message output for the default value of some parameters
module Core.NginxDefaultSettingParser
  ( parseNginxDefaultSetting,
  )
where

import Core.MyError
import Core.OrphanInstances ()
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

nginxDefaultSettingP :: Parser (FilePath, FilePath)
nginxDefaultSettingP = do
  void $ count 12 $ skipManyTill printChar eol
  skipMany $ char ' '
  void $ string "-p prefix "
  skipMany $ char ' '
  void $ char ':'
  skipMany $ char ' '
  void $ string "set prefix path (default:"
  skipMany $ char ' '
  pDefault <- P.some pathCharP
  void $ char ')'
  P.skipMany eol

  void $ count 1 $ P.skipManyTill printChar eol

  skipMany $ char ' '
  void $ string "-c filename "
  skipMany $ char ' '
  void $ char ':'
  skipMany $ char ' '
  void $ string "set configuration file (default:"
  skipMany $ char ' '
  cDefault <- P.some pathCharP
  void $ char ')'

  pure (pDefault, cDefault)

parseNginxDefaultSetting :: Text -> Either MyError (FilePath, FilePath)
parseNginxDefaultSetting helpMsg =
  first (InvalidNginxCommandLine . toText . errorBundlePretty) $
    parse nginxDefaultSettingP "" helpMsg
