-- | This module parse the command line for the parameters
module NginxCmdLineParser (
  nginxParser,
) where

import Core.OrphanInstances ()
import Core.Types (MyNginxConfPath (MyNginxConfPath), MyNginxPrefix (..), NginxCmd (NginxCmd))

import Text.Parsec
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PC
import Text.Parsec.Text (Parser)

cmdLineSpace :: Parser Char
cmdLineSpace = char ' '

nginxNginx :: Parser Text
nginxNginx = toText <$> string "nginx:"

nginxMaster :: Parser Text
nginxMaster = toText <$> string "master"

nginxProcess :: Parser Text
nginxProcess = toText <$> string "process"

nginxCommand :: Parser Text
nginxCommand =
  toText
    <$> manyTill
      anyChar
      ( string "nginx"
          P.<|> string "openresty"
          <?> "process must be nginx or openresty"
      )

commandOptionSep :: Parser Char
commandOptionSep = char '-'

nginxPrefix :: Parser Char
nginxPrefix = char 'p'

nginxConfPath :: Parser Char
nginxConfPath = char 'c'

nginxError :: Parser Char
nginxError = char 'e'

nginxGlobalDiretives :: Parser Char
nginxGlobalDiretives = char 'g'

nginxParser :: Parser Text
nginxParser = nginxNginx >> cmdLineSpace >> nginxMaster >> cmdLineSpace >> nginxProcess >> cmdLineSpace >> nginxCommand

-- $> parseTest nginxParser "nginx: master process /opt/asdf/bin/nginx"
