module Spec (main) where

import Core.NginxCmdLineParser

-- import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range

-- import qualified Hedgehog.Gen as Gen
-- import qualified Hedgehog.Range as Range
import Core.NginxConfig (addNgDirective, changeNgDirective, changeNgDirectiveForFile)
import Core.NginxDefaultSettingParser (parseNginxDefaultSetting)
import Core.Types
import Data.Conf (
  Block (Block),
  ConfStatement (ConfStatementBlock, ConfStatementExpression),
  Expression (Expression),
 )
import Test.Hspec

-- import Test.Hspec.Hedgehog

{- | Following is only for example
 | you must adapt it accordingly
-}
main :: IO ()
main = hspec $
  describe "test keep-nginx-compliance" $
    do
      it "test checkResultItemsToInstructions" $
        checkResultItemsToInstructions
          [ CheckResultItem
              { criHostName = "wasdev01kf"
              , criInstance = "server1"
              , criNo = "NA"
              , criCheckRule = "Log"
              , criCheckItem = "AccessLog"
              , criCheckResult = "true"
              , criStatus = "OK"
              , criTimestamp = "2022-08-01 16:45:00"
              }
          , CheckResultItem
              { criHostName = "wasdev01kf"
              , criInstance = "server1"
              , criNo = "NA"
              , criCheckRule = "Log"
              , criCheckItem = "LogRotate"
              , criCheckResult = "false"
              , criStatus = "ERROR"
              , criTimestamp = "2022-08-01 16:45:00"
              }
          , CheckResultItem
              { criHostName = "wasdev01kf"
              , criInstance = "server2"
              , criNo = "NA"
              , criCheckRule = "Log"
              , criCheckItem = "Group"
              , criCheckResult = "false"
              , criStatus = "ERROR"
              , criTimestamp = "2022-08-01 16:45:00"
              }
          ]
          `shouldBe` Instruction
            { insAction = Enforce
            , insModule = [LogRotate, Group]
            }

      it
        "test nginx command line without paramaters"
        $ parseNginxCmd "nginx: master process /opt/openresty/bin/openresty"
          `shouldBe` Right
            ( NginxCmd
                { ngxCmdPrefix = Nothing
                , ngxCmdConfPath = Nothing
                , ngxCmdErrorFile = Nothing
                , ngxCmdGlobalDirectives = Nothing
                }
            )

      it
        "test nginx command line with an extra parameters"
        $ parseNginxCmd "nginx: master process /opt/openresty/bin/openresty -p /opt/mysoft/openresty -e /dev/null -g daemon on; pid mypid; -c /etc/openresty.conf"
          `shouldBe` Right
            ( NginxCmd
                { ngxCmdPrefix = Just $ MyNginxPrefix "/opt/mysoft/openresty"
                , ngxCmdConfPath = Just $ MyNginxConfPath "/etc/openresty.conf"
                , ngxCmdErrorFile = Just "/dev/null"
                , ngxCmdGlobalDirectives = Just "daemon on;pid mypid;"
                }
            )

      it
        "test my nginx default setting parser"
        $ parseNginxDefaultSetting
          ( unlines
              [ "nginx version: openresty/1.19.9.1"
              , "Usage: nginx [-?hvVtTq] [-s signal] [-p prefix]"
              , "[-e filename] [-c filename] [-g directives]"
              , ""
              , "Options:"
              , "-?,-h         : this help"
              , "-v            : show version and exit"
              , "-V            : show version and configure options then exit"
              , "-t            : test configuration and exit"
              , "-T            : test configuration, dump it and exit"
              , "-q            : suppress non-error messages during configuration testing"
              , "-s signal     : send signal to a master process: stop, quit, reopen, reload"
              , "-p prefix     : set prefix path (default: /nix/store/hhbk1baii18sxvj0dk2dclqrzsf5bkfk-openresty-1.19.9.1/nginx/)"
              , "-e filename   : set error log file (default: /var/log/nginx/error.log)"
              , "-c filename   : set configuration file (default: conf/nginx.conf)"
              , "-g directives : set global directives out of configuration file"
              , ""
              ]
          )
          `shouldBe` Right ("/nix/store/hhbk1baii18sxvj0dk2dclqrzsf5bkfk-openresty-1.19.9.1/nginx/", "conf/nginx.conf")

      it
        "test my nginx config parser - changeNgDirective "
        $ changeNgDirectiveForFile
          (changeNgDirective "server_tokens" ["off"])
          "/home/ubuntu/projects/keep-nginx-compliance/test/sample/nginx.conf"
          `shouldReturn` ()
      it
        "test my nginx config parser - addNgDirective "
        $ addNgDirective
          "server_tokens"
          ["off"]
          ["http"]
          [ ConfStatementBlock $
              Block ["http"] []
          ]
          `shouldBe` [ ConfStatementBlock $
                        Block
                          ["http"]
                          [ ConfStatementExpression
                              (Expression "server_tokens" ["off"])
                              Nothing
                          ]
                     ]
