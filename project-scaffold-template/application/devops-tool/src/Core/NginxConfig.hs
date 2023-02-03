-- | This module provide some functions to lookup and manupilate
-- the nginx config file.
module Core.NginxConfig
  ( changeNgDirective,
    changeNgDirectiveForFile,
    addNgDirective,
  )
where

import Data.Conf
import GHC.Base (errorWithoutStackTrace)
import Text.Megaparsec

changeNgDirectiveForFile :: ([ConfStatement] -> [ConfStatement]) -> FilePath -> IO ()
changeNgDirectiveForFile transform fp = do
  contents <- decodeUtf8 <$> readFileBS fp
  case parse conf fp contents of
    Left err -> errorWithoutStackTrace $ show err
    Right ast -> do
      let output = show (pPrintConf $ transform ast)
      writeFile fp output

changeNgDirective :: Text -> [Text] -> [ConfStatement] -> [ConfStatement]
changeNgDirective _ _ [] = []
changeNgDirective k' newValues (orig@(ConfStatementExpression (Expression k _) c) : xs) =
  let subR = changeNgDirective k' newValues xs
   in bool
        (orig : subR)
        (ConfStatementExpression (Expression k newValues) c : subR)
        $ k == k'
changeNgDirective k' newValues (orig@ConfStatementEmptyLine : xs) =
  orig : changeNgDirective k' newValues xs
changeNgDirective k' newValues (orig@(ConfStatementComment _) : xs) =
  orig : changeNgDirective k' newValues xs
changeNgDirective k' newValues ((ConfStatementBlock (Block bs css)) : xs) =
  ConfStatementBlock (Block bs $ changeNgDirective k' newValues css) :
  changeNgDirective k' newValues xs

addNgDirective :: Text -> [Text] -> [Text] -> [ConfStatement] -> [ConfStatement]
addNgDirective _ _ _ [] = []
addNgDirective k vs ctx (orig@(ConfStatementBlock (Block bs css)) : xs) =
  bool
    orig
    ( ConfStatementBlock
        ( Block
            bs
            ( ConfStatementExpression (Expression k vs) Nothing :
              css
            )
        )
    )
    (bs == ctx) :
  addNgDirective k vs ctx xs
addNgDirective k vs ctx (orig@ConfStatementEmptyLine : xs) =
  orig : addNgDirective k vs ctx xs
addNgDirective k vs ctx (orig@(ConfStatementComment _) : xs) =
  orig : addNgDirective k vs ctx xs
addNgDirective k vs ctx (orig@(ConfStatementExpression _ _) : xs) =
  orig : addNgDirective k vs ctx xs
