-- | This module includes some utilities
module Utils
  ( someDirToAbs,
    someFileToAbs,
    filterFile,
    filterFileRel,
    oneFileInDir,
    oneFileInDirRel,
    findOneFileInDir,
    findOneFileInDirRel,
    lastAfterSplit,
    lastlastAfterSplit,
    firstAfterSplit,
    firstfirstAfterSplit,
    isScriptGenThreadDump,
    isScriptGenHeapDump,
    isHeapDumpReport,
    isWASGCLog,
  )
where

import AppError
import As
import Control.Monad.Catch
import qualified Data.List.NonEmpty as LN
import qualified Data.Text as T
import Error
import Path
import Path.IO (AnyPath (makeAbsolute), listDir, listDirRel)
import Text.Parsec
  ( anyChar,
    char,
    count,
    eof,
    many1,
    manyTill,
    parse,
    string,
  )
import qualified Text.Parsec as P
import qualified Text.Parsec.Char as PC
import Text.Parsec.Text (Parser)

-- | Convert SomeBase Dir from Path package to an absolute
--  Dir using makeAbsolute from path-io package.
someDirToAbs :: (MonadIO m) => SomeBase Dir -> m (Path Abs Dir)
someDirToAbs base = do
  case base of
    Abs base' -> pure base'
    Rel base' -> makeAbsolute base'

-- | Convert SomeBase File from Path package to an absolute
--  File using makeAbsolute from path-io package.
someFileToAbs :: (MonadIO m) => SomeBase File -> m (Path Abs File)
someFileToAbs base = do
  case base of
    Abs base' -> pure base'
    Rel base' -> makeAbsolute base'

-- | Find some files through a filter predicate function.
--  It only returns files with absolute path and discard
--  sub diretories.
filterFile :: (MonadThrow m, MonadIO m) => Path Abs Dir -> (Path Abs File -> m Bool) -> m [Path Abs File]
filterFile home p = listDir home >>= \(_, files) -> filterM p files

-- | Find some files through a filter predicate function.
--  It only returns files relative to the given directory
--  and discard sub directories.
filterFileRel :: (MonadThrow m, MonadIO m) => Path Abs Dir -> (Path Rel File -> m Bool) -> m [Path Rel File]
filterFileRel home p = listDirRel home >>= \(_, files) -> filterM p files

-- | Check if a list of File path contains only one file,
--  if yes, return it, otherwise, throw error. Abs File version
oneFileInDir :: (WithError err m, As err AppError) => [Path Abs File] -> m (Path Abs File)
oneFileInDir [] = throwError $ as $ NoSuchFileInDir "Found no such file with the searching criterates in dir."
oneFileInDir [h] = pure h
oneFileInDir files@(_ : _) = throwError $ as $ TooManySameFileInDir $ "Found more than one files with the searching criteriates in dir. They are: " <> show files

-- | Check if a list of File path contains only one file,
--  if yes, return it, otherwise, throw error. Rel File version
oneFileInDirRel :: (WithError err m, As err AppError) => [Path Abs File] -> m (Path Abs File)
oneFileInDirRel [] = throwError $ as $ NoSuchFileInDir "Found no such file with the searching criterates in dir."
oneFileInDirRel [h] = pure h
oneFileInDirRel (_ : _) = throwError $ as $ TooManySameFileInDir "Found more than one files with the searching criteriates in dir."

-- | Searching for only one file with the given criterate
--  in the Dir. Return it if found, otherwise throw AppError
findOneFileInDir ::
  (WithError err m, As err AppError, MonadThrow m, MonadIO m) =>
  Path Abs Dir ->
  (Path Abs File -> m Bool) ->
  m (Path Abs File)
findOneFileInDir home p = do
  files <- filterFile home p
  oneFileInDir files

-- | Searching for only one file with the given criterate
--  in the Dir. Return it if found, otherwise throw AppError
findOneFileInDirRel ::
  (WithError err m, As err AppError, MonadThrow m, MonadIO m) =>
  Path Abs Dir ->
  (Path Abs File -> m Bool) ->
  m (Path Abs File)
findOneFileInDirRel home p = do
  files <- filterFile home p
  oneFileInDir files

-- | Return the last part after split a text with a separator
lastAfterSplit :: Text -> String -> Text
lastAfterSplit s = last . LN.fromList . T.splitOn s . toText

-- | Return the last last part after split a text with a separator
lastlastAfterSplit :: Text -> String -> Text
lastlastAfterSplit s = head . LN.fromList . tail . LN.reverse . LN.fromList . T.splitOn s . toText

-- | Return the first part after split a text with a separator
firstAfterSplit :: Text -> String -> Text
firstAfterSplit s = head . LN.fromList . T.splitOn s . toText

-- | Return the first first part after split a text with a separator
firstfirstAfterSplit :: Text -> String -> Text
firstfirstAfterSplit s = head . LN.fromList . tail . LN.fromList . T.splitOn s . toText

-- | following are the parsers for the dump-collect script generated files
dumpFileNameSep :: Parser Char
dumpFileNameSep = char '.'

threadDumpName :: Parser String
threadDumpName = string "threadump"

heapdumpName :: Parser String
heapdumpName = string "heapdump"

dumpFileJVMPID :: Parser String
dumpFileJVMPID = many1 PC.digit

dumpFileDate :: Parser String
dumpFileDate = count 8 PC.digit

dumpFileTime :: Parser String
dumpFileTime = count 6 PC.digit

threadDumpFileNameP :: Parser ()
threadDumpFileNameP = do
  threadDumpName >> dumpFileNameSep >> dumpFileJVMPID >> dumpFileNameSep >> dumpFileDate >> dumpFileNameSep >> dumpFileTime >> eof

heapDumpFileNameP :: Parser ()
heapDumpFileNameP = do
  heapdumpName >> dumpFileNameSep >> dumpFileJVMPID >> dumpFileNameSep >> dumpFileDate >> dumpFileNameSep >> dumpFileTime >> eof

isScriptGenThreadDump :: Path Abs File -> Bool
isScriptGenThreadDump f = toFilePath f & toText & parse threadDumpFileNameP "" & isRight

isScriptGenHeapDump :: Path Abs File -> Bool
isScriptGenHeapDump f = toFilePath f & toText & parse heapDumpFileNameP "" & isRight

fileNameEndWith :: Text -> Parser ()
fileNameEndWith suffix = manyTill anyChar (P.try $ string $ toString suffix) >> eof

isHeapDumpReport :: Text -> Path Abs File -> Bool
isHeapDumpReport s f = toFilePath f & toText & parse (fileNameEndWith s) "" & isRight

-- >>> parse (fileNameEndWith "_Leak_Suspects.zip") "" "/var/jauser/data/parsed_report_mat/heapdump.20201109.112259.6793.0002_Leak_Suspects.zip" & isRight
-- True

gcWASLogNamePre :: Parser String
gcWASLogNamePre = string "native_stderr"

gcWASLogNameSuffix :: Parser String
gcWASLogNameSuffix = string "log"

gcWASLogName :: Parser ()
gcWASLogName = do
  gcWASLogNamePre >> dumpFileNameSep >> gcWASLogNameSuffix >> eof

isWASGCLog :: Path Abs File -> Bool
isWASGCLog f = toFilePath f & toText & parse gcWASLogName "" & isRight
