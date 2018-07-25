{-# LANGUAGE OverloadedStrings #-}

module Pull where

import           Control.Exception
import           Control.Monad             (filterM, when)
import           Control.Monad.Managed     (with)
import qualified Data.Text                 as Text
import qualified Filesystem.Path           as Path
import qualified Filesystem.Path.CurrentOS as Path
import           System.Directory
import qualified Turtle.Prelude            as T

dirs :: IO [FilePath]
dirs = getCurrentDirectory >>= listDirectory >>= filterM isDirectory

gitpull :: FilePath -> IO ()
gitpull f = do
  c <- getCurrentDirectory
  ds <- listDirectory (c ++ "/" ++ f)
  let isGitRepo = ".git" `elem` ds
  when isGitRepo $
    with (T.pushd (toDir c `Path.append` toDir f)) runUpdate `catch` (\e -> print (e :: T.ShellFailed))

runUpdate :: () -> IO ()
runUpdate _ = do
  c <- T.pwd
  putStr ">> " >> print c
  T.shells "git stash" T.stdin
  T.shells "git pull" T.stdin

toDir :: FilePath -> Path.FilePath
toDir = Path.fromText . Text.pack

isDirectory :: FilePath -> IO Bool
isDirectory [] = return False
isDirectory f = do
  a <- doesDirectoryExist f
  b <- pathIsSymbolicLink f
  return (a && b == False && (head f /= '.'))

run :: IO ()
run = do
  ds <- dirs
  mapM_ gitpull ds
