{-# LANGUAGE OverloadedStrings #-}

module Pull where

import qualified Data.Text                 as Text
import           System.Directory
import qualified Turtle.Prelude            as T
import           Control.Monad             (when)
import           Control.Monad.Managed     (with)
import qualified Filesystem.Path           as Path
import qualified Filesystem.Path.CurrentOS as Path
import Control.Exception

dirs :: IO [FilePath]
dirs = fmap (filter (not . isHiddenDir)) (getCurrentDirectory >>= listDirectory)

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

isHiddenDir :: FilePath -> Bool
isHiddenDir [] = False
isHiddenDir (x:_) = x == '.'

run :: IO ()
run = do
  ds <- dirs
  mapM_ gitpull ds
