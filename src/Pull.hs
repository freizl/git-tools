{-# LANGUAGE OverloadedStrings #-}

module Pull where

import qualified Data.Text                 as Text
import           System.Directory
import qualified Turtle.Prelude            as T
--import qualified Turtle.Shell         as T
import           Control.Monad             (when)
import           Control.Monad.Managed     (with)
import qualified Filesystem.Path           as Path
import qualified Filesystem.Path.CurrentOS as Path

dirs :: IO [FilePath]
dirs = getCurrentDirectory >>= listDirectory

gitpull :: FilePath -> IO ()
gitpull f = do
  c <- getCurrentDirectory
  ds <- listDirectory (c ++ f)
  let isGitRepo = ".git" `elem` ds
  when isGitRepo $
    with (T.pushd (toDir c `Path.append` toDir f)) runUpdate

runUpdate :: () -> IO ()
runUpdate _ = do
  T.pwd >>= print
  T.shells "git stash" T.stdin
  T.shells "git pull" T.stdin

toDir :: FilePath -> Path.FilePath
toDir = Path.fromText . Text.pack

run :: IO ()
run = do
  ds <- dirs
  mapM_ gitpull ds