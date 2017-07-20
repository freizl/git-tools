{-# LANGUAGE OverloadedStrings #-}

module Lib where

import System.Directory
import qualified Data.Text as Text
import qualified Turtle.Prelude         as T
import qualified Turtle.Shell         as T
import qualified Filesystem.Path as Path
import qualified Filesystem.Path.CurrentOS as Path
import Control.Monad.Managed (with)

dirs :: IO [FilePath]
dirs = getCurrentDirectory >>= listDirectory

gitpull :: FilePath -> IO ()
gitpull f = do
  c <- getCurrentDirectory
  -- TODO: check if it's an git repo.
  with
    (T.pushd (toDir c `Path.append` toDir f))
    (\_ -> T.shells "git stash && pwd && git pull" T.stdin)

toDir :: FilePath -> Path.FilePath
toDir = Path.fromText . Text.pack

runPull :: IO ()
runPull = do
  ds <- dirs
  mapM_ gitpull ds
