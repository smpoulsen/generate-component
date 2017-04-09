{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Filesystem.Path.CurrentOS (parent, (</>))
import           Turtle.Prelude
import           Types

{- Find the root of a project in order to find the config
   file, if it exists.
-}
projectRoot :: IO OSFilePath
projectRoot =
  findProjectRoot pwd

{-| A React project must have a node_modules directory. |-}
findProjectRoot :: IO OSFilePath -> IO OSFilePath
findProjectRoot dir = do
  isRoot <- hasNodeModules =<< dir
  if isRoot
  then dir
  else findProjectRoot $ recurseUp =<< dir

recurseUp :: OSFilePath -> IO OSFilePath
recurseUp dir =
  return $ parent dir

hasNodeModules :: OSFilePath -> IO Bool
hasNodeModules dir =
  testdir $ dir </> "node_modules"
