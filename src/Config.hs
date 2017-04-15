{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Control.Lens              ((&), (.~), (^.))
import           Data.Yaml                 (ParseException, decodeFileEither)
import           Filesystem.Path.CurrentOS (encodeString, fromText, parent,
                                            (</>))
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

readConfig :: IO (Either ParseException Config)
readConfig = do
  rootDir <- projectRoot
  let configPath = rootDir </> ".generate-component.yaml" :: OSFilePath
  decodeFileEither $ encodeString configPath

mergeConfig :: Either ParseException Config -> Settings -> Settings
mergeConfig (Right c) s =
  s & sProjectType .~ (c ^. projectType)
    & sComponentDir .~ dir
  where
    dir = case s ^. sComponentDir of
      Nothing -> Just $ fromText $ c ^. defaultDirectory
      Just x  -> Just x
mergeConfig _ s = s
