{-# LANGUAGE OverloadedStrings #-}

module Config where

import           Control.Lens              ((&), (.~), (^.))
import           Data.Yaml                 (ParseException, decodeFileEither)
import           Filesystem.Path.CurrentOS (encodeString, fromText, parent, toText,
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
  isRoot <- hasConfigFile =<< dir
  isSystemRoot <- dir >>= (\d -> return $ d == fromText "/")
  if isSystemRoot
    then return $ fromText "."
    else if isRoot
      then dir
      else findProjectRoot $ recurseUp =<< dir

recurseUp :: OSFilePath -> IO OSFilePath
recurseUp dir =
  return $ parent dir

hasConfigFile :: OSFilePath -> IO Bool
hasConfigFile dir =
  testfile $ dir </> ".generate-component.yaml"

readConfig :: IO Config
readConfig = do
  rootDir <- projectRoot
  let configPath = rootDir </> ".generate-component.yaml" :: OSFilePath
  let configContents = decodeFileEither $ encodeString configPath
  configFromEither =<< configContents

configFromEither :: Either ParseException Config -> IO Config
configFromEither c =
  case c of
    Left _e -> defaultConfig
    Right config -> return config

mergeConfig :: Config -> Settings -> Settings
mergeConfig c s =
  s & sProjectType .~ (c ^. projectType)
    & sComponentDir .~ dir
    & sComponentType .~ cType
  where
    dir = case s ^. sComponentDir of
      Nothing -> Just $ fromText $ c ^. defaultDirectory
      Just d  -> Just d
    cType = case s ^. sComponentType of
      Nothing -> Just $ c ^. componentType
      Just ct -> Just ct

defaultConfig :: IO Config
defaultConfig = do
  d <- pwd
  let dir = toText d
  let buildConfig = Config ReactNative ES6Class
  return $ either buildConfig buildConfig dir
