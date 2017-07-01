{-# LANGUAGE OverloadedStrings #-}
{-| A generator for React and React Native components.
 -| Travis Poulsen - 2016-2017
-}
module Main where

import           ComponentGenerator
import           Config
import           Control.Lens              (over)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, pack)
import           Data.Version              (showVersion)
import           Filesystem.Path.CurrentOS (encodeString, fromText, (</>))
import           Options.Applicative       (execParser)
import           Parser
import           Paths_componentGenerator  (version)
import           Templates.Config
import           Turtle.Prelude
import           Types

main :: IO ()
main = do
  command <- execParser opts
  case command of
    Init config -> initializeWithConfigFile config
    Version -> putStrLn ("generate-component v" <> showVersion version)
    Generate settings -> do
      configFile <- readConfig
      appRoot <- projectRoot
      generateDesiredTemplates $ over sComponentDir (fmap (appRoot </>)) $ mergeConfig configFile settings
  echo "Done"

filePathToText :: OSFilePath -> Text
filePathToText = pack . encodeString

initializeWithConfigFile :: InitConfig -> IO ()
initializeWithConfigFile config = do
  defaultConfigTemplate <- configTemplate <$> mergeDefaultConfg config
  appRoot <- pwd
  let configPath = appRoot </> (fromText . filename $ defaultConfigTemplate)
  configFileExists <- testfile configPath
  if configFileExists
  then echo $ filePathToText configPath <> " already exists; exiting without action."
  else do
    echo "Writing config file:"
    echo $ contents defaultConfigTemplate
    writeTextFile configPath  (contents defaultConfigTemplate)
    echo $ "Config generated at " <> filePathToText configPath
