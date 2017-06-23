{-# LANGUAGE OverloadedStrings #-}
{-| A generator for React Native components.
 -| Travis Poulsen - 2016
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
    Init -> initializeWithConfigFile
    Version -> putStrLn ("generate-component v" <> showVersion version)
    Generate settings -> do
      configFile <- readConfig
      appRoot <- projectRoot
      generateDesiredTemplates $ over sComponentDir (fmap (appRoot </>)) $ mergeConfig configFile settings
  echo "Done"

filePathToText :: OSFilePath -> Text
filePathToText = pack . encodeString

initializeWithConfigFile :: IO ()
initializeWithConfigFile = do
  appRoot <- pwd
  let configLocation = appRoot </> (fromText . filename $ configTemplate)
  dirExists <- testfile configLocation
  if dirExists
    then echo $ filePathToText configLocation <> " already exists; exiting without action."
  else do
    echo "Writing config file:"
    echo $ contents configTemplate
    writeTextFile configLocation  (contents configTemplate)
    echo $ "Config generated at " <> filePathToText configLocation
