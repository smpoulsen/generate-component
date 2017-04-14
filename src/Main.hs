{-# LANGUAGE OverloadedStrings #-}
{-| A generator for React Native components.
 -| Travis Poulsen - 2016
-}
module Main where

import           ComponentGenerator
import           Config
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, pack)
import           Filesystem.Path.CurrentOS (encodeString, fromText, (</>))
import           Options.Applicative       (execParser)
import           Parser
import           Templates
import           Turtle.Prelude
import           Types

main :: IO ()
main = do
  command <- execParser opts
  case command of
    Init -> do
      appRoot <- projectRoot
      let configLocation = appRoot </> (fromText . filename $ configTemplate)
      dirExists <- testfile configLocation
      if dirExists
        then echo $ filePathToText configLocation <> " already exists; exiting without action."
      else do
        echo "Writing config file:"
        echo $ contents configTemplate
        writeTextFile configLocation  (contents configTemplate)
        echo $ "Config generated at " <> filePathToText configLocation
    Generate settings -> do
      configFile <- readConfig
      generateDesiredTemplates $ mergeConfig configFile settings
  echo "Done"

filePathToText :: OSFilePath -> Text
filePathToText = pack . encodeString
