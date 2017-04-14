{-# LANGUAGE OverloadedStrings #-}
{-| A generator for React Native components.
 -| Travis Poulsen - 2016
-}
module Main where

import           ComponentGenerator
import           Config
import           Data.Monoid               ((<>))
import           Data.Text                 (pack)
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
      echo "Writing config file:"
      echo $ contents configTemplate
      writeTextFile configLocation  (contents configTemplate)
      echo $ "Config generated at " <> (pack . encodeString $ configLocation)
    Generate settings -> do
      configFile <- readConfig
      generateDesiredTemplates $ mergeConfig configFile settings
  echo "Done"


