{-# LANGUAGE OverloadedStrings #-}
{-| A generator for React Native components.
 -| Travis Poulsen - 2016
-}
module Main where

import           ComponentGenerator
import           Config
import           Options.Applicative (execParser)
import           Filesystem.Path.CurrentOS (fromText)
import           Parser
import           Templates
import           Turtle.Prelude
import           Types

main :: IO ()
main = do
  command <- execParser opts
  case command of
    Init -> do
      echo "Writing config file..."
      writeTextFile (fromText . filename $ configTemplate) (contents configTemplate)
    Generate settings -> do
      configFile <- readConfig
      generateDesiredTemplates $ mergeConfig configFile settings
  echo "Done"


