{-# LANGUAGE OverloadedStrings #-}
{-| A generator for React Native components.
 -| Travis Poulsen - 2016
-}
module Main where

import           ComponentGenerator
import           Config
import           Options.Applicative (execParser)
import           Parser
import           Turtle.Prelude

main :: IO ()
main = do
  settings <- execParser opts
  configFile <- readConfig
  generateDesiredTemplates $ mergeConfig configFile settings
  echo "Done"


