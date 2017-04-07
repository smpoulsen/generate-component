{-# LANGUAGE OverloadedStrings #-}
{-| A generator for React Native components.
 -| Travis Poulsen - 2016
-}
module Main where

import           ComponentGenerator
import           Options.Applicative (execParser)
import           Parser
import           Turtle.Prelude

main :: IO ()
main = do
  settings <- execParser opts
  generateDesiredTemplates settings
  echo "Done"


