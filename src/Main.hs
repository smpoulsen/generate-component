{-# LANGUAGE OverloadedStrings #-}
{-| A generator for React Native components.
 -| Travis Poulsen - 2016
-}
module Main where

import           ComponentGenerator
import           Parser
import           Turtle.Options     (options)
import           Turtle.Prelude

main :: IO ()
main = do
  settings <- options "Component generator" parser
  generateDesiredTemplates settings
  echo "Done"


