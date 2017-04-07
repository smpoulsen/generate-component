{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Data.Monoid               ((<>))
import           Data.Text                 (pack)
import           Filesystem.Path.CurrentOS (fromText)
import           Options.Applicative
import           Types

{--| Command line argument parser --}
parser :: Parser Settings
parser = Settings <$>
      fmap pack (Options.Applicative.argument str (metavar "NAME"))
      <*> fmap (fromText . pack) (strOption
        ( long "component-directory"
       <> short 'd'
       <> metavar "DIR"
       <> value "./app/components"
       <> help "Directory to add the component" ))
      <*> switch
        ( long "make-container"
       <> short 'c'
       <> help "Create a container component" )
      <*> switch
        ( long "react-native"
       <> short 'n'
       <> help "Create a React Native component" )
