{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Data.Text                 (pack)
import           Filesystem.Path.CurrentOS (fromText)
import           Options.Applicative
import           Types

{--| Command line argument parser --}
parser :: Parser Settings
parser = settingsParser <|> configParser

configParser :: Parser Settings
configParser = flag' GenConfig
      ( long "generate-config"
      <> short 'g'
      <> help "Create a config file to specify project defaults; this should be in the project root" )

settingsParser :: Parser Settings
settingsParser = Settings <$>
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
      <*> flag React ReactNative
        ( long "react-native"
       <> short 'n'
       <> help "Create a React Native component" )

opts :: ParserInfo Settings
opts = info (parser <**> helper)
  ( fullDesc
  <> progDesc "Generate React/React-Native components"
  <> header "Flexible generator for React/React-Native components" )
