{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Data.Text                 (pack)
import           Filesystem.Path.CurrentOS (fromText)
import           Options.Applicative
import           Types

{--| Command line argument parser --}
commandParser :: Parser Command
commandParser = subparser $
     command "init" (info initParser $ progDesc "Create a config file")
  <> command "gen" (info settingsParser $ progDesc "Generate a component")

initParser :: Parser Command
initParser = pure Init

settingsParser :: Parser Command
settingsParser = fmap Generate $ Settings <$>
      fmap pack (Options.Applicative.argument str (metavar "NAME"))
      <*> optional parseComponentDirectory
      <*> switch
        ( long "redux-container"
       <> short 'r'
       <> help "Create a redux connected container component" )
      <*> flag React ReactNative
        ( long "react-native"
       <> short 'n'
       <> help "Create a React Native component" )

parseComponentDirectory :: Parser OSFilePath
parseComponentDirectory =
  fmap (fromText . pack) (strOption
  (  long "component-directory"
  <> short 'd'
  <> metavar "DIR"
  <> help "Directory in which to add the component. Relative to the project root." ))

opts :: ParserInfo Command
opts = info (commandParser <**> helper)
  ( fullDesc
  <> progDesc "Generate React/React-Native components"
  <> header "Flexible generator for React/React-Native components" )
