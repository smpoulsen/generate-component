{-# LANGUAGE OverloadedStrings #-}

module Parser where

import           Data.Monoid               ((<>))
import           Data.Text                 (pack, split, words)
import           Filesystem.Path.CurrentOS (fromText)
import           Options.Applicative
import           Prelude                   hiding (words)
import           Types

{--| Command line argument parser --}
opts :: ParserInfo Command
opts = info (commandParser <**> helper)
  ( fullDesc
  <> progDesc "Generate React/React-Native components"
  <> header "Flexible generator for React/React-Native components" )

commandParser :: Parser Command
commandParser = subparser $
     command "init" (info (initParser <**> helper) $ progDesc "Create a config file")
  <> command "gen" (info (settingsParser <**> helper) $ progDesc "Generate a component")

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
      <*> parseComponentType
      <*> parsePropTypes

parseComponentDirectory :: Parser OSFilePath
parseComponentDirectory =
  fmap (fromText . pack) (strOption
  (  long "component-directory"
  <> short 'd'
  <> metavar "DIR"
  <> help "Directory in which to add the component. Relative to the project root." ))

parseComponentType :: Parser (Maybe ComponentType)
parseComponentType =
  optional $ option auto
  ( long "component-type"
  <> short 't'
  <> help "The type of component to generate" )

parsePropTypes :: Parser (Maybe [PropType])
parsePropTypes =
  optional $ option parsePropTypesReader
  ( long "proptypes"
  <> short 'p'
  <> help "Component props and types (enclosed in quotes) - e.g. -p \"id:number name:string\"" )

parsePropTypesReader :: ReadM [PropType]
parsePropTypesReader = eitherReader $ \s ->
  pure $ toPropType $ split (== ':') <$> (words . pack $ s)
  where toPropType = fmap (\x -> PropType (Prelude.head x) (Prelude.last x))
