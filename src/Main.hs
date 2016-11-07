{-# LANGUAGE OverloadedStrings #-}
{-| A generator for React Native components.
 -| Travis Poulsen - 2016
-}
module Main where

import           Control.Monad.IO.Class    (MonadIO)
import           Data.Monoid               ((<>))
import           Data.Text                 (pack, replace)
import           Filesystem.Path.CurrentOS (FilePath, fromText, (</>))
import           Options.Applicative
import           Templates
import           Turtle                    (Text)
import           Turtle.Format
import           Turtle.Options            (options)
import           Turtle.Prelude

type OSFilePath = Filesystem.Path.CurrentOS.FilePath
data Settings = Settings
  { sComponentName :: Text
  , sComponentDir  :: OSFilePath
  , sMakeContainer :: Bool
  , sReactNative   :: Bool
  } deriving (Eq, Show, Ord)

main :: IO ()
main = do
  settings <- options "Component generator" parser
  generateDesiredTemplates settings
  echo "Done"

{--| Command line argument parser --}
parser :: Parser Settings
parser = Settings <$>
      fmap pack (argument str (metavar "NAME"))
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

{--| If the component doesn't already exist, creates component directory and requisite files. --}
generateDesiredTemplates :: Settings -> IO ()
generateDesiredTemplates settings@(Settings componentName componentPath' _container _native) = do
  dirExists <- testdir componentPath
  if dirExists
    then echo "Component directory exists; exiting without action."
    else do
      echo $ format ("Making directory at: "%s%"") (format fp componentPath)
      mktree componentPath
      echo "Copying files..."
      runGenerator $ determineTemplatesToGenerate settings
  where componentPath = componentPath' </> componentNamePath
        componentNamePath = fromText componentName
        componentGenerator = generateComponent settings
        runGenerator = mapM_ componentGenerator

{--| Determines which templates to create based on command line arguments. --}
determineTemplatesToGenerate :: Settings -> [Template]
determineTemplatesToGenerate settings =
  case makeReactNative of
    True  | makeContainer -> containerTemplate : nativeTemplates
          | otherwise     -> nativeTemplates
    False | makeContainer -> containerTemplate : reactTemplates
          | otherwise     -> reactTemplates
  where makeReactNative = sReactNative settings
        makeContainer   = sMakeContainer settings
        reactTemplates  = [componentTemplate, indexTemplate]
        nativeTemplates = [nativeComponentTemplate, stylesTemplate, indexTemplate]

{--| Generates the component's path, writes the file, and replaces the placeholder text with the template name. --}
generateComponent :: Settings -> Template -> IO ()
generateComponent settings template =
  writeTemplateFile (componentPath </> fromText sanitizedFileName) (contents template) >>= replacePlaceholder
  where componentPath = sComponentDir settings </> fromText componentName
        componentName = sComponentName settings
        sanitizedFileName = replace "COMPONENT" componentName (filename template)
        replacePlaceholder = replacePlaceholderText componentName


{--| Writes a file with the template's contents to a file named after the template's name. --}
writeTemplateFile :: OSFilePath -> Text -> IO OSFilePath
writeTemplateFile dest src = do
  echo $ format ("Writing\t"%s%"...") (format fp dest)
  writeTextFile dest src
  return dest

{--| Replaces the text "COMPONENT" in the template file with the component name. --}
replacePlaceholderText :: MonadIO io => Text -> OSFilePath -> io()
replacePlaceholderText componentName =
  inplace ("COMPONENT" *> pure componentName)
