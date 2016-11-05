{-# LANGUAGE OverloadedStrings #-}
{-| A generator for React Native components.
 -| Travis Poulsen - 2016
-}
module Main where

import           Component
import           Control.Applicative       (optional)
import           Control.Monad.IO.Class    (MonadIO)
import           Data.Maybe
import           Data.Monoid               ((<>))
import           Filesystem.Path           ((<.>))
import           Filesystem.Path.CurrentOS (FilePath, append, fromText, (</>))
import           Index
import           Styles
import           Turtle                    (Text)
import           Turtle.Format
import           Turtle.Options
import           Turtle.Prelude            hiding (FilePath, append)

type OSFilePath = Filesystem.Path.CurrentOS.FilePath
data Settings = Settings
  { sComponentName :: Text
  , sComponentDir  :: Maybe OSFilePath
  , sMakecontainer :: Bool
  } deriving (Eq, Show, Ord)

main :: IO ()
main = do
  (Settings componentName componentDir container) <- options "Component generator" parser
  case componentDir of
    Just dir -> copyTemplateDir componentName dir container
    Nothing  -> copyTemplateDir componentName "./app/components" container
  echo "Done"

parser :: Parser Settings
parser = Settings <$> argText "COMPONENT_NAME" "Name of the component"
                  <*> optional (optPath "COMPONENT_DIRECTORY" 'd' "Directory to add the component")
                  <*> switch "containerComponent" 'c' "Create a container component"

copyTemplateDir :: Text -> OSFilePath -> Bool -> IO ()
copyTemplateDir componentName componentPath' container = do
  dirExists <- testdir componentPath
  if dirExists
    then echo "Component directory exists; exiting without action."
    else do
      echo $ format ("Making directory at: "%s%"") (format fp componentPath)
      mktree componentPath
      echo "Copying files..."
      currentDir <- pwd
      component <- writeTemplateFile (componentPath </> componentNamePath <.> "js") componentTemplate
      styles    <- writeTemplateFile (componentPath </> "styles.js") stylesTemplate
      index     <- writeTemplateFile (componentPath </> "index.js") indexTemplate
      echo "Replacing placeholder text..."
      if container then do
        container <- writeTemplateFile (componentPath </> fromText (componentName <> "Container") <.> "js") containerTemplate
        replacePlaceholder container
      else
        echo "Working..."
      mapM_ replacePlaceholder [component, styles, index]
  where componentPath = componentPath' </> componentNamePath
        componentNamePath = fromText componentName
        replacePlaceholder = replacePlaceholderText componentName

writeTemplateFile :: OSFilePath -> Text -> IO OSFilePath
writeTemplateFile dest src = do
  echo $ format ("Writing\t"%s%"...") (format fp dest)
  writeTextFile dest src
  return dest

replacePlaceholderText :: MonadIO io => Text -> OSFilePath -> io()
replacePlaceholderText componentName =
  inplace ("COMPONENT" *> pure componentName)
