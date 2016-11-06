{-# LANGUAGE OverloadedStrings #-}
{-| A generator for React Native components.
 -| Travis Poulsen - 2016
-}
module Main where

import           Control.Applicative       (optional)
import           Control.Monad.IO.Class    (MonadIO)
import           Data.Monoid               ((<>))
import           Data.Text                 (pack, replace)
import           Filesystem.Path           ((<.>))
import           Filesystem.Path.CurrentOS (FilePath, append, fromText, (</>))
import           Options.Applicative
import           Templates
import           Turtle                    (Text)
import           Turtle.Format
import           Turtle.Options            (options)
import           Turtle.Prelude            hiding (FilePath, append)

type OSFilePath = Filesystem.Path.CurrentOS.FilePath
data Settings = Settings
  { sComponentName :: Text
  , sComponentDir  :: OSFilePath
  , sMakeContainer :: Bool
  } deriving (Eq, Show, Ord)


parser :: Parser Settings
parser = Settings <$>
      fmap pack (argument str (metavar "NAME"))
      <*> fmap (fromText . pack) $ strOption
        ( long "component-directory"
       <> short 'd'
       <> metavar "DIR"
       <> value "./app/components"
       <> help "Directory to add the component"
        )
  <*> switch
        ( long "make-container"
       <> short 'c'
       <> help "Create a container component"
        )

main :: IO ()
main = do
  settings <- options "Component generator" parser
  copyTemplateDir settings
  echo "Done"

copyTemplateDir :: Settings -> IO ()
copyTemplateDir settings@(Settings componentName componentPath' container) = do
  dirExists <- testdir componentPath
  if dirExists
    then echo "Component directory exists; exiting without action."
    else do
      echo $ format ("Making directory at: "%s%"") (format fp componentPath)
      mktree componentPath
      echo "Copying files..."
      mapM_ componentGenerator [componentTemplate, stylesTemplate, indexTemplate]
      if sMakeContainer settings
        then componentGenerator containerTemplate
        else echo "\n"
  where componentPath = componentPath' </> componentNamePath
        componentNamePath = fromText componentName
        componentGenerator = generateComponent settings

generateComponent :: Settings -> Template -> IO ()
generateComponent settings template =
  writeTemplateFile (componentPath </> fromText sanitizedFileName) (contents template) >>= replacePlaceholder
  where componentPath = sComponentDir settings </> fromText componentName
        componentName = sComponentName settings
        sanitizedFileName = replace "COMPONENT" componentName (filename template)
        replacePlaceholder = replacePlaceholderText componentName


writeTemplateFile :: OSFilePath -> Text -> IO OSFilePath
writeTemplateFile dest src = do
  echo $ format ("Writing\t"%s%"...") (format fp dest)
  writeTextFile dest src
  return dest

replacePlaceholderText :: MonadIO io => Text -> OSFilePath -> io()
replacePlaceholderText componentName =
  inplace ("COMPONENT" *> pure componentName)
