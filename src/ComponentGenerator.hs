{-# LANGUAGE OverloadedStrings #-}
{-| A generator for React Native components.
 -| Travis Poulsen - 2016
-}
module ComponentGenerator where

import           Control.Lens
import           Data.Text                 (replace)
import           Filesystem.Path.CurrentOS (fromText, (</>))
import           Templates
import           Turtle                    (Text)
import           Turtle.Format
import           Turtle.Prelude
import           Types

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
    True  | makeContainer -> containerTemplate : containerIndexTemplate : nativeTemplates
          | otherwise     -> indexTemplate : nativeTemplates
    False | makeContainer -> containerTemplate : containerIndexTemplate : reactTemplates
          | otherwise     -> indexTemplate : reactTemplates
  where makeReactNative = settings ^. sReactNative
        makeContainer   = settings ^. sMakeContainer
        reactTemplates  = [componentTemplate]
        nativeTemplates = [nativeComponentTemplate, stylesTemplate]

{--| Generates the component's path, writes the file, and replaces the placeholder text with the template name. --}
generateComponent :: Settings -> Template -> IO OSFilePath
generateComponent settings template =
  writeTemplateFile (componentPath </> fromText sanitizedFileName) sanitizedTemplate
  where componentPath = (settings ^. sComponentDir) </> fromText componentName
        componentName = settings ^. sComponentName
        sanitizedFileName = replacePlaceholder (filename template)
        sanitizedTemplate = replacePlaceholder (contents template)
        replacePlaceholder = replacePlaceholderText componentName


{--| Writes a file with the template's contents to a file named after the template's name. --}
writeTemplateFile :: OSFilePath -> Text -> IO OSFilePath
writeTemplateFile dest src = do
  echo $ format ("Writing\t"%s%"...") (format fp dest)
  writeTextFile dest src
  return dest

replacePlaceholderText :: Text -> Text -> Text
replacePlaceholderText =
  replace "COMPONENT"
