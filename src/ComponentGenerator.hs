{-# LANGUAGE OverloadedStrings #-}
{-| A generator for React Native components.
 -| Travis Poulsen - 2016
-}
module ComponentGenerator where

import           Config                    (projectRoot)
import           Control.Lens
import           Data.Text                 (replace)
import           Filesystem.Path.CurrentOS (fromText, (</>))
import           Templates.Components
import           Templates.Styles
import           Templates.Containers
import           Turtle                    (Text)
import           Turtle.Format
import           Turtle.Prelude
import           Types

{--| If the component doesn't already exist, creates component directory and requisite files. --}
generateDesiredTemplates :: Settings -> IO ()
generateDesiredTemplates settings@(Settings componentName componentPath' _container _native) = do
  appRoot <- projectRoot
  let componentPath = appRoot </> componentPath' </> componentNamePath
  let settings' = settings & sComponentDir .~ componentPath
  let componentGenerator = generateComponent settings'
  let runGenerator = mapM_ componentGenerator
  dirExists <- testdir componentPath
  if dirExists
    then echo "Component directory exists; exiting without action."
    else do
      echo $ format ("Making directory at: "%s%"") (format fp componentPath)
      mktree componentPath
      echo "Copying files..."
      runGenerator $ determineTemplatesToGenerate settings'
  where componentNamePath = fromText componentName

{--| Determines which templates to create based on command line arguments. --}
determineTemplatesToGenerate :: Settings -> [Template]
determineTemplatesToGenerate settings =
  case makeReactNative of
    True  | makeContainer -> containerTemplate : containerIndexTemplate : nativeTemplates
          | otherwise     -> indexTemplate : nativeTemplates
    False | makeContainer -> containerTemplate : containerIndexTemplate : reactTemplates
          | otherwise     -> indexTemplate : reactTemplates
  where makeReactNative = _sProjectType settings == ReactNative
        makeContainer   = _sMakeContainer settings
        reactTemplates  = [componentTemplate]
        nativeTemplates = [nativeComponentTemplate, stylesTemplate]

{--| Generates the component's path, writes the file, and replaces the placeholder text with the template name. --}
generateComponent :: Settings -> Template -> IO OSFilePath
generateComponent settings template =
  writeTemplateFile (componentPath </> fromText sanitizedFileName) sanitizedTemplate
  where componentPath = settings ^. sComponentDir
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
