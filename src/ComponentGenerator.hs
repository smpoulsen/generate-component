{-# LANGUAGE OverloadedStrings #-}
{-| A generator for React Native components.
 -| Travis Poulsen - 2016
-}
module ComponentGenerator where

import           Control.Lens
import           Data.Maybe                (fromJust)
import           Data.Text                 (replace)
import           Filesystem.Path.CurrentOS (fromText, (</>))
import           Templates
import           Turtle                    (Text)
import           Turtle.Format             (format, fp, printf, s, (%))
import           Turtle.Prelude
import           Types

{--| If the component doesn't already exist, creates component directory and requisite files. --}
generateDesiredTemplates :: Settings -> IO ()
generateDesiredTemplates settings@(Settings componentName (Just componentPath') _ _ _ _) = do
  let componentPath = componentPath' </> fromText componentName
  let settings' = settings & sComponentDir .~ Just componentPath
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
generateDesiredTemplates _ = echo "Bad component path..."

{--| Determines which templates to create based on command line arguments. --}
determineTemplatesToGenerate :: Settings -> [Template]
determineTemplatesToGenerate settings =
  templatesToGenerate pType cType propTypes makeContainer
  where pType         = settings ^. sProjectType
        cType         = fromJust $ settings ^. sComponentType
        makeContainer = settings ^. sMakeContainer
        propTypes     = settings ^. sPropTypes


{--| Generates the component's path, writes the file, and replaces the placeholder text with the template name. --}
generateComponent :: Settings -> Template -> IO OSFilePath
generateComponent settings template =
  writeTemplateFile (componentPath </> fromText sanitizedFileName) sanitizedTemplate
  where componentPath = fromJust $ settings ^. sComponentDir
        componentName = settings ^. sComponentName
        sanitizedFileName = replacePlaceholder (filename template)
        sanitizedTemplate = replacePlaceholder (contents template)
        replacePlaceholder = replacePlaceholderText componentName


{--| Writes a file with the template's contents to a file named after the template's name. --}
writeTemplateFile :: OSFilePath -> Text -> IO OSFilePath
writeTemplateFile dest src = do
  printf ("Writing\t"%s) (format fp dest)
  writeTextFile dest src
  echo "...Done!"
  return dest

replacePlaceholderText :: Text -> Text -> Text
replacePlaceholderText =
  replace "COMPONENT"
