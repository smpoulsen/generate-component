{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}
{-| A generator for React Native components.
 -| Travis Poulsen - 2016
-}
module Main where

import           Control.Lens
import           Control.Monad.IO.Class    (MonadIO)
import           Data.Char                 (chr)
import           Data.Monoid               ((<>))
import           Data.Text                 (pack, replace)
import           Filesystem.Path.CurrentOS (FilePath, fromText, valid, (</>))
import           Options.Applicative
import           Templates
import           Test.QuickCheck           (Gen, choose, listOf1, oneof, suchThat)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()
import           Turtle                    (Text)
import           Turtle.Format
import           Turtle.Options            (options)
import           Turtle.Prelude

type OSFilePath = Filesystem.Path.CurrentOS.FilePath
data Settings = Settings
  { _sComponentName :: Text
  , _sComponentDir  :: OSFilePath
  , _sMakeContainer :: Bool
  , _sReactNative   :: Bool
  } deriving (Eq, Show, Ord)
makeLenses ''Settings

main :: IO ()
main = do
  settings <- options "Component generator" parser
  generateDesiredTemplates settings
  echo "Done"

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
  where makeReactNative = settings ^. sReactNative
        makeContainer   = settings ^. sMakeContainer
        reactTemplates  = [componentTemplate, indexTemplate]
        nativeTemplates = [nativeComponentTemplate, stylesTemplate, indexTemplate]

{--| Generates the component's path, writes the file, and replaces the placeholder text with the template name. --}
generateComponent :: Settings -> Template -> IO ()
generateComponent settings template =
  writeTemplateFile (componentPath </> fromText sanitizedFileName) (contents template) >>= replacePlaceholder
  where componentPath = (settings ^. sComponentDir) </> fromText componentName
        componentName = settings ^. sComponentName
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

{--| Testing --}
instance Arbitrary Settings where
  arbitrary = Settings <$>
        genComponentName
    <*> genFilePath
    <*> arbitrary
    <*> arbitrary

{--| Generate a filepath using characters 0-9 and A-z --}
genFilePath :: Gen OSFilePath
genFilePath = arbitraryFilePath `suchThat` valid
  where arbitraryFilePath = fromText <$> genText

genComponentName :: Gen Text
genComponentName = genText `suchThat` (valid . fromText)

genText :: Gen Text
genText = pack <$> listOf1 validChars
  where validChars = chr <$> oneof [genNums, genLowerCase, genUpperCase]
        genNums = choose (48, 57)
        genLowerCase = choose (97, 122)
        genUpperCase = choose (65, 90)
