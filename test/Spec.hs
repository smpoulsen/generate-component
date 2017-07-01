{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Control.Lens              hiding (pre, re, (<.>))
import           Data.List
import           Data.Maybe                (fromJust)
import           Data.Text                 (Text, length)
import           Filesystem.Path.CurrentOS (fromText, valid, (<.>), (</>))
import           Prelude                   hiding (length)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic   as QCM
import           Test.Tasty
import           Test.Tasty.HUnit
import           Test.Tasty.QuickCheck     as QC
import           Text.Regex.PCRE.Heavy
import           Turtle.Prelude            (readTextFile, testdir, testfile)

import           ComponentGenerator
import           ParserSpec
import           Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties, unitTests]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

unitTests :: TestTree
unitTests = testGroup "(HUnit tests)"
  [ testCase "parse instanceOf" testParseInstanceOf
  , testCase "parse arrayOf" testParseArrayOf
  , testCase "parse objectOf" testParseObjectOf
  , testCase "parse oneOfType" testParseOneOfType
  , testCase "parse oneOf" testParseOneOf
  , testCase "parse shape" testParseShape
  ]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "files are created" prop_makesFiles
  , QC.testProperty "placeholder text is replaced with the component name" prop_replacePlaceholderText
  ]

prop_makesFiles :: Settings -> Property
prop_makesFiles settings@(Settings componentName (Just componentPath) _ native _ _) = monadicIO $ do
  let tmpDir = pure $ "/tmp" </> componentPath
  let componentNamePath = fromText componentName
  let tmpSettings = sComponentDir .~  tmpDir $ settings

  let componentDir = fromJust tmpDir </> componentNamePath

  pre $ componentSettingsValid componentName componentPath
  run $ makeFiles settings

  dirExists <- testdir componentDir
  filesExist <- if native == ReactNative
  then mapM testfile $ (componentDir </>) <$> [componentNamePath <.> "js", "index.js", "styles.js"]
  else mapM testfile $ (componentDir </>) <$> [componentNamePath <.> "js", "index.js"]

  QCM.assert dirExists
  QCM.assert $ and filesExist

prop_replacePlaceholderText :: Settings -> Property
prop_replacePlaceholderText settings@(Settings componentName (Just componentPath) _ _ _ _) = monadicIO $ do
  let tmpDir = pure $ "/tmp" </> componentPath
  let componentNamePath = fromText componentName

  let componentDir = fromJust tmpDir </> componentNamePath

  pre $ componentSettingsValid componentName componentPath
  run $ makeFiles settings

  component <- run $ readTextFile $ componentDir </> componentNamePath <.> "js"
  indexFile <- run $ readTextFile $ componentDir </> "index.js"

  let placeholderTextReplaced = (=~ [re|COMPONENT|]) <$> [component, indexFile]

  QCM.assert $ and $ not <$> placeholderTextReplaced

-- Setup/make files
makeFiles :: Settings -> IO ()
makeFiles settings = do
  let tmpDir = fmap ("/tmp" </>) (settings ^. sComponentDir)
  let tmpSettings = sComponentDir .~  tmpDir $ settings

  generateDesiredTemplates tmpSettings

componentSettingsValid :: Text -> OSFilePath -> Bool
componentSettingsValid componentName componentDirPath =
  Data.Text.length componentName > 1 && valid componentDirPath && (valid $ fromText componentName)
