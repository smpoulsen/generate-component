{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Control.Lens              hiding (pre, re, (<.>))
import           Data.List
import           Data.Maybe
import           Data.Text                 (length, Text)
import           Filesystem.Path.CurrentOS (fromText, valid, (<.>), (</>))
import           Prelude                   hiding (length)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck     as QC
import           Text.Regex.PCRE.Heavy
import           Turtle.Prelude            (testdir, testfile, readTextFile)

import           ComponentGenerator
import           Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "files are created" prop_makesFiles
  , QC.testProperty "placeholder text is replaced with the component name" prop_replacePlaceholderText
  ]

prop_makesFiles :: Settings -> Property
prop_makesFiles settings@(Settings componentName componentPath _container native) = monadicIO $ do
  let componentNamePath = fromText componentName
  let tmpDir = "/tmp" </> (settings ^. sComponentDir)
  let componentDir = tmpDir </> componentNamePath

  pre $ componentSettingsValid componentName componentPath
  run $ makeFiles settings

  dirExists <- testdir componentDir
  filesExist <- if native
  then mapM testfile $ (componentDir </>) <$> [componentNamePath <.> "js", "index.js", "styles.js"]
  else mapM testfile $ (componentDir </>) <$> [componentNamePath <.> "js", "index.js"]

  assert dirExists
  assert $ and filesExist

prop_replacePlaceholderText :: Settings -> Property
prop_replacePlaceholderText settings@(Settings componentName componentPath _container _native) = monadicIO $ do
  let componentNamePath = fromText componentName
  let tmpDir = "/tmp" </> (settings ^. sComponentDir)
  let componentDir = tmpDir </> componentNamePath

  pre $ componentSettingsValid componentName componentPath
  run $ makeFiles settings

  component <- run $ readTextFile $ componentDir </> componentNamePath <.> "js"
  index <- run $ readTextFile $ componentDir </> "index.js"

  let placeholderTextReplaced = (=~ [re|COMPONENT|]) <$> [component, index]

  assert $ and $ not <$> placeholderTextReplaced

-- Setup/make files
makeFiles :: Settings -> IO ()
makeFiles settings = do
  let tmpDir = "/tmp" </> (settings ^. sComponentDir)
  let tmpSettings = sComponentDir .~  tmpDir $ settings

  generateDesiredTemplates tmpSettings

componentSettingsValid :: Text -> OSFilePath -> Bool
componentSettingsValid componentName componentDirPath =
  Data.Text.length componentName > 1 && valid componentDirPath && (valid $ fromText componentName)
