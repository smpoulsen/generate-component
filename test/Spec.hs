{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Main where

import           Control.Lens              hiding (pre, re, (<.>))
import           Data.List
import           Data.Maybe
import           Data.Text                 (length)
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
  , QC.testProperty "placeholder text is replaced with the component name" prop_generatedComponentHasComponentName
  ]

runMakesFileProp :: IO ()
runMakesFileProp = quickCheck prop_makesFiles

prop_makesFiles :: Settings -> Property
prop_makesFiles settings@(Settings componentName componentPath _container _native) = monadicIO $ do
  let componentNamePath = fromText componentName
  let tmpDir = "/tmp" </> (settings ^. sComponentDir)
  let componentDir = tmpDir </> componentNamePath

  pre (Data.Text.length componentName > 1 && valid componentPath && valid componentNamePath)
  run $ makeFiles settings

  dirExists <- testdir componentDir
  filesExist <- mapM testfile $ (componentDir </>) <$> [componentNamePath <.> "js", "index.js"]

  assert dirExists
  assert $ and filesExist

prop_generatedComponentHasComponentName :: Settings -> Property
prop_generatedComponentHasComponentName settings@(Settings componentName componentPath _container _native) = monadicIO $ do
  let componentNamePath = fromText componentName
  let tmpDir = "/tmp" </> (settings ^. sComponentDir)
  let componentDir = tmpDir </> componentNamePath

  pre (Data.Text.length componentName > 1 && valid componentPath && valid componentNamePath)
  run $ makeFiles settings

  component <- run $ readTextFile $ componentDir </> componentNamePath <.> "js"
  let stillHasPlaceholderText = component =~ [re|COMPONENT|]

  assert $ stillHasPlaceholderText == False

-- Setup/make files
makeFiles :: Settings -> IO ()
makeFiles settings = do
  let tmpDir = "/tmp" </> (settings ^. sComponentDir)
  let tmpSettings = sComponentDir .~  tmpDir $ settings

  generateDesiredTemplates tmpSettings
