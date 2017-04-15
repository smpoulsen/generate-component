{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens              hiding (pre, (<.>))
import           Data.Maybe                (fromJust)
import           Data.Text                 (length)
import           Filesystem.Path.CurrentOS (fromText, valid, (<.>), (</>))
import           Prelude                   hiding (length)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck     as QC
import           Turtle.Prelude            (testdir, testfile)

import           ComponentGenerator
import           Types

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [properties]

properties :: TestTree
properties = testGroup "Properties" [qcProps]

qcProps :: TestTree
qcProps = testGroup "(checked by QuickCheck)"
  [ QC.testProperty "files are created" prop_makesFiles
  ]

runMakesFileProp :: IO ()
runMakesFileProp = quickCheck prop_makesFiles

prop_makesFiles :: Settings -> Property
prop_makesFiles settings@(Settings componentName (Just componentPath) _container _native) = monadicIO $ do
  let tmpDir = pure $ "/tmp" </> componentPath
  let componentNamePath = fromText componentName
  let tmpSettings = sComponentDir .~  tmpDir $ settings

  let componentDir = fromJust tmpDir </> componentNamePath

  pre (length componentName > 1 && valid componentPath && valid componentNamePath)
  run $ generateDesiredTemplates tmpSettings

  dirExists <- testdir componentDir
  filesExist <- mapM testfile $ (componentDir </>) <$> [componentNamePath <.> "js", "index.js"]

  assert dirExists
  assert $ and filesExist
