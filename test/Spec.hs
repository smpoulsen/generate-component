{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Lens              hiding (pre, (<.>))
import           Data.Text                 (length)
import           Filesystem.Path.CurrentOS (fromText, valid, (<.>), (</>))
import           Prelude                   hiding (length)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Test.Tasty
import           Test.Tasty.QuickCheck     as QC
import           Turtle.Prelude            (mktree, testdir, testfile)

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
prop_makesFiles settings@(Settings componentName componentPath _container _native) = monadicIO $ do
  let componentNamePath = fromText componentName
  let tmpDir = "/tmp" </> (settings ^. sComponentDir)
  let tmpSettings = sComponentDir .~  tmpDir $ settings
  let componentDir = tmpDir </> componentNamePath
  mktree $ "/tmp" </> "node_modules"

  pre (length componentName > 1 && valid componentPath && valid componentNamePath)
  run $ generateDesiredTemplates tmpSettings

  dirExists <- testdir componentDir
  filesExist <- mapM testfile $ (componentDir </>) <$> [componentNamePath <.> "js", "index.js"]

  assert dirExists
  assert $ and filesExist
