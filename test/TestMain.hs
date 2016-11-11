{-# LANGUAGE OverloadedStrings #-}

module TestMain where

import           Control.Lens              hiding (pre, (<.>))
import           Data.Text                 (length)
import           Filesystem.Path.CurrentOS (fromText, valid, (</>), (<.>))
import           Prelude                   hiding (length)
import           Test.QuickCheck
import           Test.QuickCheck.Monadic
import           Turtle.Prelude            (testdir, testfile)

import           Main

runMakesFileProp :: IO ()
runMakesFileProp = quickCheck prop_makesFiles

prop_makesFiles :: Settings -> Property
prop_makesFiles settings@(Settings componentName componentPath _container _native) = monadicIO $ do
  let componentNamePath = fromText componentName
  let tmpDir = "/tmp" </> (settings ^. sComponentDir)
  let tmpSettings = sComponentDir .~  tmpDir $ settings
  let componentDir = tmpDir </> componentNamePath

  pre (length componentName > 1 && valid componentPath && valid componentNamePath)
  run $ generateDesiredTemplates tmpSettings

  dirExists <- testdir componentDir
  filesExist <- mapM testfile $ (componentDir </>) <$> [componentNamePath <.> "js", "index.js"]

  assert dirExists
  assert $ and filesExist
