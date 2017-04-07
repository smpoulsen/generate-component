{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import           Control.Lens
import           Data.Char                 (chr)
import           Data.Text
import           Filesystem.Path.CurrentOS (FilePath, fromText, valid)
import           Test.QuickCheck           (Gen, choose, listOf1, oneof, suchThat)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

type OSFilePath = Filesystem.Path.CurrentOS.FilePath
data Settings = Settings
  { _sComponentName :: Text
  , _sComponentDir  :: OSFilePath
  , _sMakeContainer :: Bool
  , _sReactNative   :: Bool
  } | GenConfig
  deriving (Eq, Show, Ord)
makeLenses ''Settings

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
