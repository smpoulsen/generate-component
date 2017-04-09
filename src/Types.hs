{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import           Control.Lens
import           Data.Aeson                (decode, withObject)
import           Data.Char                 (chr)
import           Data.Text
import           Data.Yaml                 (FromJSON, ToJSON, parseJSON, (.:))
import           Filesystem.Path.CurrentOS (FilePath, fromText, valid)
import           GHC.Generics
import           Test.QuickCheck           (Gen, choose, listOf1, oneof,
                                            suchThat)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

type OSFilePath = Filesystem.Path.CurrentOS.FilePath

data ProjectType = React | ReactNative deriving (Generic, Show)
instance ToJSON ProjectType
instance FromJSON ProjectType

data ComponentType = ES6Class | CreateClass | Functional deriving (Generic, Show)
instance ToJSON ComponentType
instance FromJSON ComponentType

data Config = Config
  { _projectType      :: ProjectType
  , _componentType    :: ComponentType
  , _defaultDirectory :: Text
  } deriving (Generic, Show)
makeLenses ''Config
instance FromJSON Config where
  parseJSON = withObject "Config" $ \v -> Config
    <$> v .: "projectType"
    <*> v .: "componentType"
    <*> v .: "defaultDirectory"


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
