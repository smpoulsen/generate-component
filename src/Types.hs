{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import           Control.Lens              hiding (elements)
import           Data.Aeson                (decode, withObject)
import           Data.Char                 (chr)
import           Data.Text
import           Data.Yaml                 (FromJSON, ToJSON, parseJSON, (.:))
import           Filesystem.Path.CurrentOS (FilePath, fromText, valid)
import           GHC.Generics
import           Test.QuickCheck           (Gen, choose, elements, listOf1,
                                            oneof, suchThat)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()
import Options.Applicative (Parser)

type OSFilePath = Filesystem.Path.CurrentOS.FilePath

data ProjectType = React | ReactNative
  deriving (Generic, Show, Eq, Ord)
instance ToJSON ProjectType
instance FromJSON ProjectType

data ComponentType = ES6Class | CreateClass | Functional
  deriving (Generic, Show, Eq, Ord)
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
  , _sProjectType   :: ProjectType
  }
  deriving (Eq, Show, Ord)
makeLenses ''Settings

type CSettings = Settings

data Command =
    Init
  | Generate CSettings

{--| Testing --}
instance Arbitrary Settings where
  arbitrary = Settings <$>
        genComponentName
    <*> genFilePath
    <*> arbitrary
    <*> arbitrary

instance Arbitrary ProjectType where
  arbitrary = elements [React, ReactNative]

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
