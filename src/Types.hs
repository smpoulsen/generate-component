{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types where

import           Control.Lens              hiding (elements)
import           Data.Aeson                (decode, withObject)
import           Data.Char                 (chr)
import           Data.Monoid               ((<>))
import           Data.Text
import           Data.Yaml                 (FromJSON, ToJSON, parseJSON, (.:))
import           Filesystem.Path.CurrentOS (FilePath, fromText, valid)
import           GHC.Generics
import           Options.Applicative       (Parser)
import           Test.QuickCheck           (Gen, choose, elements, listOf1,
                                            oneof, suchThat)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)
import           Test.QuickCheck.Instances ()

data Template = Template
  { filename :: Text
  , contents :: Text
  } deriving (Show)

type OSFilePath = Filesystem.Path.CurrentOS.FilePath

data ProjectType = React | ReactNative
  deriving (Generic, Show, Eq, Ord)
instance ToJSON ProjectType
instance FromJSON ProjectType

data ComponentType = ES6Class | CreateClass | Functional
  deriving (Generic, Read, Show, Eq, Ord)
instance ToJSON ComponentType
instance FromJSON ComponentType

data Prop = Prop
  { _name     :: Text
  , _propType :: Text
  } deriving (Generic, Eq, Ord)
instance Show Prop where
  show (Prop n t) =
    unpack $ n <> ": PropTypes." <> t
makeLenses ''Prop

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
  , _sComponentDir  :: Maybe OSFilePath
  , _sMakeContainer :: Bool
  , _sProjectType   :: ProjectType
  , _sComponentType :: Maybe ComponentType
  , _sPropTypes     :: Maybe [Prop]
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
    <*> fmap Just genFilePath
    <*> arbitrary
    <*> arbitrary
    <*> fmap Just arbitrary
    <*> arbitrary

instance Arbitrary ProjectType where
  arbitrary = elements [React, ReactNative]

instance Arbitrary ComponentType where
  arbitrary = elements [ES6Class, CreateClass, Functional]

instance Arbitrary Prop where
  arbitrary = Prop <$>
        genText
    <*> genText

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
