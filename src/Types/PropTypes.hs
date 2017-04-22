{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.PropTypes where

import           Data.Aeson                (FromJSON, ToJSON)
import           Data.Char                 (toLower)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, intercalate, pack, unwords)
import           GHC.Generics
import           Prelude                   hiding (unwords)
import           Test.QuickCheck           (Gen, choose)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

{-
Atomic types

  t ::=
      PropTypes.any,
      PropTypes.array,
      PropTypes.bool,
      PropTypes.func,
      PropTypes.number,
      PropTypes.object,
      PropTypes.string,
      PropTypes.symbol,
      PropTypes.node,
      PropTypes.element,
      PropTypes.oneOfType([t]),
      PropTypes.arrayOf(t),
      PropTypes.objectOf(t),
      PropTypes.oneOf([Text]),
      PropTypes.instanceOf([Text]),
      PropTypes.shape({key: value}),
-}
data PropType =
    Any
  | Array
  | Bool
  | Func
  | Number
  | Object
  | String
  | Symbol
  | Node
  | Element
  | OneOfType [PropType]
  | ArrayOf PropType
  | ObjectOf PropType
  | OneOf [Text]
  | InstanceOf Text
  | Shape [(Text, PropType)]
  deriving (Generic, Read, Show, Eq, Ord)
instance FromJSON PropType
instance ToJSON PropType

toLowerCamelCase :: String -> String
toLowerCamelCase (h:t) = toLower h : t
toLowerCamelCase []    = ""

data IsRequired = Required | Optional
  deriving (Generic, Read, Show, Eq, Ord)

propTypeDisplay :: PropType -> Text
propTypeDisplay p =
  case p of
    OneOfType t ->
      "PropTypes.oneOfType([" <> intercalate ", " (propTypeDisplay <$> t) <> "])"
    ArrayOf t ->
      "PropTypes.arrayOf(" <> propTypeDisplay t <> ")"
    ObjectOf t ->
      "PropTypes.objectOf(" <> propTypeDisplay t <> ")"
    OneOf t ->
      "PropTypes.oneOf(" <> pack (show t) <> ")"
    InstanceOf s ->
      "PropTypes.instanceOf(" <> pack (show s) <> ")"
    Shape t ->
      "PropTypes.shape({" <> unwords (formatKeyValue <$> t) <> "})"
    _ ->
      "PropTypes." <> (pack . toLowerCamelCase . show $ p)
  where formatKeyValue (k, v) =
          k <> ": " <> propTypeDisplay v <> ","

instance Arbitrary PropType where
  arbitrary = do
    n <- choose (0, 9) :: Gen Int
    return $ case n of
      0 -> Any
      1 -> Array
      2 -> Bool
      3 -> Func
      4 -> Number
      5 -> Object
      6 -> String
      7 -> Symbol
      8 -> Node
      9 -> Element
      _ -> String
