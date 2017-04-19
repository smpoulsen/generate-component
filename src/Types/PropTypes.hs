{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Types.PropTypes where

import           Data.Aeson   (FromJSON, ToJSON)
import           Data.Char    (toLower)
import           Data.Monoid  ((<>))
import           Data.Text    (Text, intercalate, pack, unwords)
import           GHC.Generics
import           Prelude      hiding (unwords)

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
data PropType a =
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
  | OneOfType [PropType a]
  | ArrayOf (PropType a)
  | ObjectOf (PropType a)
  | OneOf [Text]
  | InstanceOf Text
  | Shape [(Text, PropType a)]
  deriving (Generic, Read, Show, Eq, Ord)
instance FromJSON (PropType a)
instance ToJSON (PropType a)

toLowerCamelCase :: String -> String
toLowerCamelCase (h:t) = toLower h : t
toLowerCamelCase []    = ""

propTypeDisplay :: PropType a -> Text
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
