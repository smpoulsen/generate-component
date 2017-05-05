{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types.PropTypes where

import           Control.Lens              hiding (elements)
import           Data.Char                 (toLower)
import           Data.Monoid               ((<>))
import           Data.Text                 (Text, intercalate, pack, unpack, unwords)
import           GHC.Generics
import           Prelude                   hiding (unwords)
import           Test.QuickCheck           (Gen, choose)
import           Test.QuickCheck.Arbitrary (Arbitrary, arbitrary)

data IsRequired = Required | Optional
  deriving (Generic, Read, Show, Eq, Ord)

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
  | Shape [Prop]
  deriving (Generic, Show, Eq, Ord)

{- Stringifying proptypes for display in templates. -}
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
  where formatKeyValue t =
          pack $ show t <> ","

toLowerCamelCase :: String -> String
toLowerCamelCase (h:t) = toLower h : t
toLowerCamelCase []    = ""

data Prop = Prop
  { _name     :: Text
  , _propType :: PropType
  , _required :: IsRequired
  } deriving (Generic, Eq, Ord)
instance Show Prop where
  show (Prop n t r) =
    case r of
      Optional ->
        unpack $ n <> ": " <> propTypeDisplay t
      Required ->
        unpack $ n <> ": " <> propTypeDisplay t <> ".isRequired"

makeLenses ''Prop


{- Testing -}
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

instance Arbitrary IsRequired where
  arbitrary = do
    n <- choose (0, 1) :: Gen Int
    return $ case n of
      0 -> Optional
      1 -> Required
      _ -> Optional
