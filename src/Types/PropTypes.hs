{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Types.PropTypes where

import           Data.Aeson      (FromJSON)
import           Data.Map.Strict
import           Data.Text       (Text)
import           GHC.Generics

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
  deriving (Generic, Show, Read, Eq, Ord)
instance FromJSON PropType

{-
Compound types:

  t ::=
      PropTypes.oneOfType([t]),
      PropTypes.arrayOf(t),
      PropTypes.objectOf(t),
      PropTypes.oneOf([Text]),
      PropTypes.instanceOf([Text]),
      PropTypes.shape({key: value}),
-}
data CompoundPropType a =
    OneOfType [PropType]
  | ArrayOf PropType
  | ObjectOf PropType
  | OneOf [Text]
  | InstanceOf Text
  | Shape (Map Text PropType)
  deriving (Generic, Show, Read, Eq, Ord)
instance FromJSON (CompoundPropType a)
