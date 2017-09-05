{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Templates.Components where

import           Control.Lens                  ((^.))
import           Data.Monoid                   ((<>))
import           Data.Text                     (Text, intercalate, pack)
import           Text.InterpolatedString.Perl6 (q)
import           Types
import           Types.PropTypes

indexTemplate :: Template
indexTemplate = Template "index.js" [q|
import COMPONENT from './COMPONENT';

export default COMPONENT;
|]

stringifyPropTypes :: Int -> Maybe [Prop] -> Text
stringifyPropTypes nSpaces ts =
  case ts of
    Nothing -> ""
    Just xs -> intercalate (",\n" <> spaces) $ pack . show <$> xs
  where spaces = pack . take nSpaces $ cycle " "

propNames :: Maybe [Prop] -> Text
propNames ts =
  case ts of
    Nothing -> ""
    Just xs -> intercalate ", " $ fmap (^. name) xs

reasonProps :: Maybe [Prop] -> Text
reasonProps ts =
  case ts of
    Nothing -> ""
    Just xs -> intercalate " " $ fmap (\x -> ("::" <>) $ (^. name) x) xs

reasonJSProps :: Maybe [Prop] -> Text
reasonJSProps ts =
  case ts of
    Nothing -> ""
    Just xs -> intercalate " " $ fmap (\x -> (\y -> y <> "::jsProps##" <> y) $ (^. name) x) xs
