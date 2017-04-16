{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Templates.Components where

import           Data.Monoid                   ((<>))
import           Data.Text                     (Text, intercalate, pack)
import           Text.InterpolatedString.Perl6 (q)
import           Types

indexTemplate :: Template
indexTemplate = Template "index.js" [q|
import COMPONENT from './COMPONENT';

export default COMPONENT;
|]

stringifyPropTypes :: Int -> Maybe [PropType] -> Text
stringifyPropTypes nSpaces ts =
  case ts of
    Nothing -> ""
    Just x  -> intercalate (",\n" <> spaces) $ pack . show <$> x
  where spaces = pack . take nSpaces $ cycle " "
