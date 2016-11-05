{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Index where

import Data.Text (Text)
import Data.String.QQ

indexTemplate :: Text
indexTemplate = [s|
import COMPONENT from './COMPONENT';

export default COMPONENT;
|]
