{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module Styles where

import Data.Text (Text)
import Data.String.QQ

stylesTemplate :: Text
stylesTemplate = [s|
// @flow
/*
  NOTE: This file was auto-generated for a component
  named "COMPONENT"; it is intended to be modified as
  needed to be useful.
*/

import {StyleSheet} from 'react-native';

const styles = StyleSheet.create({
});

export default styles;
|]
