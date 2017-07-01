{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Templates.Styles where

import           Text.InterpolatedString.Perl6 (q)
import           Types

stylesTemplate :: Template
stylesTemplate = Template "styles.js" [q|// @flow
/*
   NOTE: This file was auto-generated for a component
   named "COMPONENT"; it should be modified as needed.
*/

import {StyleSheet} from 'react-native';

const styles = StyleSheet.create({
});

export default styles;
|]
