{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Templates.Components where

import           Data.Text                     (Text)
import           Text.InterpolatedString.Perl6 (q, qc)
import           Types

indexTemplate :: Template
indexTemplate = Template "index.js" [q|
import COMPONENT from './COMPONENT';

export default COMPONENT;
|]

componentTemplate :: Template
componentTemplate = Template "COMPONENT.js" [q|
// @flow
/*
   NOTE: This file was auto-generated for a component
   named "COMPONENT"; it is intended to be modified as
   needed to be useful.
*/

import React, {PropTypes} from 'react';
import {render} from 'react-dom';

const COMPONENT = ({}) => (
  <div>
  </div>
);

COMPONENT.propTypes = {
};

export default COMPONENT;
|]

nativeComponentTemplate :: Template
nativeComponentTemplate = Template "COMPONENT.js" [q|
// @flow
/*
   NOTE: This file was auto-generated for a component
   named "COMPONENT"; it is intended to be modified as
   needed to be useful.
*/

import React, {PropTypes} from 'react';
import {View} from 'react-native';

import styles from './styles';

const COMPONENT = ({}) => (
  <View>
  </View>
);

COMPONENT.propTypes = {
};

export default COMPONENT;
|]
