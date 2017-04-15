{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Templates.Components.ReactNative where

import           Text.InterpolatedString.Perl6 (q, qc)
import           Types

nativeComponentTemplate :: ComponentType -> Template
nativeComponentTemplate cType =
  case cType of
    Functional  -> functionalNativeComponent
    ES6Class    -> es6NativeComponent
    CreateClass -> createClassNativeComponent

functionalNativeComponent :: Template
functionalNativeComponent = Template "COMPONENT.js" [q|
// @flow
/*
   NOTE: This file was auto-generated for a component
   named "COMPONENT"; it is intended to be modified as
   needed to be useful.
*/

import React from 'react';
import PropTypes from 'prop-types';
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

es6NativeComponent :: Template
es6NativeComponent = Template "COMPONENT.js" [q|
// @flow
/*
   NOTE: This file was auto-generated for a component
   named "COMPONENT"; it is intended to be modified as
   needed to be useful.
*/

import React from 'react';
import PropTypes from 'prop-types';
import {View} from 'react-native';

import styles from './styles';

class COMPONENT extends Component {
  static propTypes = {
  };

  render() {
    return (
      <View>
      </View>
    );
  }
}

export default COMPONENT;
|]

createClassNativeComponent :: Template
createClassNativeComponent = Template "COMPONENT.js" [q|
// @flow
/*
   NOTE: This file was auto-generated for a component
   named "COMPONENT"; it is intended to be modified as
   needed to be useful.
*/

import React, {Component} from 'react';
import PropTypes from 'prop-types';
import createReactClass from 'create-react-class';
import {View} from 'react-native';

const COMPONENT = createReactClass({
  propTypes: {
  };

  render() {
    return (
      <View>
      </View>
    );
  }
});

export default COMPONENT;
|]
