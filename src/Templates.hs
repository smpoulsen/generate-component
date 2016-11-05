{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Templates where

import           Data.String.QQ
import           Data.Text      (Text)

componentTemplate :: Text
componentTemplate = [s|
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

COMPONENT.PropTypes = {
};

export default COMPONENT;
|]

containerTemplate :: Text
containerTemplate = [s|
// @flow
/*
   NOTE: This file was auto-generated for a component
   named "COMPONENT"; it is intended to be modified as
   needed to be useful.
*/

import {connect} from 'react-redux';

import COMPONENT from './COMPONENT';

const mapStateToProps = (state: Object) => {
  return {
  };
};

const mapDispatchToProps = (dispatch) => {
  return {
    fn: () => {
      dispatch();
    },
  }
};

const COMPONENTContainer = connect(
  mapStateToProps,
  mapDispatchToProps,
)(COMPONENT);

export default COMPONENTContainer;
|]

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

indexTemplate :: Text
indexTemplate = [s|
import COMPONENT from './COMPONENT';

export default COMPONENT;
|]
