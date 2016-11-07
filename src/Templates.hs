{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Templates where

import           Data.String.QQ
import           Data.Text      (Text)

data Template = Template
  { filename :: Text
  , contents :: Text
  }

componentTemplate :: Template
componentTemplate = Template "COMPONENT.js" [s|
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

COMPONENT.PropTypes = {
};

export default COMPONENT;
|]

nativeComponentTemplate :: Template
nativeComponentTemplate = Template "COMPONENT.js" [s|
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

containerTemplate :: Template
containerTemplate = Template "COMPONENTContainer.js" [s|
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

stylesTemplate :: Template
stylesTemplate = Template "styles.js" [s|
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

indexTemplate :: Template
indexTemplate = Template "index.js" [s|
import COMPONENT from './COMPONENT';

export default COMPONENT;
|]
