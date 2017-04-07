{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Templates where

import           Data.Text                     (Text)
import           Text.InterpolatedString.Perl6 (q, qc)

data Template = Template
  { filename :: Text
  , contents :: Text
  } deriving (Show)

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

containerTemplate :: Template
containerTemplate = Template "COMPONENTContainer.js" [q|
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
stylesTemplate = Template "styles.js" [q|
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
indexTemplate = Template "index.js" [q|
import COMPONENT from './COMPONENT';

export default COMPONENT;
|]

containerIndexTemplate :: Template
containerIndexTemplate = Template "index.js" [q|
import COMPONENTContainer from './COMPONENTContainer';

export default COMPONENTContainer;
|]

configTemplate :: Template
configTemplate = Template ".generate-component.yaml" [q|
# Type of the current project; determines what files will be
# generated for a component.
# Valid values: react | react-native
projectType: react-native

# Default directory in which to generate components.
defaultDirectory: app/components

# Style of components to generate
# Valid values: createClass | es6-class | functional
componentType: functional
|]
