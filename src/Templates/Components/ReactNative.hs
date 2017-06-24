{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Templates.Components.ReactNative where

import           Templates.Components
import           Text.InterpolatedString.Perl6 (qc)
import           Types
import           Types.PropTypes

nativeComponentTemplate :: ComponentType -> Maybe [Prop] -> Template
nativeComponentTemplate cType propTypes =
  case cType of
    Functional  -> functionalNativeComponent propTypes
    ES6Class    -> es6NativeComponent propTypes
    CreateClass -> createClassNativeComponent propTypes

functionalNativeComponent :: Maybe [Prop] -> Template
functionalNativeComponent p = Template "COMPONENT.js" [qc|// @flow
/*
   NOTE: This file was auto-generated for a component
   named "COMPONENT"; it is intended to be modified as
   needed to be useful.
*/

import React from 'react';
import PropTypes from 'prop-types';
import \{View} from 'react-native';

import styles from './styles';

const COMPONENT = (\{{propNames p}}) => (
  <View>
  </View>
);

COMPONENT.propTypes = \{
  {stringifyPropTypes 2 p}
};

export default COMPONENT;
|]

es6NativeComponent :: Maybe [Prop] -> Template
es6NativeComponent p = Template "COMPONENT.js" [qc|// @flow
/*
   NOTE: This file was auto-generated for a component
   named "COMPONENT"; it is intended to be modified as
   needed to be useful.
*/

import React from 'react';
import PropTypes from 'prop-types';
import \{View} from 'react-native';

import styles from './styles';

class COMPONENT extends Component \{
  static propTypes = \{
    {stringifyPropTypes 4 p}
  };

  render() \{
    return (
      <View>
      </View>
    );
  }
}

export default COMPONENT;
|]

createClassNativeComponent :: Maybe [Prop] -> Template
createClassNativeComponent p = Template "COMPONENT.js" [qc|// @flow
/*
   NOTE: This file was auto-generated for a component
   named "COMPONENT"; it is intended to be modified as
   needed to be useful.
*/

import React from 'react';
import PropTypes from 'prop-types';
import createReactClass from 'create-react-class';
import \{View} from 'react-native';

const COMPONENT = createReactClass(\{
  propTypes: \{
    {stringifyPropTypes 4 p}
  };

  render() \{
    return (
      <View>
      </View>
    );
  }
});

export default COMPONENT;
|]
