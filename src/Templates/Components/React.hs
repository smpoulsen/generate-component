{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Templates.Components.React where

import           Templates.Components
import           Text.InterpolatedString.Perl6 (q, qc)
import           Types

reactComponentTemplate :: ComponentType -> Maybe [PropType] -> Template
reactComponentTemplate cType propTypes =
  case cType of
    Functional  -> functionalReactComponent propTypes
    ES6Class    -> es6ReactComponent propTypes
    CreateClass -> createClassReactComponent propTypes

functionalReactComponent :: Maybe [PropType] -> Template
functionalReactComponent p = Template "COMPONENT.js" [qc|
// @flow
/*
   NOTE: This file was auto-generated for a component
   named "COMPONENT"; it is intended to be modified as
   needed to be useful.
*/

import React from 'react';
import PropTypes from 'prop-types';
import \{render} from 'react-dom';

const COMPONENT = (\{}) => (
  <div>
  </div>
);

COMPONENT.propTypes = \{
  {stringifyPropTypes 2 p}
};

export default COMPONENT;
|]

es6ReactComponent :: Maybe [PropType] -> Template
es6ReactComponent p = Template "COMPONENT.js" [qc|
// @flow
/*
   NOTE: This file was auto-generated for a component
   named "COMPONENT"; it is intended to be modified as
   needed to be useful.
*/

import React, \{Component} from 'react';
import PropTypes from 'prop-types';
import \{render} from 'react-dom';

class COMPONENT extends Component \{
  static propTypes = \{
    {stringifyPropTypes 4 p}
  };

  render() \{
    return (
      <div>
      </div>
    );
  }
}

export default COMPONENT;
|]

createClassReactComponent :: Maybe [PropType] -> Template
createClassReactComponent p = Template "COMPONENT.js" [qc|
// @flow
/*
   NOTE: This file was auto-generated for a component
   named "COMPONENT"; it is intended to be modified as
   needed to be useful.
*/

import React from 'react';
import PropTypes from 'prop-types';
import createReactClass from 'create-react-class';
import \{render} from 'react-dom';

const COMPONENT = createReactClass(\{
  propTypes: \{
    {stringifyPropTypes 4 p}
  };

  render() \{
    return (
      <div>
      </div>
    );
  }
});

export default COMPONENT;
|]
