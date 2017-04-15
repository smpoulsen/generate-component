{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Templates.Components.React where

import           Text.InterpolatedString.Perl6 (q, qc)
import           Types

reactComponentTemplate :: ComponentType -> Template
reactComponentTemplate cType =
  case cType of
    Functional  -> functionalReactComponent
    ES6Class    -> es6ReactComponent
    CreateClass -> createClassReactComponent

functionalReactComponent :: Template
functionalReactComponent = Template "COMPONENT.js" [q|
// @flow
/*
   NOTE: This file was auto-generated for a component
   named "COMPONENT"; it is intended to be modified as
   needed to be useful.
*/

import React from 'react';
import PropTypes from 'prop-types';
import {render} from 'react-dom';

const COMPONENT = ({}) => (
  <div>
  </div>
);

COMPONENT.propTypes = {
};

export default COMPONENT;
|]

es6ReactComponent :: Template
es6ReactComponent = Template "COMPONENT.js" [q|
// @flow
/*
   NOTE: This file was auto-generated for a component
   named "COMPONENT"; it is intended to be modified as
   needed to be useful.
*/

import React, {Component} from 'react';
import PropTypes from 'prop-types';
import {render} from 'react-dom';

class COMPONENT extends Component {
  static propTypes = {
  };

  render() {
    return (
      <div>
      </div>
    );
  }
}

export default COMPONENT;
|]

createClassReactComponent :: Template
createClassReactComponent = Template "COMPONENT.js" [q|
// @flow
/*
   NOTE: This file was auto-generated for a component
   named "COMPONENT"; it is intended to be modified as
   needed to be useful.
*/

import React, {Component} from 'react';
import PropTypes from 'prop-types';
import createReactClass from 'create-react-class';
import {render} from 'react-dom';

const COMPONENT = createReactClass({
  propTypes: {
  };

  render() {
    return (
      <div>
      </div>
    );
  }
});

export default COMPONENT;
|]
