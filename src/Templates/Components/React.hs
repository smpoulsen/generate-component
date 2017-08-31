{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module Templates.Components.React where

import           Data.Text                     (Text)
import           Templates.Components
import           Text.InterpolatedString.Perl6 (qc)
import           Types
import           Types.PropTypes

reactComponentTemplate :: ComponentType -> Maybe [Prop] -> Template
reactComponentTemplate cType propTypes =
  case cType of
    Functional  -> functionalReactComponent propTypes
    ES6Class    -> es6ReactComponent propTypes
    CreateClass -> createClassReactComponent propTypes
    Reason      -> reasonComponent propTypes

functionalReactComponent :: Maybe [Prop] -> Template
functionalReactComponent p = Template "COMPONENT.js" [qc|// @flow
/*
   NOTE: This file was auto-generated for a component
   named "COMPONENT"; it is intended to be modified as
   needed to be useful.
*/

import React from 'react';
import PropTypes from 'prop-types';

const COMPONENT = (\{{propNames p}}) => (
  <div>
  </div>
);

COMPONENT.propTypes = \{
  {stringifyPropTypes 2 p}
};

export default COMPONENT;
|]

es6ReactComponent :: Maybe [Prop] -> Template
es6ReactComponent p = Template "COMPONENT.js" [qc|// @flow
/*
   NOTE: This file was auto-generated for a component
   named "COMPONENT"; it is intended to be modified as
   needed to be useful.
*/

import React, \{Component} from 'react';
import PropTypes from 'prop-types';

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

createClassReactComponent :: Maybe [Prop] -> Template
createClassReactComponent p = Template "COMPONENT.js" [qc|// @flow
/*
   NOTE: This file was auto-generated for a component
   named "COMPONENT"; it is intended to be modified as
   needed to be useful.
*/

import React from 'react';
import PropTypes from 'prop-types';
import createReactClass from 'create-react-class';

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

reasonComponent :: Maybe [Prop] -> Template
reasonComponent p = Template "COMPONENT.re" [qc|/* COMPONENT.re */
let component = ReasonReact.statelessComponent "COMPONENT";

let se = ReasonReact.stringToElement;

let make {reasonProps p} _children => \{
  ...component,
  render: fun _self =>
    <div> </div>
};

/* This wrapper allows you to call React.createElement from JS and pass in props */
let comp =
  ReasonReact.wrapReasonForJs
    ::component
    (fun jsProps => make {reasonJSProps p} {ocamlList});
|]
  where ocamlList = "[||]" :: Text -- Embedding directly killed the quasiquoter
