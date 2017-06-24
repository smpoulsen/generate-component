{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Templates.Containers where

import           Text.InterpolatedString.Perl6 (q)
import           Types

containerIndexTemplate :: Template
containerIndexTemplate = Template "index.js" template
  where template = [q|import COMPONENTContainer from './COMPONENTContainer';

export default COMPONENTContainer;
|]

containerTemplate :: Template
containerTemplate = Template "COMPONENTContainer.js" [q|// @flow
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
