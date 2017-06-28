{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Templates.Config where

import           Control.Lens                  ((^.))
import           Text.InterpolatedString.Perl6 (qq)
import           Types

configTemplate :: Config -> Template
configTemplate c = Template ".generate-component.yaml" config
  where pType = c ^. projectType
        cType = c ^. componentType
        config = [qq|# Type of the current project; determines what files will be
# generated for a component.
# Valid values: React | ReactNative
projectType: $pType

# Default directory in which to generate components.
defaultDirectory: app/components

# Style of components to generate
# Valid values: CreateClass | ES6Class | Functional
componentType: $cType
|]
