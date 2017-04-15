{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Templates.Config where

import           Text.InterpolatedString.Perl6 (q, qc)
import           Types

configTemplate :: Template
configTemplate = Template ".generate-component.yaml" [q|
# Type of the current project; determines what files will be
# generated for a component.
# Valid values: React | ReactNative
projectType: ReactNative

# Default directory in which to generate components.
defaultDirectory: app/components

# Style of components to generate
# Valid values: CreateClass | ES6Class | Functional
componentType: ES6Class
|]
