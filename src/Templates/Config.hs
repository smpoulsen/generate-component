{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
module Templates.Config where

import           Control.Lens                  ((^.))
import           Filesystem.Path.CurrentOS (toText)
import           Text.InterpolatedString.Perl6 (qq)
import           Types

configTemplate :: Config -> Template
configTemplate config = Template ".generate-component.yaml" template
  where pType = config ^. projectType
        cType = config ^. componentType
        dir   = case toText $ config ^. defaultDirectory of
          Right d -> d
          Left  e -> e
        template = [qq|# Type of the current project; determines what files will be
# generated for a component.
# Valid values: React | ReactNative
projectType: $pType

# Default directory in which to generate components.
defaultDirectory: $dir

# Style of components to generate
# Valid values: CreateClass | ES6Class | Functional
componentType: $cType
|]
