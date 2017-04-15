module Templates where

import           Templates.Components
import           Templates.Components.React
import           Templates.Components.ReactNative
import           Templates.Containers
import           Templates.Styles
import           Types

templatesToGenerate :: ProjectType -> ComponentType -> Bool -> [Template]
templatesToGenerate p c container =
  if container
    then containerTemplate : containerIndexTemplate : componentTemplates
    else indexTemplate : componentTemplates
  where componentTemplates = pickComponentTemplates p c

pickComponentTemplates :: ProjectType -> ComponentType -> [Template]
pickComponentTemplates p c =
  case p of
    ReactNative -> nativeTemplates c
    React       -> reactTemplates c

nativeTemplates :: ComponentType -> [Template]
nativeTemplates c =
  [nativeComponentTemplate c, stylesTemplate]

reactTemplates :: ComponentType -> [Template]
reactTemplates c =
  [reactComponentTemplate c]
