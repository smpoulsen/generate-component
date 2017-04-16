module Templates where

import           Templates.Components
import           Templates.Components.React
import           Templates.Components.ReactNative
import           Templates.Containers
import           Templates.Styles
import           Types

templatesToGenerate :: ProjectType -> ComponentType -> Maybe [PropType] -> Bool -> [Template]
templatesToGenerate p c propTypes container =
  if container
    then containerTemplate : containerIndexTemplate : componentTemplates
    else indexTemplate : componentTemplates
  where componentTemplates = pickComponentTemplates p c propTypes

pickComponentTemplates :: ProjectType -> ComponentType -> Maybe [PropType] -> [Template]
pickComponentTemplates p c propTypes =
  case p of
    ReactNative -> nativeTemplates c propTypes
    React       -> reactTemplates c propTypes

nativeTemplates :: ComponentType -> Maybe [PropType] -> [Template]
nativeTemplates c propTypes =
  [nativeComponentTemplate c propTypes, stylesTemplate]

reactTemplates :: ComponentType -> Maybe [PropType] -> [Template]
reactTemplates c propTypes =
  [reactComponentTemplate c propTypes]
