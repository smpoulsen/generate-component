module Templates where

import           Templates.Components
import           Templates.Components.React
import           Templates.Components.ReactNative
import           Templates.Containers
import           Templates.Styles
import           Types
import           Types.PropTypes

templatesToGenerate :: ProjectType -> ComponentType -> Maybe [Prop] -> Bool -> [Template]
templatesToGenerate p c propTypes container =
  if container
    then containerTemplate : containerIndexTemplate : componentTemplates
    else if c == Reason then componentTemplates else indexTemplate : componentTemplates
  where componentTemplates = pickComponentTemplates p c propTypes

pickComponentTemplates :: ProjectType -> ComponentType -> Maybe [Prop] -> [Template]
pickComponentTemplates p c propTypes =
  case p of
    ReactNative -> nativeTemplates c propTypes
    React       -> reactTemplates c propTypes

nativeTemplates :: ComponentType -> Maybe [Prop] -> [Template]
nativeTemplates c propTypes =
  [nativeComponentTemplate c propTypes, stylesTemplate]

reactTemplates :: ComponentType -> Maybe [Prop] -> [Template]
reactTemplates c propTypes =
  [reactComponentTemplate c propTypes]
