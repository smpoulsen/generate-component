module Templates where

import           Templates.Components
import           Templates.Containers
import           Templates.Styles
import           Types

templatesToGenerate :: ProjectType -> Bool -> [Template]
templatesToGenerate p container =
  if container
    then containerTemplate : containerIndexTemplate : componentTemplates
    else indexTemplate : componentTemplates
  where componentTemplates = pickComponentTemplates p

pickComponentTemplates :: ProjectType -> [Template]
pickComponentTemplates p =
  case p of
    ReactNative -> nativeTemplates
    React       -> reactTemplates

nativeTemplates :: [Template]
nativeTemplates =
  [nativeComponentTemplate, stylesTemplate]

reactTemplates :: [Template]
reactTemplates =
  [reactComponentTemplate]
