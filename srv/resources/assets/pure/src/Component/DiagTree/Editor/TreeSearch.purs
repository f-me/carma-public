module Component.DiagTree.Editor.TreeSearch
     ( diagTreeEditorTreeSearch
     ) where

import Prelude

import React (ReactClass)
import React.DOM (IsDynamic (IsDynamic), mkDOM)
import React.Spaces (renderIn, text)

import Utils (createClassStatelessWithName)
import App.Store (AppContext)


diagTreeEditorTreeSearchRender :: ReactClass { appContext :: AppContext }
diagTreeEditorTreeSearchRender = f $ do
  text $ "TODO " <> name

  where
    name = "diag-tree-editor-tree-search"
    displayName = "DiagTreeEditorTreeSearch"
    wrapper = mkDOM (IsDynamic false) name []
    f = createClassStatelessWithName displayName <<< const <<< renderIn wrapper


diagTreeEditorTreeSearch :: ReactClass { appContext :: AppContext }
diagTreeEditorTreeSearch = diagTreeEditorTreeSearchRender
