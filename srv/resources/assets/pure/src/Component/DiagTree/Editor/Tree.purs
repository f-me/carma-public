module Component.DiagTree.Editor.Tree
     ( diagTreeEditorTree
     ) where

import Prelude

import React (ReactClass)
import React.DOM (IsDynamic (IsDynamic), mkDOM)
import React.Spaces (renderIn, text)

import Utils (createClassStatelessWithName)
import App.Store (AppContext)


diagTreeEditorTreeRender :: ReactClass { appContext :: AppContext }
diagTreeEditorTreeRender = f $ do
  text $ "TODO " <> name

  where
    name = "diag-tree-editor-tree"
    displayName = "DiagTreeEditorTree"
    wrapper = mkDOM (IsDynamic false) name []
    f = createClassStatelessWithName displayName <<< const <<< renderIn wrapper


diagTreeEditorTree :: ReactClass { appContext :: AppContext }
diagTreeEditorTree = diagTreeEditorTreeRender
