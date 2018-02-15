module Component.DiagTree.Editor
     ( diagTreeEditor
     ) where

import Prelude

import React (ReactClass)
import React.DOM (IsDynamic (IsDynamic), mkDOM, text, h1')
import React.DOM.Props (className)

import Utils (StoreConnectEff, createClassStatelessWithName)
import App.Store (AppContext)


diagTreeEditorRender
  :: forall eff
   . ReactClass { appContext :: AppContext (StoreConnectEff eff) }

diagTreeEditorRender = f $ \props -> wrap
  [ h1' [text "TODO diag tree editor"]
  ]

  where
    wrap = mkDOM (IsDynamic false) "diag-tree-editor" [className "container"]
    f = createClassStatelessWithName "DiagTreeEditor"


diagTreeEditor
  :: forall eff
   . ReactClass { appContext :: AppContext (StoreConnectEff eff) }

diagTreeEditor = diagTreeEditorRender
