module Component.DiagTree.Editor
     ( diagTreeEditor
     ) where

import Prelude

import React (ReactClass, createClassStateless)
import React.DOM (IsDynamic (IsDynamic), mkDOM, text, h1')
import React.DOM.Props (className)

import Utils (StoreConnectEff)
import App.Store (AppContext)


diagTreeEditorRender
  :: forall eff
   . ReactClass { appContext :: AppContext (StoreConnectEff eff) }

diagTreeEditorRender = createClassStateless $ \props -> wrapper
  [ h1' [text "TODO diag tree editor"]
  ]

  where
    wrapper = mkDOM (IsDynamic false) "diag-tree-editor" [className "container"]


diagTreeEditor
  :: forall eff
   . ReactClass { appContext :: AppContext (StoreConnectEff eff) }

diagTreeEditor = diagTreeEditorRender
