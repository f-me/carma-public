module Component.DiagTree.Editor
     ( diagTreeEditor
     ) where

import Prelude

import Data.Record.Builder (merge)

import Control.Monad.Aff (launchAff_, delay, Milliseconds (Milliseconds))
import Control.Monad.Aff.Unsafe (unsafeCoerceAff)

import React (ReactClass, getProps, createElement)
import React.DOM (IsDynamic (IsDynamic), mkDOM, text, h1')
import React.DOM.Props (className)

import Utils (createClassStatelessWithSpec, storeConnect)
import App.Store (AppContext, dispatch)
import App.Store.Types (StoreConnectEffects)
import App.Store.Actions (AppAction (DiagTree))
import App.Store.DiagTree.Actions (DiagTreeAction (Editor))
import Component.Spinner (spinner)

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (LoadSlidesRequest)
     )


diagTreeEditorRender
  :: forall eff
   . ReactClass { isSlidesLoading :: Boolean
                , isSlidesLoaded  :: Boolean
                , appContext      :: AppContext (StoreConnectEffects eff)
                }

diagTreeEditorRender = f $ \props ->
  [ h1' [text $ "TODO diag tree editor | " <> show props.isSlidesLoading]
  ]

  where
    wrapper = mkDOM (IsDynamic false) "diag-tree-editor" [className "container"]

    f render = spec $ \props -> wrapper
      if props.isSlidesLoading
         then [ createElement spinner { appContext : props.appContext } [] ]
         else render props

    spec = createClassStatelessWithSpec $ _
      { displayName = "DiagTreeEditor"

      , componentDidMount = \this -> do
          props <- getProps this

          launchAff_ $ do
            -- TODO FIXME temporary hack to solve dispatching after all
            --            subscribers are subscribed (parent `componentDidMount`
            --            is executed after child one).
            unsafeCoerceAff $ delay $ Milliseconds 0.0

            unsafeCoerceAff $
              dispatch props.appContext $ DiagTree $ Editor $ LoadSlidesRequest
      }


diagTreeEditor
  :: forall eff
   . ReactClass { appContext :: AppContext (StoreConnectEffects eff) }

diagTreeEditor = storeConnect f diagTreeEditorRender
  where
    f appState = merge $ let branch = appState.diagTree.editor in
      { isSlidesLoading : branch.isSlidesLoading
      , isSlidesLoaded  : branch.isSlidesLoaded
      }
