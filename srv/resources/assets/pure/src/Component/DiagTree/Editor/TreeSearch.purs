module Component.DiagTree.Editor.TreeSearch
     ( diagTreeEditorTreeSearch
     ) where

import Prelude

import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (Maybe (..), maybe)
import Data.Record.Builder (merge)

import Control.Monad.Eff.Console (log)

import React
     ( ReactClass, createClass, spec'
     , getProps, readState, transformState
     )

import React.DOM (IsDynamic (IsDynamic), mkDOM)
import React.DOM.Props (value, onChange, _type, placeholder)
import React.Spaces ((!), (!.), renderIn)
import React.Spaces.DOM (input)

import RxJS.ReplaySubject (just, debounceTime, send, subscribeNext)
import RxJS.Subscription (unsubscribe)

import Utils (storeConnect)
import App.Store (AppContext)


diagTreeEditorTreeSearchRender :: ReactClass { appContext :: AppContext }
diagTreeEditorTreeSearchRender = createClass $ spec $
  \props { changeObservable, changeHandler, query } -> do
    input !. classSfx "search-input"
      ! _type "text"
      ! placeholder "Поиск"
      ! value query
      ! onChange changeHandler

  where
    name = "diag-tree-editor-tree-search"
    classSfx s = name <> "--" <> s
    wrapper = mkDOM (IsDynamic false) name []

    onChangeHandler this event = do
      { changeObservable } <- readState this
      let query = unsafeCoerce event # _.currentTarget.value
      transformState this _ { query = query }
      send query changeObservable

    getInitialState this = do
      {-- { appContext } <- getProps this --}

      pure { changeObservable   : just "" # debounceTime 500
           , changeSubscription : Nothing
           , changeHandler      : onChangeHandler this
           , query              : ""
           }

    spec renderFn =
      let
        renderHandler this = do
          props <- getProps  this
          state <- readState this
          pure $ renderIn wrapper $ renderFn props state
      in
        spec' getInitialState renderHandler # _
          { displayName = "DiagTreeEditorTreeSearch"

          , componentWillMount = \this -> do
              { changeObservable } <- readState this
              subscription <- flip subscribeNext changeObservable log
              transformState this _ { changeSubscription = Just subscription }

          , componentWillUnmount = \this -> do
              { changeSubscription } <- readState this
              maybe (pure unit) unsubscribe changeSubscription
          }


diagTreeEditorTreeSearch :: ReactClass { appContext :: AppContext }
diagTreeEditorTreeSearch = {--storeConnect f--} diagTreeEditorTreeSearchRender
  {-- where --}
  {--   f appState = merge $ let branch = appState.diagTree.editor.treeSearch in --}
  {--     { searchQuery : branch.searchQuery --}
  {--     } --}
