module Component.DiagTree.Editor.TreeSearch
     ( diagTreeEditorTreeSearch
     ) where

import Prelude

import Data.Maybe (Maybe (..), maybe)
import Data.Record.Builder (merge)
import Data.String (trim)
import Data.String.NonEmpty (NonEmptyString, fromString, toString)

import Control.Monad.Eff (Eff)
import Control.Monad.Aff (launchAff_)
import Control.Monad.Aff.AVar (AVAR)

import React
     ( ReactClass, createClass, spec'
     , getProps, readState, transformState
     )

import React.DOM (IsDynamic (IsDynamic), mkDOM)
import React.DOM.Props (value, onChange, onClick, _type, placeholder)
import React.Spaces ((!), (!.), renderIn, empty)
import React.Spaces.DOM (input, button, i)

import RxJS.ReplaySubject (just, debounceTime, send, subscribeNext)
import RxJS.Subscription (unsubscribe)

import Utils ((<.>), storeConnect, eventInputValue)
import App.Store (AppContext, dispatch)
import App.Store.Actions (AppAction (DiagTree))
import App.Store.DiagTree.Actions (DiagTreeAction (Editor))

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (TreeSearch)
     )

import App.Store.DiagTree.Editor.TreeSearch.Actions
     ( DiagTreeEditorTreeSearchAction (SearchByQuery, ResetSearch)
     )


diagTreeEditorTreeSearchRender
  :: ReactClass { appContext  :: AppContext
                , searchQuery :: Maybe NonEmptyString
                }

diagTreeEditorTreeSearchRender = createClass $ spec $
  \ { changeObservable, changeHandler, clearHandler, query } -> do
    input !. classSfx "search-input"
      ! _type "text"
      ! placeholder "Поиск"
      ! value query
      ! onChange changeHandler
    button !. classSfx "clear" ! onClick clearHandler $
      i !. "glyphicon" <.> "glyphicon-remove" $ empty

  where
    name = "diag-tree-editor-tree-search"
    classSfx s = name <> "--" <> s
    wrapper = mkDOM (IsDynamic false) name []

    onChangeHandler this event = do
      { changeObservable } <- readState this
      let query = eventInputValue event
      transformState this _ { query = query }
      send query changeObservable

    onClearHandler appCtx = act appCtx ResetSearch

    searchHandler appCtx query = act appCtx $
      case fromString $ trim query of
           Nothing -> ResetSearch
           Just x  -> SearchByQuery x

    getInitialState this = do
      { appContext, searchQuery } <- getProps this

      pure { changeObservable   : just "" # debounceTime 500
           , changeSubscription : Nothing
           , changeHandler      : onChangeHandler this
           , clearHandler       : const $ onClearHandler appContext
           , query              : maybe "" toString searchQuery
           , search             : searchHandler appContext
           }

    spec renderFn =
      let
        renderHandler this = readState this <#> renderFn >>> renderIn wrapper
      in
        spec' getInitialState renderHandler # _
          { displayName = "DiagTreeEditorTreeSearch"

          , componentWillMount = \this -> do
              { changeObservable, search } <- readState this
              subscription <- flip subscribeNext changeObservable search
              transformState this _ { changeSubscription = Just subscription }

          , componentWillReceiveProps = \this { searchQuery: newQuery } -> do
              { searchQuery: oldQuery } <- getProps this

              when (newQuery /= oldQuery) $
                case newQuery <#> toString of
                     Nothing -> transformState this _ { query = "" }
                     Just x  -> do
                       { query } <- readState this
                       if trim query /= x
                          then transformState this _ { query = x }
                          else pure unit

          , componentWillUnmount = \this -> do
              { changeSubscription } <- readState this
              maybe (pure unit) unsubscribe changeSubscription
          }


diagTreeEditorTreeSearch :: ReactClass { appContext :: AppContext }
diagTreeEditorTreeSearch = storeConnect f diagTreeEditorTreeSearchRender
  where
    f appState =
      let { searchQuery } = appState.diagTree.editor.treeSearch
       in merge { searchQuery }


act :: forall eff
     . AppContext
    -> DiagTreeEditorTreeSearchAction
    -> Eff (avar :: AVAR | eff) Unit

act ctx = launchAff_ <<< dispatch ctx <<< DiagTree <<< Editor <<< TreeSearch
