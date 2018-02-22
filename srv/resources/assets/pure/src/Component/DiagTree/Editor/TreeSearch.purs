module Component.DiagTree.Editor.TreeSearch
     ( diagTreeEditorTreeSearch
     ) where

import Prelude

import Unsafe.Coerce (unsafeCoerce)
import Data.Maybe (Maybe (..), maybe)
import Data.Record.Builder (merge)
import Data.String (trim)
import Data.String.NonEmpty (NonEmptyString, fromString, toString)

import Control.Monad.Aff (launchAff_)

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
  \ { changeObservable, changeHandler, query } -> do
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

    searchHandler appCtx query =
      launchAff_ $ dispatch appCtx $ DiagTree $ Editor $ TreeSearch $
        case fromString $ trim query of
             Nothing -> ResetSearch
             Just x  -> SearchByQuery x

    getInitialState this = do
      { appContext, searchQuery } <- getProps this

      pure { changeObservable   : just "" # debounceTime 500
           , changeSubscription : Nothing
           , changeHandler      : onChangeHandler this
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
