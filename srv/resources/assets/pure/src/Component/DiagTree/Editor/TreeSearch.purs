module Component.DiagTree.Editor.TreeSearch
     ( diagTreeEditorTreeSearch
     ) where

import Prelude hiding (div)

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
     , preventDefault
     )

import React.Spaces ((!), (!.), renderIn, empty)
import React.Spaces.DOM (input, button, i)
import React.DOM (div)

import React.DOM.Props
     ( className, value, onChange, onClick, onKeyUp, _type, placeholder, title
     )

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
  \ { changeObservable
    , changeHandler
    , clearHandler
    , keyHandler
    , query
    } -> do

      input !. classSfx "search-input"
            ! _type "text"
            ! placeholder "Поиск"
            ! value query
            ! onChange changeHandler
            ! onKeyUp keyHandler

      button !. classSfx "clear"
             ! onClick clearHandler
             ! title "Очистить строку поиска" $

        i !. "glyphicon" <.> "glyphicon-remove" $ empty

  where
    name = "DiagTreeEditorTreeSearch"
    classSfx s = name <> "--" <> s
    wrapper = div [className name]

    onChangeHandler this event = do
      { changeObservable } <- readState this
      let query = eventInputValue event
      transformState this _ { query = query }
      send query changeObservable

    onClearHandler appCtx this event = do
      preventDefault event
      resetSearch appCtx this

    onKeyHandler appCtx this { altKey, ctrlKey, shiftKey, key } =
      if not altKey && not ctrlKey && not shiftKey && key == "Escape"
         then resetSearch appCtx this
         else pure unit

    resetSearch appCtx this = do
      { changeObservable } <- readState this
      act appCtx ResetSearch

      -- In case escape pressed before debounced request
      send "" changeObservable
      transformState this _ { query = "" }

    searchHandler appCtx query = act appCtx $
      case fromString $ trim query of
           Nothing -> ResetSearch
           Just x  -> SearchByQuery x

    getInitialState this = do
      { appContext, searchQuery } <- getProps this

      pure { changeObservable   : just "" # debounceTime 500
           , changeSubscription : Nothing
           , changeHandler      : onChangeHandler this
           , clearHandler       : onClearHandler appContext this
           , keyHandler         : onKeyHandler appContext this
           , query              : maybe "" toString searchQuery
           , search             : searchHandler appContext
           }

    spec renderFn =
      let
        renderHandler this = readState this <#> renderFn >>> renderIn wrapper
      in
        spec' getInitialState renderHandler # _
          { displayName = name

          , componentWillMount = \this -> do
              { changeObservable, search } <- readState this
              subscription <- flip subscribeNext changeObservable search
              transformState this _ { changeSubscription = Just subscription }

          , componentWillReceiveProps = \this { searchQuery: newQuery } -> do
              { searchQuery: oldQuery } <- getProps this

              if newQuery == oldQuery
                then pure unit
                else case newQuery <#> toString of
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
