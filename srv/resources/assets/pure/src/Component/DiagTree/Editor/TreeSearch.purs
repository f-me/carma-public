module Component.DiagTree.Editor.TreeSearch
     ( diagTreeEditorTreeSearch
     ) where

import Prelude hiding (div)

import Data.Monoid (mempty)
import Data.Maybe (Maybe (..), maybe)
import Data.Record.Builder (merge)
import Data.String (trim, null)
import Data.String.NonEmpty (NonEmptyString, fromString, toString)

import Control.Monad.Eff (Eff)
import Control.Monad.Aff (launchAff_)
import Control.Monad.Aff.AVar (AVAR)

import React
     ( ReactClass, createClass, spec'
     , getProps, readState, transformState
     , preventDefault
     )

import React.DOM (div, input, button, i)

import React.DOM.Props
     ( className, value, _type, placeholder, title, disabled
     , onChange, onClick, onKeyUp
     )

import Utils ((<.>), storeConnect, eventInputValue)

import Utils.Debouncer
     ( newDebouncer
     , subscribeToDebouncer
     , unsubscribeFromDebouncer
     , sendToDebouncer
     )

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
                , isDisabled  :: Boolean
                , searchQuery :: Maybe NonEmptyString
                }

diagTreeEditorTreeSearchRender = createClass $ spec $
  \ { isDisabled } { changeHandler, clearHandler, keyHandler, query } ->

  [ input
      [ className $ classSfx "search-input"
      , _type "text"
      , placeholder "Поиск"
      , value query
      , disabled isDisabled
      , onChange changeHandler
      , onKeyUp keyHandler
      ]
      mempty

  , button
      [ className $ classSfx "clear"
      , disabled $ isDisabled || null query
      , onClick clearHandler
      , title "Очистить строку поиска"
      ]
      [ i [className $ "glyphicon" <.> "glyphicon-remove"] mempty ]
  ]

  where
    name = "DiagTreeEditorTreeSearch"
    classSfx s = name <> "--" <> s
    wrapper = div [className name]

    onChangeHandler this event = do
      { changeDebouncer } <- readState this
      let query = eventInputValue event
      transformState this _ { query = query }
      sendToDebouncer changeDebouncer query

    onClearHandler appCtx this event = do
      preventDefault event
      resetSearch appCtx this

    onKeyHandler appCtx this { altKey, ctrlKey, shiftKey, key } =
      if not altKey && not ctrlKey && not shiftKey && key == "Escape"
         then resetSearch appCtx this
         else pure unit

    resetSearch appCtx this = do
      { changeDebouncer } <- readState this
      act appCtx ResetSearch

      -- In case escape pressed before debounced request
      sendToDebouncer changeDebouncer ""
      transformState this _ { query = "" }

    searchHandler appCtx query = act appCtx $
      case fromString $ trim query of
           Nothing -> ResetSearch
           Just x  -> SearchByQuery x

    getInitialState this = do
      { appContext, searchQuery } <- getProps this
      changeDebouncer <- newDebouncer 500

      pure { changeDebouncer
           , changeSubscription : Nothing
           , changeHandler      : onChangeHandler this
           , clearHandler       : onClearHandler appContext this
           , keyHandler         : onKeyHandler appContext this
           , query              : maybe "" toString searchQuery
           , search             : searchHandler appContext
           }

    spec renderFn = go where
      renderHandler this =
        map wrapper $ renderFn <$> getProps this <*> readState this

      go
        = spec' getInitialState renderHandler # _
        { displayName = name

        , componentWillMount = \this -> do
            { changeDebouncer, search } <- readState this
            subscription <- subscribeToDebouncer changeDebouncer search
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
            { changeDebouncer, changeSubscription } <- readState this

            case changeSubscription of
                 Nothing -> pure unit
                 Just x  -> unsubscribeFromDebouncer changeDebouncer x
        }


diagTreeEditorTreeSearch
  :: ReactClass { appContext :: AppContext, isDisabled :: Boolean }

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
