module Component.DiagTree.Editor.TreeSearch
     ( diagTreeEditorTreeSearch
     ) where

import Prelude hiding (div)

import Data.Maybe (Maybe (..), maybe)
import Data.String (trim, null)
import Data.String.NonEmpty (NonEmptyString, fromString, toString)

import Record.Builder (merge)

import Effect (Effect)
import Effect.Aff (launchAff_)

import React
     ( ReactClass, component, getProps, getState, modifyState
     )

import React.SyntheticEvent (preventDefault, altKey, ctrlKey, shiftKey, key)
import React.DOM (div, input, button, i)

import React.DOM.Props
     ( className, value, _type, placeholder, title, disabled
     , onChange, onClick, onKeyUp
     )

import Utils ((<.>), storeConnect, eventInputValue)

import Utils.Debouncer
     ( DebouncerSubscription
     , newDebouncer
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

diagTreeEditorTreeSearchRender = defineComponent $
  \ { changeHandler, clearHandler, keyHandler } { isDisabled } { query } ->

  [ input
      [ className $ classSfx "search-input"
      , _type "text"
      , placeholder "Поиск"
      , value query
      , disabled isDisabled
      , onChange changeHandler
      , onKeyUp keyHandler
      ]

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

    onChangeHandler this changeDebouncer event = do
      query <- eventInputValue event
      modifyState this _ { query = query }
      sendToDebouncer changeDebouncer query

    onClearHandler appCtx this changeDebouncer event = do
      preventDefault event
      resetSearch appCtx this changeDebouncer

    onKeyHandler appCtx this changeDebouncer event = go where
      condition a b c d = a && b && c && d
      f x = if x then resetSearch appCtx this changeDebouncer else pure unit

      go  =  f
         =<< condition
         <$> (not <$> altKey   event)
         <*> (not <$> ctrlKey  event)
         <*> (not <$> shiftKey event)
         <*> (key event <#> (_ == "Escape"))

    resetSearch appCtx this changeDebouncer = do
      act appCtx ResetSearch

      -- In case escape pressed before debounced request
      sendToDebouncer changeDebouncer ""
      modifyState this _ { query = "" }

    searchHandler appCtx query = act appCtx $
      case fromString $ trim query of
           Nothing -> ResetSearch
           Just x  -> SearchByQuery x

    defineComponent renderFn = component name \this -> do
      { appContext, searchQuery } <- getProps this
      changeDebouncer <- newDebouncer 500
      let search = searchHandler appContext

      let preBound =
            { changeHandler : onChangeHandler this changeDebouncer
            , clearHandler  : onClearHandler appContext this changeDebouncer
            , keyHandler    : onKeyHandler appContext this changeDebouncer
            }

      let state =
            { changeSubscription: (Nothing :: Maybe DebouncerSubscription)
            , query: maybe "" toString searchQuery
            }

      let r = renderFn preBound

      pure
        { state
        , render: map wrapper $ r <$> getProps this <*> getState this

        , unsafeComponentWillMount: do
            subscription <- subscribeToDebouncer changeDebouncer search
            modifyState this _ { changeSubscription = Just subscription }

        , unsafeComponentWillReceiveProps: \ { searchQuery: newQuery } -> do
            { searchQuery: oldQuery } <- getProps this

            if newQuery == oldQuery
               then pure unit
               else case newQuery <#> toString of
                         Nothing -> modifyState this _ { query = "" }
                         Just x  -> do
                           { query } <- getState this
                           if trim query /= x
                              then modifyState this _ { query = x }
                              else pure unit

        , componentWillUnmount: do
            { changeSubscription } <- getState this

            case changeSubscription of
                 Nothing -> pure unit
                 Just x  -> unsubscribeFromDebouncer changeDebouncer x
        }


diagTreeEditorTreeSearch
  :: ReactClass { appContext :: AppContext, isDisabled :: Boolean }

diagTreeEditorTreeSearch = storeConnect f diagTreeEditorTreeSearchRender where
  f appState = merge { searchQuery } where
    { searchQuery } = appState.diagTree.editor.treeSearch


act :: AppContext -> DiagTreeEditorTreeSearchAction -> Effect Unit
act ctx = launchAff_ <<< dispatch ctx <<< DiagTree <<< Editor <<< TreeSearch
