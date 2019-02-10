module Component.Generic.DropDownSelect
     ( dropDownSelect
     ) where

import Prelude hiding (div)

import Control.Alt ((<|>))

import Data.Monoid (mempty)
import Data.Maybe (Maybe (..), maybe, fromMaybe, isJust, isNothing)
import Data.Tuple (Tuple (Tuple), snd)
import Data.Foldable (class Foldable, foldl)
import Data.Array (snoc)

import React.DOM (div, li, span, button, a, a', text)
import React.DOM.Dynamic (ul)

import React.DOM.Props
     ( className, _type, href, role, disabled
     , key, withRef
     , onClick
     )

import React
     ( ReactClass, EventHandler
     , createClass, spec'
     , getProps, readState, transformState
     , writeRef, readRef, preventDefault
     )

import Utils ((<.>), callEventHandler)
import Utils.React.OutsideClick (subscribeOutsideClick, unsubscribeOutsideClick)
import App.Store (AppContext)


type Props f a eff =
  { appContext  :: AppContext
  , isDisabled  :: Boolean
  , variants    :: f a
  , selected    :: Maybe a
  , variantView :: a -> String -- Used to show variant title
  , onSelected  :: Maybe (EventHandler (Maybe a))

  , placeholder :: Maybe String
    -- ^ Shown as a dropdown button title when `selected` is `Nothing`
    --   otherwise `notSelectedTitle` will be used or "…" otherwise.

  , notSelectedTitle :: Maybe String
    -- ^ You could not have "not selected" option at all
    --   if it is set to `Nothing`.
  }


dropDownSelectRender
  :: forall f a eff. Foldable f => Eq a => ReactClass (Props f a eff)

dropDownSelectRender = createClass $ spec $
  \ props@{ isDisabled, variants, selected, variantView, placeholder
          , notSelectedTitle
          }

    state@{ isOpened, onToggle, onSelect } ->

  [ button
      [ className "btn btn-default dropdown-toggle"
      , _type "button"
      , onClick onToggle
      , disabled isDisabled
      ]

      let
        variantTitle = variantView <$> selected

        btnLabelText = fromMaybe "…" $
          variantTitle <|> placeholder <|> notSelectedTitle
      in
        [ span ( if isNothing variantTitle
                    then [className $ classSfx "not-selected-label"]
                    else mempty
               ) [text $ btnLabelText <> " "]

        , span [className "caret"] mempty
        ]

  , ul [className "dropdown-menu"] $ let

      unselectItem = fromMaybe mempty $ notSelectedTitle <#> \title ->
        [ let
            item = li attrs [link] where
              attrs = [key "-1"] `snoc`
                      if isJust selected
                         then onClick $ onSelect Nothing
                         else className $ classSfx "selected"

            link = a attrs [child] where
              attrs = if isJust selected then [href "#"] else mempty
              child = span [className $ classSfx "not-selected-label"]
                           [text title]

          in item

        , li [className "divider", role "separator", key "0"] mempty
        ]

      items = snd $ foldl (itemReducer props state) (Tuple 1 mempty) variants

      in unselectItem <> items
  ]

  where
    name = "DropDownSelect"
    classSfx s = name <> "--" <> s
    classNameSfx = className <<< classSfx

    itemReducer { selected, variantView } { onSelect } (Tuple n list) item =
      Tuple (n + 1) $ list `snoc`

        let isSelected = selected <#> (_ == item)
            keyProp    = key $ show n

         in if isSelected == Nothing || isSelected == Just false

               then li [keyProp, onClick $ onSelect $ Just item]
                       [a [href "#"] [text $ variantView item]]

               else li [keyProp, classNameSfx "selected"]
                       [a' [text $ variantView item]]

    selectHandler this item event = do
      preventDefault event
      transformState this _ { isOpened = false }
      { onSelected, isDisabled } <- getProps this

      if isDisabled
         then pure unit
         else case onSelected of
                   Nothing -> pure unit
                   Just f  -> callEventHandler f item

    toggleHandler this _ = do
      { isDisabled } <- getProps this

      if isDisabled
         then pure unit
         else transformState this \s -> s { isOpened = not s.isOpened }

    rootRefName = "componentRoot"

    hookOutsideClick this outsideSubscription = do
      maybe (pure unit) unsubscribeOutsideClick outsideSubscription
      rootRef <- readRef this rootRefName

      case rootRef of
           Nothing ->
             if isNothing outsideSubscription
                then pure unit
                else transformState this _ { outsideSubscription = Nothing }

           Just ref -> do
             subscription <-
               flip subscribeOutsideClick ref $
                 transformState this _ { isOpened = false }

             transformState this _ { outsideSubscription = Just subscription }

    unhookOutsideClick this outsideSubscription = do
      case outsideSubscription of
           Nothing -> pure unit
           Just x  -> do
             unsubscribeOutsideClick x
             transformState this _ { outsideSubscription = Nothing }

    getInitialState this = pure
      { isOpened: false
      , onToggle: toggleHandler this
      , onSelect: selectHandler this
      , refSetter: writeRef this rootRefName
      , outsideSubscription: Nothing
      }

    spec renderFn =
      spec' getInitialState renderHandler # _
        { displayName = name

        , componentWillUpdate =
            \this _ { isOpened, outsideSubscription } -> do
              { isOpened: prevIsOpened } <- readState this

              if (not prevIsOpened && isOpened) ||
                 (isOpened && isNothing outsideSubscription)
                 then hookOutsideClick this outsideSubscription
                 else pure unit

              if (not isOpened && prevIsOpened) ||
                 (not isOpened && isJust outsideSubscription)
                 then unhookOutsideClick this outsideSubscription
                 else pure unit

        , componentWillUnmount = \this -> do
            { outsideSubscription } <- readState this
            maybe (pure unit) unsubscribeOutsideClick outsideSubscription
        }

      where
        renderHandler this = do
          props@{ isDisabled } <- getProps this
          state@{ refSetter, isOpened } <- readState this
          let w = wrapper refSetter $ isOpened && not isDisabled
          pure $ w $ renderFn props state

        wrapper refSetter isOpened = go where
          addOpenClass x = if isOpened then x <.> "open" else x
          go = div [ className $ addOpenClass "dropdown" <.> name
                   , withRef refSetter
                   ]


dropDownSelect
  :: forall f a eff. Foldable f => Eq a => ReactClass (Props f a eff)

dropDownSelect = dropDownSelectRender
