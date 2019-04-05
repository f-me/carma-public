module Component.Generic.DropDownSelect
     ( dropDownSelect
     ) where

import Prelude hiding (div)

import Data.Maybe (Maybe (..), maybe, fromMaybe, isJust, isNothing)
import Data.Tuple (Tuple (Tuple), snd)
import Data.Foldable (class Foldable, foldl)
import Data.Array (snoc)
import Data.Nullable (toMaybe)

import Control.Alt ((<|>))
import Control.MonadZero (guard)

import Effect (Effect)
import Effect.Ref (Ref, new, read, write) as Ref

import React.SyntheticEvent (preventDefault)
import React.DOM (div, li, span, button, a, a', text)
import React.DOM.Dynamic (ul)

import React.DOM.Props
     ( className, _type, href, role, disabled, key, ref, onClick
     )

import React
     ( ReactClass, ReactRef, component, getProps, getState, modifyState
     )

import Utils ((<.>))

import Utils.React.OutsideClick
     ( OutsideClickSubscription, subscribeOutsideClick, unsubscribeOutsideClick
     )

import App.Store (Store)


type Props state action f a =
   { store       :: Store state action
   , isDisabled  :: Boolean
   , variants    :: f a -- ^ Some list (Foldable) of `a`
   , selected    :: Maybe a
   , variantView :: a -> String -- ^ Used to show variant title

   , onSelected :: Maybe (Maybe a -> Effect Unit)
     -- ^ Outer `Maybe` indicates whether callback is set
     --   and inner `Maybe` is `selected`.

   , placeholder :: Maybe String
     -- ^ Shown as a dropdown button title when `selected` is `Nothing`
     --   otherwise `notSelectedTitle` will be used or "…" otherwise.

   , notSelectedTitle :: Maybe String
     -- ^ You could not have "not selected" option at all
     --   if it is set to `Nothing`.
   }


dropDownSelectRender
  :: forall state action f a
   . Foldable f
  => Eq a
  => ReactClass (Props state action f a)

dropDownSelectRender = defineComponent $
  \ { onToggle, onSelect, itemReducer: itemReducer' }

    props@{ isDisabled, variants, selected, variantView, placeholder
          , notSelectedTitle
          } ->

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

      items = snd $ foldl (itemReducer' props) (Tuple 1 mempty) variants

      in unselectItem <> items
  ]

  where
    name = "DropDownSelect"
    classSfx s = name <> "--" <> s
    classNameSfx = className <<< classSfx

    itemReducer onSelect { selected, variantView } (Tuple n list) item =
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
      modifyState this _ { isOpened = false }
      { onSelected, isDisabled } <- getProps this
      maybe (pure unit) (_ $ item) $ guard (not isDisabled) *> onSelected

    toggleHandler this _ = do
      { isDisabled } <- getProps this

      if isDisabled
         then pure unit
         else modifyState this \s -> s { isOpened = not s.isOpened }

    hookOutsideClick this readRootRef outsideSubscription = do
      maybe (pure unit) unsubscribeOutsideClick outsideSubscription
      rootRef <- readRootRef

      case rootRef of
           Nothing ->
             if isNothing outsideSubscription
                then pure unit
                else modifyState this _ { outsideSubscription = Nothing }

           Just ref -> do
             subscription <-
               flip subscribeOutsideClick ref $
                 modifyState this _ { isOpened = false }

             modifyState this _ { outsideSubscription = Just subscription }

    unhookOutsideClick this outsideSubscription = do
      case outsideSubscription of
           Nothing -> pure unit
           Just x  -> do
             unsubscribeOutsideClick x
             modifyState this _ { outsideSubscription = Nothing }

    defineComponent renderFn = component name \this -> do
      (rootRef :: Ref.Ref (Maybe ReactRef)) <- Ref.new Nothing

      let onSelect = selectHandler this

      let preBound =
            { onToggle: toggleHandler this
            , onSelect
            , itemReducer: itemReducer onSelect
            }

      let w = wrapper $ toMaybe >>> flip Ref.write rootRef
          r = renderFn preBound
          readRootRef = Ref.read rootRef

      let state =
            { isOpened: false
            , outsideSubscription: (Nothing :: Maybe OutsideClickSubscription)
            }

      pure
        { state

        , render: do
            props@{ isDisabled } <- getProps this
            { isOpened } <- getState this
            pure $ w (isOpened && not isDisabled) $ r props

        , unsafeComponentWillUpdate:
            \ _ { isOpened, outsideSubscription } -> do
              { isOpened: prevIsOpened } <- getState this

              if (not prevIsOpened && isOpened) ||
                 (isOpened && isNothing outsideSubscription)
                 then hookOutsideClick this readRootRef outsideSubscription
                 else pure unit

              if (not isOpened && prevIsOpened) ||
                 (not isOpened && isJust outsideSubscription)
                 then unhookOutsideClick this outsideSubscription
                 else pure unit

        , componentWillUnmount: do
            { outsideSubscription } <- getState this
            maybe (pure unit) unsubscribeOutsideClick outsideSubscription
        }

    wrapper refSetter isOpened = go where
      addOpenClass x = if isOpened then x <.> "open" else x
      go = div [ className $ addOpenClass "dropdown" <.> name
               , ref refSetter
               ]


dropDownSelect
  :: forall state action f a
   . Foldable f
  => Eq a
  => ReactClass (Props state action f a)

dropDownSelect = dropDownSelectRender
