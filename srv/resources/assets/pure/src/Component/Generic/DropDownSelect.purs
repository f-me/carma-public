module Component.Generic.DropDownSelect
     ( dropDownSelect
     , OnSelectedEff
     ) where

import Prelude

import Control.Monad.Eff (Eff, kind Effect)
import Control.Alt ((<|>))

import Data.Maybe (Maybe (..), maybe, fromMaybe, isJust)
import Data.Tuple (Tuple (Tuple), snd)
import Data.Foldable (class Foldable, foldl)
import Data.Array (snoc)

import React.DOM (div, li) as R
import React.DOM.Props (className, key, _type, href, role, disabled, onClick)
import React.Spaces ((!), (!.), renderIn, text, empty, elements)
import React.Spaces.DOM (span, button, li, a)
import React.Spaces.DOM.Dynamic (ul)

import React
     ( ReactClass, ReactProps, ReactState, ReactRefs, ReadWrite, ReadOnly
     , createClass, spec'
     , getProps, readState, transformState
     , preventDefault
     )

import Utils ((<.>))
import App.Store (AppContext)


type OnSelectedEff eff =
  ( props :: ReactProps
  , state :: ReactState ReadWrite
  , refs  :: ReactRefs  ReadOnly
  | eff
  ) :: # Effect

type Props f a eff =
  { appContext  :: AppContext
  , isDisabled  :: Boolean
  , variants    :: f a
  , selected    :: Maybe a
  , variantView :: a -> String -- Used to show variant title
  , onSelected  :: Maybe (Maybe a -> Eff (OnSelectedEff eff) Unit)

  , placeholder :: Maybe String
    -- ^ Shown as a dropdown button title when `selected` is `Nothing`
    --   otherwise `notSelectedTitle` will be used or "…" otherwise.

  , notSelectedTitle :: Maybe String
    -- ^ You could not have "not selected" option at all
    --   if it is set to `Nothing`.
  }


-- TODO click outside should close dropdown
dropDownSelectRender
  :: forall f a eff. Foldable f => Eq a => ReactClass (Props f a eff)

dropDownSelectRender = createClass $ spec $
  \ props@{ isDisabled, variants, selected, variantView, placeholder
          , notSelectedTitle
          }

    state@{ isOpened, onToggle, onSelect } -> do

  button !. "btn btn-default dropdown-toggle"
         ! _type "button"
         ! onClick onToggle
         ! disabled isDisabled
         $ do

    let variantTitle = variantView <$> selected

        btnLabelClassy =
          maybe (_ !. classSfx "not-selected-label") (const id) variantTitle

        btnLabelText = fromMaybe "…" $
          variantTitle <|> placeholder <|> notSelectedTitle

    btnLabelClassy span $ text $ btnLabelText <> " "
    span !. "caret" $ empty

  ul !. "dropdown-menu" $ do

    case notSelectedTitle of
         Nothing -> empty
         Just x  -> do
           let markSelected el =
                 if isJust selected
                    then el ! onClick (onSelect Nothing)
                    else el !. classSfx "selected"

               markSelectedLink el =
                 if isJust selected
                    then el ! href "#"
                    else el

           markSelected li ! key "-1" $
             markSelectedLink a $ span !. classSfx "not-selected-label" $ text x

           li !. "divider" ! role "separator" ! key "0" $ empty

    elements $ snd $ foldl (itemReducer props state) (Tuple 1 []) variants

  where
    name = "DropDownSelect"
    classSfx s = name <> "--" <> s
    classNameSfx = className <<< classSfx

    itemReducer { selected, variantView } { onSelect } (Tuple n list) item =
      Tuple (n + 1) $ list `snoc`

        let isSelected = selected <#> (_ == item)
            keyProp    = key $ show n

         in if isSelected == Nothing || isSelected == Just false

               then renderIn (R.li [keyProp, onClick $ onSelect $ Just item]) $
                      a ! href "#" $ text $ variantView item

               else renderIn (R.li [keyProp, classNameSfx "selected"]) $
                      a $ text $ variantView item

    selectHandler this item event = do
      preventDefault event
      transformState this _ { isOpened = false }
      { onSelected, isDisabled } <- getProps this

      if isDisabled
         then pure unit
         else case onSelected of
                   Nothing -> pure unit
                   Just f  -> f item

    toggleHandler this _ = do
      { isDisabled } <- getProps this

      if isDisabled
         then pure unit
         else transformState this \s -> s { isOpened = not s.isOpened }

    getInitialState this = pure
      { isOpened: false
      , onToggle: toggleHandler this
      , onSelect: selectHandler this
      }

    spec renderFn =
      spec' getInitialState renderHandler # _ { displayName = name }

      where
        renderHandler this = do
          props <- getProps  this
          state <- readState this
          let w = wrapper $ state.isOpened && not props.isDisabled
          pure $ renderIn w $ renderFn props state

        wrapper isOpened = R.div [className $ addOpenClass "dropdown" <.> name]
          where addOpenClass x = if isOpened then x <.> "open" else x


dropDownSelect
  :: forall f a eff. Foldable f => Eq a => ReactClass (Props f a eff)

dropDownSelect = dropDownSelectRender
