module Component.DiagTree.Editor.SlideEditor.Answers
     ( diagTreeEditorSlideEditorAnswers
     ) where

import Prelude hiding (div)

import Data.Monoid (mempty)
import Data.Tuple (Tuple (Tuple), snd)
import Data.Foldable (class Foldable, foldl, length)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Nullable (toNullable)
import Data.Array (snoc)

import React.DOM (text, div, button, label, i)
import React.DOM.Dynamic (ul)
import React.DOM.Props (className, _type, disabled, onClick)

import React
     ( ReactClass, EventHandler
     , createClass, spec', createElement
     , getProps, readState, transformState
     , handle
     )

import Utils ((<.>))
import App.Store (AppContext)

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlide (DiagTreeSlide)
     , DiagTreeSlideId
     , DiagTreeSlideAttachment
     , DiagTreeSlideAnswer
     )

import App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAttachment
     ( BackendAttachment
     )

import Component.DiagTree.Editor.SlideEditor.Helpers (ItemModification)

import Component.DiagTree.Editor.SlideEditor.Answer
     ( diagTreeEditorSlideEditorAnswer
     )


type Props answers newAnswers =
  { appContext :: AppContext
  , slideId    :: DiagTreeSlideId
  , isDisabled :: Boolean
  , answers    :: answers DiagTreeSlideAnswer

  , newAnswers :: newAnswers { header     :: String
                             , text       :: String
                             , attachment :: Maybe DiagTreeSlideAttachment
                             }

  , updateAnswer -- See answer item props type component for details
      :: EventHandler
           ( ItemModification (Either DiagTreeSlideId Int)
               { header              :: String
               , text                :: String
               , attachment          :: Maybe BackendAttachment
               , isAttachmentDeleted :: Boolean
               } )

  , onMoveUp   :: EventHandler (Either DiagTreeSlideId Int)
  , onMoveDown :: EventHandler (Either DiagTreeSlideId Int)
  }


diagTreeEditorSlideEditorAnswersRender
  :: forall f1 f2 . Foldable f1 => Foldable f2 => ReactClass (Props f1 f2)

diagTreeEditorSlideEditorAnswersRender = createClass $ spec $
  \ { appContext, slideId, isDisabled
    , answers, newAnswers, updateAnswer
    , onMoveUp, onMoveDown
    }
    { isAdding, turnAddingOn, turnAddingOff } ->

  [ label [className "control-label"] [text "Ответы"]

  , ul
      [ className $ "list-group" <.> classSfx "list" ]
      $
      let
        getMoveUp itemIndex =
          if itemIndex <= 0 then Nothing else Just onMoveUp

        getMoveDown itemIndex lastIndex =
          if itemIndex >= lastIndex then Nothing else Just onMoveDown

        reducer lastIndex (Tuple itemIndex list) answer = go where
          go = Tuple (itemIndex + 1) $ list `snoc` itemEl p mempty
          answerSlideId = answer.nextSlide # \(DiagTreeSlide x) -> x.id
          moveUp = getMoveUp itemIndex
          moveDown = getMoveDown itemIndex lastIndex

          p = props (Left answerSlideId) moveUp moveDown
            { header: answer.header
            , text: answer.text
            , attachment: answer.attachment
            }

        newReducer lastIndex (Tuple itemIndex list) answer = go where
          go = Tuple (itemIndex + 1) $ list `snoc` itemEl p mempty
          moveUp = getMoveUp itemIndex
          moveDown = getMoveDown itemIndex lastIndex

          p = props (Right itemIndex) moveUp moveDown
            { header: answer.header
            , text: answer.text
            , attachment: answer.attachment
            }

        props identity moveUp moveDown item =
          { appContext
          , slideId
          , key: toNullable $ Just $ show identity
          , identity: Just identity
          , isDisabled
          , answer: Just item
          , updateAnswer
          , onCancel: Nothing
          , onMoveUp: moveUp
          , onMoveDown: moveDown
          }
      in
        reduceEls (reducer $ length answers - 1) answers <>
        reduceEls (newReducer $ length newAnswers - 1) newAnswers

  , if isAdding
       then flip itemEl mempty
              { appContext
              , slideId
              , key: toNullable Nothing
              , identity: Nothing
              , isDisabled
              , answer: Nothing
              , updateAnswer
              , onCancel: Just turnAddingOff
              , onMoveUp: Nothing
              , onMoveDown: Nothing
              }

       else button
              [ className $ "btn btn-default" <.> classSfx "add-button"
              , _type "button"
              , onClick turnAddingOn
              , disabled isDisabled
              ]
              [ i [className "glyphicon glyphicon-plus"] mempty
              , text " Добавить ответ"
              ]
  ]

  where
    name = "DiagTreeEditorSlideEditorAnswers"
    classSfx s = name <> "--" <> s
    wrapper = div [className $ "form-group" <.> name]
    itemEl = createElement diagTreeEditorSlideEditorAnswer

    reduceEls
      :: forall f a b . Foldable f
      => (Tuple Int (Array b) -> a -> Tuple Int (Array b)) -> f a -> Array b
    reduceEls r = foldl r acc >>> snd where acc = Tuple 0 mempty

    turnAddingHandler this isOn =
      transformState this _ { isAdding = isOn }

    getInitialState this = pure
      { isAdding: false
      , turnAddingOn: const $ turnAddingHandler this true
      , turnAddingOff: let f = turnAddingHandler this false in handle \unit -> f
      }

    spec renderFn = go where
      go = spec' getInitialState renderHandler # _ { displayName = name }

      renderHandler this =
        map wrapper $ renderFn <$> getProps this <*> readState this


diagTreeEditorSlideEditorAnswers
  :: forall f1 f2 . Foldable f1 => Foldable f2 => ReactClass (Props f1 f2)

diagTreeEditorSlideEditorAnswers = diagTreeEditorSlideEditorAnswersRender
