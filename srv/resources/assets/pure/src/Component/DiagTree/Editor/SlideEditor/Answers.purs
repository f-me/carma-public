module Component.DiagTree.Editor.SlideEditor.Answers
     ( diagTreeEditorSlideEditorAnswers
     ) where

import Prelude hiding (div)

import Control.Monad.Eff (Eff)

import Data.Tuple (Tuple (Tuple), snd)
import Data.Foldable (class Foldable, foldl, length)
import Data.Maybe (Maybe (..))
import Data.Either (Either (..))
import Data.Nullable (toNullable)
import Data.Array (snoc)

import React.DOM (div) as R
import React.Spaces ((!), (!.), renderIn, text, empty, elements, element)
import React.Spaces.DOM (button, label, i)
import React.Spaces.DOM.Dynamic (ul) as SDyn
import React.DOM.Props (className, _type, disabled, onClick)

import React
     ( ReactClass, ReactProps, ReactState, ReactRefs, ReadWrite, ReadOnly
     , createClass, spec', createElement
     , getProps, readState, transformState
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


type Props answers newAnswers eff =
  { appContext :: AppContext
  , slideId    :: DiagTreeSlideId
  , isDisabled :: Boolean
  , answers    :: answers DiagTreeSlideAnswer

  , newAnswers :: newAnswers { header     :: String
                             , text       :: String
                             , attachment :: Maybe DiagTreeSlideAttachment
                             }

  , updateAnswer -- See answer item props type component for details
      :: ItemModification (Either DiagTreeSlideId Int)
           { header              :: String
           , text                :: String
           , attachment          :: Maybe BackendAttachment
           , isAttachmentDeleted :: Boolean
           }

      -> Eff ( props :: ReactProps
             , state :: ReactState ReadWrite
             , refs  :: ReactRefs  ReadOnly
             | eff
             ) Unit

  , onMoveUp   :: (Either DiagTreeSlideId Int)
               -> Eff ( props :: ReactProps
                      , state :: ReactState ReadWrite
                      , refs  :: ReactRefs  ReadOnly
                      | eff
                      ) Unit

  , onMoveDown :: (Either DiagTreeSlideId Int)
               -> Eff ( props :: ReactProps
                      , state :: ReactState ReadWrite
                      , refs  :: ReactRefs  ReadOnly
                      | eff
                      ) Unit
  }


diagTreeEditorSlideEditorAnswersRender
  :: forall f1 f2 eff
   . Foldable f1
  => Foldable f2
  => ReactClass (Props f1 f2 eff)

diagTreeEditorSlideEditorAnswersRender = createClass $ spec $
  \ { appContext, slideId, isDisabled
    , answers, newAnswers, updateAnswer
    , onMoveUp, onMoveDown
    }
    { isAdding, turnAddingOn, turnAddingOff } -> do

  label !. "control-label" $ text "Ответы"

  SDyn.ul !. "list-group" <.> classSfx "list" $

    let
      getMoveUp itemIndex =
        if itemIndex <= 0
           then Nothing
           else Just onMoveUp

      getMoveDown itemIndex lastIndex =
        if itemIndex >= lastIndex
           then Nothing
           else Just onMoveDown

      reducer lastIndex (Tuple itemIndex list) answer =
        Tuple (itemIndex + 1) $ list `snoc`
          let
            answerSlideId = answer.nextSlide # \(DiagTreeSlide x) -> x.id
            moveUp = getMoveUp itemIndex
            moveDown = getMoveDown itemIndex lastIndex

            p = props (Left answerSlideId) moveUp moveDown
              { header: answer.header
              , text: answer.text
              , attachment: answer.attachment
              }
          in
            itemEl p []

      newReducer lastIndex (Tuple itemIndex list) answer =
        Tuple (itemIndex + 1) $ list `snoc`
          let
            moveUp = getMoveUp itemIndex
            moveDown = getMoveDown itemIndex lastIndex

            p = props (Right itemIndex) moveUp moveDown
              { header: answer.header
              , text: answer.text
              , attachment: answer.attachment
              }
          in
            itemEl p []

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

    in do
      elements $ snd $
        foldl (reducer $ length answers - 1) (Tuple 0 []) answers

      elements $ snd $
        foldl (newReducer $ length newAnswers - 1) (Tuple 0 []) newAnswers

  if isAdding
     then element $
            itemEl
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
              } []

     else button !. "btn btn-default" <.> classSfx "add-button"
                 ! _type "button"
                 ! onClick turnAddingOn
                 ! disabled isDisabled
                 $ do i !. "glyphicon glyphicon-plus" $ empty
                      text " Добавить ответ"

  where
    name = "DiagTreeEditorSlideEditorAnswers"
    classSfx s = name <> "--" <> s
    wrapper = R.div [className $ "form-group" <.> name]
    renderer = renderIn wrapper
    itemEl = createElement diagTreeEditorSlideEditorAnswer

    turnAddingHandler this isOn =
      transformState this _ { isAdding = isOn }

    getInitialState this = pure
      { isAdding: false
      , turnAddingOn: const $ turnAddingHandler this true
      , turnAddingOff: turnAddingHandler this false
      }

    spec renderFn =
      spec' getInitialState renderHandler # _ { displayName = name }
      where
        renderHandler this = do
          props <- getProps  this
          state <- readState this
          pure $ renderer $ renderFn props state


diagTreeEditorSlideEditorAnswers
  :: forall f1 f2 eff
   . Foldable f1
  => Foldable f2
  => ReactClass (Props f1 f2 eff)

diagTreeEditorSlideEditorAnswers = diagTreeEditorSlideEditorAnswersRender
