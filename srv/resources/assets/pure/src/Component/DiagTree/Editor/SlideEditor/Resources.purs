module Component.DiagTree.Editor.SlideEditor.Resources
     ( diagTreeEditorSlideEditorResources
     ) where

import Prelude hiding (div)

import Data.Tuple (Tuple (Tuple), snd)
import Data.Foldable (class Foldable, foldl, length)
import Data.Maybe (Maybe (..))
import Data.Nullable (toNullable)
import Data.String (joinWith)
import Data.Array (snoc)

import React.DOM (div) as R
import React.DOM.Props (className, _type, disabled, onClick)
import React.Spaces ((!), (!.), renderIn, text, empty, elements, element)
import React.Spaces.DOM (button, label, i)
import React.Spaces.DOM.Dynamic (ul) as SDyn

import React
     ( ReactClass, EventHandler
     , createClass, spec', createElement
     , getProps, readState, transformState
     , handle
     )

import Utils ((<.>), unfoldrBoundedEnum, showAccusative)
import App.Store (AppContext)

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlideId
     , DiagTreeSlideResource
     )

import App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAttachment
     ( BackendAttachment
     , BackendAttachmentMediaType
     )

import Component.DiagTree.Editor.SlideEditor.Helpers (ItemModification)

import Component.DiagTree.Editor.SlideEditor.Resource
     ( diagTreeEditorSlideEditorResource
     )


type Props f =
  { appContext :: AppContext
  , slideId    :: DiagTreeSlideId
  , isDisabled :: Boolean
  , resources  :: f DiagTreeSlideResource

  , updateResource -- See item component for details
      :: EventHandler
           ( ItemModification Int
               { text :: String
               , file :: Maybe BackendAttachment
               } )

  , onMoveUp   :: EventHandler Int
  , onMoveDown :: EventHandler Int
  }


diagTreeEditorSlideEditorResourcesRender
  :: forall f . Foldable f => ReactClass (Props f)

diagTreeEditorSlideEditorResourcesRender = createClass $ spec $
  \ { appContext, slideId, isDisabled, resources
    , updateResource, onMoveUp, onMoveDown
    }
    { isAdding, turnAddingOn, turnAddingOff } -> do

  label !. "control-label" $ text "Прикреплённые файлы"

  SDyn.ul !. "list-group" <.> classSfx "list" $

    let itemReducer lastIndex (Tuple itemIndex list) resource =
          Tuple (itemIndex + 1) $ list `snoc`

            let props = { appContext
                        , slideId
                        , key: toNullable $ Just $ show itemIndex
                        , itemIndex: Just itemIndex
                        , isDisabled
                        , resource: Just resource
                        , updateResource
                        , onCancel: Nothing

                        , onMoveUp:
                            if itemIndex <= 0
                               then Nothing
                               else Just onMoveUp

                        , onMoveDown:
                            if itemIndex >= lastIndex
                               then Nothing
                               else Just onMoveDown
                        }

             in itemEl props []

     in elements $ snd $
          foldl (itemReducer $ length resources - 1) (Tuple 0 []) resources

  if isAdding
     then element $
            itemEl
              { appContext
              , slideId
              , key: toNullable Nothing
              , itemIndex: Nothing
              , isDisabled
              , resource: Nothing
              , updateResource
              , onCancel: Just turnAddingOff
              , onMoveUp: Nothing
              , onMoveDown: Nothing
              } []

     else button !. "btn btn-default" <.> classSfx "add-button"
                 ! _type "button"
                 ! onClick turnAddingOn
                 ! disabled isDisabled
                 $ do

            i !. "glyphicon glyphicon-plus" $ empty
            text $ (" Добавить " <> _) $
              joinWith "/" $ map showAccusative
                (unfoldrBoundedEnum :: Array BackendAttachmentMediaType)

  where
    name = "DiagTreeEditorSlideEditorResources"
    classSfx s = name <> "--" <> s
    wrapper = R.div [className $ "form-group" <.> name]
    renderer = renderIn wrapper
    itemEl = createElement diagTreeEditorSlideEditorResource

    turnAddingHandler this isOn =
      transformState this _ { isAdding = isOn }

    getInitialState this = pure
      { isAdding: false
      , turnAddingOn: const $ turnAddingHandler this true
      , turnAddingOff: let f = turnAddingHandler this false in handle \unit -> f
      }

    spec renderFn =
      spec' getInitialState renderHandler # _ { displayName = name }
      where
        renderHandler this = do
          props <- getProps  this
          state <- readState this
          pure $ renderer $ renderFn props state


diagTreeEditorSlideEditorResources
  :: forall f . Foldable f => ReactClass (Props f)
diagTreeEditorSlideEditorResources = diagTreeEditorSlideEditorResourcesRender
