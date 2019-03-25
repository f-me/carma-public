module Component.DiagTree.Editor.SlideEditor.Resources
     ( diagTreeEditorSlideEditorResources
     ) where

import Prelude hiding (div)

import Data.Tuple (Tuple (Tuple), snd)
import Data.Foldable (class Foldable, foldl, length)
import Data.Maybe (Maybe (..))
import Data.String (joinWith)
import Data.Array (snoc)

import Effect (Effect)

import React.DOM (text, div, button, label, i)
import React.DOM.Dynamic (ul) as RDyn
import React.DOM.Props (className, _type, disabled, onClick)
import React.SyntheticEvent (preventDefault)

import React
     ( ReactClass, component, createLeafElement
     , getProps, getState, modifyState
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
       :: ItemModification Int
            { text :: String
            , file :: Maybe BackendAttachment
            }
       -> Effect Unit

   , onMoveUp   :: Int -> Effect Unit
   , onMoveDown :: Int -> Effect Unit
   }


diagTreeEditorSlideEditorResourcesRender
  :: forall f . Foldable f => ReactClass (Props f)

diagTreeEditorSlideEditorResourcesRender = defineComponent $
  \ { turnAddingOn, turnAddingOff }
    { appContext, slideId, isDisabled, resources
    , updateResource, onMoveUp, onMoveDown
    }
    { isAdding } ->

  [ label [className "control-label"] [text "Прикреплённые файлы"]

  , RDyn.ul
      [ className $ "list-group" <.> classSfx "list" ]
      let
        itemReducer lastIndex (Tuple itemIndex list) resource = go where
          go = Tuple (itemIndex + 1) $ list `snoc` itemEl props

          props =
            { appContext
            , slideId
            , key: show itemIndex
            , itemIndex: Just itemIndex
            , isDisabled
            , resource: Just resource
            , updateResource
            , onCancel: Nothing
            , onMoveUp:
                if itemIndex <= 0 then Nothing else Just onMoveUp
            , onMoveDown:
                if itemIndex >= lastIndex then Nothing else Just onMoveDown
            }
      in
        snd $ foldl (itemReducer $ length resources - 1) (Tuple 0 []) resources

  , if isAdding
       then itemEl
              { appContext
              , slideId
              , key: mempty
              , itemIndex: Nothing
              , isDisabled
              , resource: Nothing
              , updateResource
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
              , text $ (" Добавить " <> _) $
                  joinWith "/" $ map showAccusative
                    (unfoldrBoundedEnum :: Array BackendAttachmentMediaType)
              ]
  ]

  where
    name = "DiagTreeEditorSlideEditorResources"
    classSfx s = name <> "--" <> s
    wrapper = div [className $ "form-group" <.> name]
    itemEl = createLeafElement diagTreeEditorSlideEditorResource

    turnAddingHandler this isOn =
      modifyState this _ { isAdding = isOn }

    defineComponent renderFn = component name \this -> do
      let preBound =
            { turnAddingOn:
                \event -> preventDefault event *> turnAddingHandler this true

            , turnAddingOff: turnAddingHandler this false
            }

      let r = renderFn preBound

      pure
        { state: { isAdding: false }
        , render: map wrapper $ r <$> getProps this <*> getState this
        }


diagTreeEditorSlideEditorResources
  :: forall f . Foldable f => ReactClass (Props f)
diagTreeEditorSlideEditorResources = diagTreeEditorSlideEditorResourcesRender
