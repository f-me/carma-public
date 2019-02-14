module Component.DiagTree.Editor.SlideEditor.Resources
     ( diagTreeEditorSlideEditorResources
     ) where

import Prelude hiding (div)

import Data.Monoid (mempty)
import Data.Tuple (Tuple (Tuple), snd)
import Data.Foldable (class Foldable, foldl, length)
import Data.Maybe (Maybe (..))
import Data.Nullable (toNullable)
import Data.String (joinWith)
import Data.Array (snoc)

import React.DOM (text, div, button, label, i)
import React.DOM.Dynamic (ul) as RDyn
import React.DOM.Props (className, _type, disabled, onClick)

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
    { isAdding, turnAddingOn, turnAddingOff } ->

  [ label [className "control-label"] [text "Прикреплённые файлы"]

  , RDyn.ul
      [ className $ "list-group" <.> classSfx "list" ]
      let
        itemReducer lastIndex (Tuple itemIndex list) resource = go where
          go = Tuple (itemIndex + 1) $ list `snoc` itemEl props mempty

          props =
            { appContext
            , slideId
            , key: toNullable $ Just $ show itemIndex
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
       then flip itemEl mempty
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
    itemEl = createElement diagTreeEditorSlideEditorResource

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


diagTreeEditorSlideEditorResources
  :: forall f . Foldable f => ReactClass (Props f)
diagTreeEditorSlideEditorResources = diagTreeEditorSlideEditorResourcesRender
