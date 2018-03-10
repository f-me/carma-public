module Component.DiagTree.Editor.SlideEditor.Resource
     ( diagTreeEditorSlideEditorResource
     ) where

import Prelude hiding (div)

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)

import Data.Maybe (Maybe (..), fromMaybe, isJust, isNothing)

import DOM.HTML (window) as DOM
import DOM.HTML.Window (confirm) as DOM
import React.DOM (li) as R
import React.Spaces ((!), (!.), renderIn, text, empty)
import React.Spaces.DOM (div, img, span, button, i, input)

import React.DOM.Props
     ( className, role, src, title, placeholder, _type, value, disabled
     , onClick, onChange
     )

import React
     ( ReactClass, ReactProps, ReactState, ReactRefs, ReadWrite, ReadOnly
     , createClass, spec'
     , getProps, readState, transformState
     )

import Utils ((<.>), eventInputValue)
import App.Store (AppContext)

import Utils.DiagTree.Editor
     ( getDiagTreeSlideResourcePath
     , eqDiagTreeSlideResource
     )

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlideResource
     , DiagTreeSlideResourceAttachment (..)
     )


type Props eff =
  { appContext :: AppContext
  , key        :: String
  , itemIndex  :: Int
  , isDisabled :: Boolean

  , resource   :: Maybe DiagTreeSlideResource
    -- ^ When resource is `Nothing` is means adding new one

  , updateResource
      :: Maybe Int -- ^ `Nothing` to add new one

      -> Maybe { text :: String
               , file :: Maybe { id       :: Int
                               , hash     :: String
                               , filename :: String
                               }
               }

      -> Eff ( props :: ReactProps
             , state :: ReactState ReadWrite
             , refs  :: ReactRefs  ReadOnly
             | eff
             ) Unit

  , onCancel
      :: Maybe ( Eff ( props :: ReactProps
                     , state :: ReactState ReadWrite
                     , refs  :: ReactRefs  ReadOnly
                     | eff
                     ) Unit )
      -- ^ Only for adding new one (when `resource` is `Nothing`)
  }


diagTreeEditorSlideEditorResourceRender :: forall eff. ReactClass (Props eff)
diagTreeEditorSlideEditorResourceRender = createClass $ spec $
  \ { resource, isDisabled } state@{ file, isEditing } -> do

  case resource <#> _.attachment of
       Just (Legacy _) -> div $ do
         span !. "label label-warning" $ text "Внимание"
         text " Картинка хранится в базе неэффективным образом,\
              \ рекомендуется загрузить её заново."

       _ -> empty

  let imgSrc =
        let
          modern = file <#> _.filename >>> getDiagTreeSlideResourcePath

          legacy = resource <#> _.attachment >>= case _ of
                                                      Legacy x -> Just x
                                                      Modern _ -> Nothing
        in
          modern <|> legacy

      imgM =
        case imgSrc of
             Nothing -> empty
             Just x  -> img !. classSfx "image"
                            ! role "presentation"
                            ! src x

  if isEditing || isNothing resource
     then editRender isDisabled resource state imgM
     else viewRender isDisabled state imgM

  where
    name = "DiagTreeEditorSlideEditorResource"
    classSfx s = name <> "--" <> s
    wrapper = R.li [className $ "list-group-item" <.> name]

    viewRender isDisabled state imgM = do
      imgM
      span $ text state.text

      div !. "btn-toolbar" <.> classSfx "buttons" ! role "toolbar" $ do

        button !. "btn btn-success"
               ! title "Редактировать"
               ! onClick state.enterEditing
               ! disabled isDisabled
               $ i !. "glyphicon glyphicon-pencil" $ empty

        button !. "btn btn-danger"
               ! title "Удалить"
               ! onClick state.delete
               ! disabled isDisabled
               $ i !. "glyphicon glyphicon-trash" $ empty

    editRender isDisabled resource state imgM = do
      div !. "form-group" $ do
        div $ text "TODO drop zone"
        imgM

        input !. "form-control"
              ! _type "text"
              ! placeholder "Подпись к картинке"
              ! value state.text
              ! onChange state.onChangeText
              ! disabled isDisabled

      div !. "btn-toolbar" $ do
        button !. "btn btn-default"
               ! _type "button"
               ! onClick state.cancelEditing
               ! disabled isDisabled
               $ text "Отменить"

        let isSaveBlocked =
              isDisabled || not state.isChanged ||
              (isNothing resource && isNothing state.file)

        button !. "btn btn-success"
               ! _type "button"
               ! onClick state.save
               ! disabled isSaveBlocked
               $ text "Сохранить"

    cancelEditingHandler this _ = do
      { resource, onCancel } <- getProps this

      if isNothing resource

         then case onCancel of
                   Nothing -> pure unit
                   Just f  -> f

         else let values = buildIntervalValues resource
               in transformState this _
                    { text      = values.text
                    , file      = values.file
                    , isEditing = false
                    , isChanged = false
                    }

    saveHandler this _ = do
      state@{ isChanged } <- readState this
      { resource, updateResource, itemIndex } <- getProps this

      if not isChanged && not (isNothing resource && isNothing state.file)
         then pure unit
         else updateResource (resource <#> const itemIndex) $
                Just { text: state.text, file: state.file }

    deleteHandler this _ = do
      { updateResource, itemIndex } <- getProps this

      wnd         <- DOM.window
      isConfirmed <- DOM.confirm "Вы действительно хотите удалить картинку?" wnd

      if not isConfirmed
         then pure unit
         else -- Coercing to not infect parent handler
              -- with DOM and CONFIRM effects.
              unsafeCoerceEff $ updateResource (Just itemIndex) Nothing

    changeTextHandler this event = do
      let newText = eventInputValue event
      transformState this _ { text = newText, isChanged = true }

    enterEditingHandler this _ =
      transformState this _ { isEditing = true }

    buildIntervalValues
      :: Maybe DiagTreeSlideResource
      -> { text :: String
         , file :: Maybe { id :: Int, hash :: String, filename :: String }
         }

    buildIntervalValues resource =
      { text: fromMaybe "" $ resource <#> _.text

      , file:
          resource <#> _.attachment >>=
            case _ of
                 Modern x -> Just x
                 Legacy _ -> Nothing
      }

    getInitialState this = do
      { appContext, resource } <- getProps this
      let values = buildIntervalValues resource

      pure { text: values.text
           , file: values.file
           , isEditing: false
           , isChanged: false
           , enterEditing: enterEditingHandler this
           , onChangeText: changeTextHandler this
           , cancelEditing: cancelEditingHandler this
           , save: saveHandler this
           , delete: deleteHandler this
           }

    spec renderFn =
      spec' getInitialState renderHandler # _
        { displayName = name

        , componentWillReceiveProps = \this nextProps -> do
            prevProps <- getProps this

            if isJust nextProps.resource == isJust prevProps.resource &&
               Just true == (eqDiagTreeSlideResource <$> nextProps.resource
                                                     <*> prevProps.resource)
               then pure unit
               else let values = buildIntervalValues nextProps.resource
                     in transformState this _
                          { text      = values.text
                          , file      = values.file
                          , isEditing = false
                          , isChanged = false
                          }
        }

      where
        renderHandler this = do
          props <- getProps  this
          state <- readState this
          pure $ renderIn wrapper $ renderFn props state


diagTreeEditorSlideEditorResource :: forall eff. ReactClass (Props eff)
diagTreeEditorSlideEditorResource = diagTreeEditorSlideEditorResourceRender
