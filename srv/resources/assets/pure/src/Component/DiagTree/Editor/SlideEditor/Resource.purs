module Component.DiagTree.Editor.SlideEditor.Resource
     ( diagTreeEditorSlideEditorResource
     ) where

import Prelude hiding (div, id)

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Aff (launchAff_)

import Data.Maybe (Maybe (..), maybe, fromMaybe, isJust, isNothing)
import Data.Either (Either (..))
import Data.Nullable (Nullable, toNullable)
import Data.Array (head)

import DOM.HTML (window) as DOM
import DOM.HTML.Window (confirm) as DOM
import React.DOM (li) as R
import React.Spaces ((!), (!.), (^), (^^), renderIn, text, empty)
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
import Component.Generic.Spinner (spinner)
import Bindings.ReactDropzone (dropzone, handle2)

import Utils.DiagTree.Editor
     ( getDiagTreeSlideResourcePath
     , eqDiagTreeSlideResource
     , uploadFile
     , dropzoneDefaultProps
     )

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlideId
     , DiagTreeSlideResource
     , DiagTreeSlideResourceAttachment (..)
     )


type Props eff =
  { appContext :: AppContext
  , key        :: Nullable String
  , slideId    :: DiagTreeSlideId
  , itemIndex  :: Maybe Int
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
         -- ^ `Nothing` means to delete a resource

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
  \ { appContext, resource, isDisabled }
    state@{ file, isEditing, isProcessing, isUploadingFailed } -> do

  case (Modern <$> file) <|> (resource <#> _.attachment) of
       Just (Legacy _) -> div $ do
         span !. "label label-warning" $ text "Внимание"
         text " Картинка хранится в базе неэффективным образом,\
              \ рекомендуется загрузить её заново."

       _ -> empty

  if not isUploadingFailed
     then empty
     else div $ do
            span !. "label label-danger" $ text "Ошибка"
            text " Произошла ошибка при попытке загрузить картинку."

  if not isProcessing
     then empty
     else spinner ^ { withLabel: Right "Загрузка…"
                    , appContext
                    }

  let imgSrc =
        let
          modern = file <#> getDiagTreeSlideResourcePath
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

  let isBlocked = isDisabled || isProcessing

  if isEditing || isNothing resource
     then editRender isBlocked resource state imgM
     else viewRender isBlocked state imgM

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
        dropzone ^^ dropzoneDefaultProps
          { disabled = isDisabled

          , onDropAccepted = toNullable $ Just $ handle2 $
              \files _ -> case head files of
                               Nothing -> pure unit
                               Just x  -> state.onFileDropped x
          }
          $ text "Нажмите для добавления картинки или перетащите её сюда"

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
         else updateResource (resource *> itemIndex) $
                Just { text: state.text, file: state.file }

    deleteHandler this _ = do
      { updateResource, itemIndex } <- getProps this

      wnd         <- DOM.window
      isConfirmed <- DOM.confirm "Вы действительно хотите удалить картинку?" wnd

      if not isConfirmed || isNothing itemIndex
         then pure unit
         else -- Coercing to not infect parent handler
              -- with DOM and CONFIRM effects.
              unsafeCoerceEff $ updateResource itemIndex Nothing

    changeTextHandler this event = do
      let newText = eventInputValue event
      transformState this _ { text = newText, isChanged = true }

    enterEditingHandler this _ =
      transformState this _ { isEditing = true }

    fileDroppedHandler this file = guardNotProcessing $ do
      { slideId } <- getProps this
      transformState this _ { isProcessing = true, isUploadingFailed = false }

      launchAff_ $
        uploadFile slideId file >>=
          liftEff <<< maybe failProcessing doneProcessing

      where
        guardNotProcessing m = do
          { isProcessing } <- readState this
          if isProcessing then pure unit else m

        failProcessing =
          transformState this _
            { isProcessing = false, isUploadingFailed = true }

        doneProcessing attachment =
          transformState this _
            { isProcessing = false
            , isUploadingFailed = false
            , isChanged = true
            , file = Just attachment
            }

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
           , isProcessing: false
           , isUploadingFailed: false
           , enterEditing: enterEditingHandler this
           , onChangeText: changeTextHandler this
           , cancelEditing: cancelEditingHandler this
           , onFileDropped: fileDroppedHandler this
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
