module Component.DiagTree.Editor.SlideEditor.Resource
     ( diagTreeEditorSlideEditorResource
     ) where

import Prelude hiding (div)

import Data.Maybe (Maybe (..), maybe, fromMaybe, isJust, isNothing)
import Data.Either (Either (..))
import Data.Nullable (toNullable)
import Data.Array (head, snoc)

import Control.Alt ((<|>))
import Control.MonadZero (guard)

import Effect (Effect)
import Effect.Uncurried (mkEffectFn2)
import Effect.Class (liftEffect)
import Effect.Aff (launchAff_)

import Web.HTML (window) as DOM
import Web.HTML.Window (confirm) as DOM

import React.DOM
     ( text, div, div', img, span, span', button, i, input, p', li
     , audio, video, source
     )

import React.DOM.Props
     ( className, role, src, title, placeholder, _type, value, disabled
     , onClick, onChange
     , controls
     )

import React
     ( ReactClass, component, createLeafElement, unsafeCreateElement
     , getProps, getState, modifyState
     )

import App.Store (Store)
import Bindings.ReactDropzone (dropzone)
import Component.Generic.Spinner (spinner)
import Component.Generic.DropDownSelect (dropDownSelect)
import Component.DiagTree.Editor.SlideEditor.Helpers (ItemModification (..))

import Utils
     ( (<.>)
     , showNominative, showGenitive, showDative, showAccusative
     , getSex, sexyShow, capitalize
     , unfoldrBoundedEnum
     , eventInputValue
     )

import Utils.DiagTree.Editor
     ( getDiagTreeSlideAttachmentPath
     , eqDiagTreeSlideResource
     , uploadFile
     , dropzoneDefaultProps
     , rejectedFilesAlert
     )

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlideId
     , DiagTreeSlideResource
     , DiagTreeSlideAttachment (..)
     )

import App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAttachment
     ( BackendAttachment
     , BackendAttachmentMediaType (..)
     )


type Props state action =
   { store      :: Store state action
   , slideId    :: DiagTreeSlideId
   , itemIndex  :: Maybe Int
   , isDisabled :: Boolean

   , resource   :: Maybe DiagTreeSlideResource
     -- ^ When resource is `Nothing` is means adding new one

   , updateResource
       :: ItemModification Int
            { text :: String
            , file :: Maybe BackendAttachment
            }
       -> Effect Unit

   , onCancel :: Maybe (Effect Unit)
     -- ^ Only for adding new one (when `resource` prop is `Nothing`)

   , onMoveUp   :: Maybe (Int -> Effect Unit)
     -- ^ `Maybe` indicates whether a resource could be moved up
   , onMoveDown :: Maybe (Int -> Effect Unit)
     -- ^ `Maybe` indicates whether a resource could be moved down
   }


diagTreeEditorSlideEditorResourceRender
  :: forall state action. ReactClass (Props state action)

diagTreeEditorSlideEditorResourceRender = defineComponent $
  \ preBound
    { store, resource, isDisabled, onMoveUp, onMoveDown }
    state@{ file, mediaType, isEditing, isProcessing, isUploadingFailed } ->

  case resource of
       Just _  -> mempty
       Nothing -> pure $
         p' $ pure $
           span [className "label label-primary"] $ pure $
             text $
               "Нов" <> sexyShow "ый" "ое" "ая" (getSex mediaType) <> " " <>
               showNominative mediaType

  <>

  ( if not isUploadingFailed
       then mempty
       else pure $
            div' [ span [className "label label-danger"] [text "Ошибка"]
                 , text $
                     " Произошла ошибка при попытке загрузить " <>
                     showAccusative mediaType <> "."
                 ]
  )

  <>

  ( if not isProcessing
       then mempty
       else pure $
            spinnerEl
              { withLabel: Right "Загрузка…"
              , store
              }
  )

  <>

  let
    legacyWarnM =
      case (Modern <$> file) <|> (resource <#> _.attachment) of
           Just (Legacy _) -> pure $
             span
               [ className $ classSfx "deprecation-warning" ]
               [ span [className "label label-warning"] [text "Внимание"]

               , -- It can be only `ImageMediaType` in case of legacy attachment
                 text " Картинка хранится в базе неэффективным образом,\
                      \ рекомендуется загрузить её заново."
               ]

           _ -> mempty

    imgSrc = modern <|> legacy where
      modern = file <#> getDiagTreeSlideAttachmentPath

      legacy =
        resource <#> _.attachment >>=
          case _ of
               Legacy x -> Just x
               Modern _ -> Nothing

    imgEls = do
      guard $ mediaType == ImageMediaType
      x <- imgSrc

      pure $
        legacyWarnM `snoc`
        img [ className $ classSfx "image"
            , role "presentation"
            , src x
            ]

    audioEls = do
      guard $ mediaType == AudioMediaType
      filePath <- file <#> getDiagTreeSlideAttachmentPath

      pure $ pure $
        audio
          [ className $ classSfx "audio"
          , controls true
          ]
          [ source [src filePath]
          , text "Ваш браузер не поддерживает отображение аудиофайлов"
          ]

    videoEls = do
      guard $ mediaType == VideoMediaType
      filePath <- file <#> getDiagTreeSlideAttachmentPath

      pure $ pure $
        video
          [ className $ classSfx "video"
          , controls true
          ]
          [ source [src filePath]
          , text "Ваш браузер не поддерживает отображение видеофайлов"
          ]

    previewEls = fromMaybe mempty $ imgEls <|> audioEls <|> videoEls
    isBlocked = isDisabled || isProcessing
  in
    if isEditing || isNothing resource
       then editRender isBlocked store    resource   preBound state previewEls
       else viewRender isBlocked onMoveUp onMoveDown preBound state previewEls

  where
    name = "DiagTreeEditorSlideEditorResource"
    classSfx s = name <> "--" <> s

    wrapper =
      li [className $ "list-group-item" <.> name] <<< pure <<<
        div [className $ "list-group-item" <.> classSfx "wrap"]

    spinnerEl        = createLeafElement spinner
    dropDownSelectEl = createLeafElement dropDownSelect
    dropzoneEl       = unsafeCreateElement dropzone

    viewRender isDisabled onMoveUp onMoveDown
               preBound state previewEls = go where

      go = previewEls `snoc` span' [text state.text] `snoc` toolbar

      toolbar =
        div
          [ className $ "btn-toolbar" <.> classSfx "buttons"
          , role "toolbar"
          ] $

          if isNothing onMoveUp && isNothing onMoveDown
             then mempty
             else [ button
                      [ className "btn btn-default"
                      , title "Поднять вверх"
                      , disabled $ isNothing onMoveUp
                      , onClick preBound.onMoveUp
                      ]
                      [ i [className "glyphicon glyphicon-arrow-up"] mempty ]

                  , button
                      [ className "btn btn-default"
                      , title "Опустить вниз"
                      , disabled $ isNothing onMoveDown
                      , onClick preBound.onMoveDown
                      ]
                      [ i [className "glyphicon glyphicon-arrow-down"] mempty ]
                  ]

          `snoc`

          button
            [ className "btn btn-success"
            , title "Редактировать"
            , disabled isDisabled
            , onClick preBound.enterEditing
            ]
            [ i [className "glyphicon glyphicon-pencil"] mempty ]

          `snoc`

          button
            [ className "btn btn-danger"
            , title "Удалить"
            , disabled isDisabled
            , onClick preBound.delete
            ]
            [ i [className "glyphicon glyphicon-trash"] mempty ]

    editRender isDisabled store resource preBound state previewEls =
      [ div
          [ className "form-group" ]
          [ div' $ pure $
              dropDownSelectEl
                { store
                , isDisabled: isDisabled || isJust state.file || isJust resource
                , variants:
                    (unfoldrBoundedEnum :: Array BackendAttachmentMediaType)
                , selected: Just state.mediaType
                , variantView: showNominative >>> capitalize
                , onSelected: Just preBound.onMediaTypeSelected
                , placeholder: Just "Тип прикрепляемого файла"
                , notSelectedTitle: Nothing
                }

          , dropzoneEl (dropzoneDefaultProps state.mediaType)
              { disabled = isDisabled

              , onDropAccepted = toNullable $ Just $ mkEffectFn2 $
                  \files _ -> maybe (pure unit)
                                    preBound.onFileDropped
                                    (head files)

              , onDropRejected = toNullable $ Just $ mkEffectFn2 $
                  \files _ -> preBound.onFilesRejected files
              }
              [ text $
                  "Нажмите для добавления " <> showGenitive state.mediaType <>
                  " или перетащите " <>
                  sexyShow "его" "файл" "её" (getSex state.mediaType) <> " сюда"
              ]

          , span [className $ classSfx "edit-image-wrap"] previewEls

          , input
              [ className "form-control"
              , _type "text"
              , placeholder $ "Подпись к " <> showDative state.mediaType
              , value state.text
              , onChange preBound.onChangeText
              , disabled isDisabled
              ]
          ]

      , div
          [ className "btn-toolbar" ]
          [ button
              [ className "btn btn-default"
              , _type "button"
              , onClick preBound.cancelEditing
              , disabled isDisabled
              ]
              [ text "Отменить" ]

          , let
              isSaveBlocked =
                isDisabled || not state.isChanged ||
                (isNothing resource && isNothing state.file)
            in
              button
                [ className "btn btn-success"
                , _type "button"
                , onClick preBound.save
                , disabled isSaveBlocked
                ]
                [ text "Сохранить" ]
          ]
      ]

    cancelEditingHandler this _ = do
      { resource, onCancel } <- getProps this

      if isNothing resource
         then fromMaybe (pure unit) onCancel
         else let values = buildIntervalValues resource
               in modifyState this _
                    { text      = values.text
                    , file      = values.file
                    , isEditing = false
                    , isChanged = false
                    }

    saveHandler this _ = do
      state@{ isChanged } <- getState this
      { isDisabled, resource, updateResource, itemIndex } <- getProps this

      let existing = resource *> (Left <$> itemIndex)
          new = Right <$> state.file -- To create new one `file` must be set
          value = { text: state.text, file: state.file }

      case guard (not isDisabled) *> guard isChanged *> (existing <|> new) of
           Just (Left idx) ->
             updateResource $ ChangeItem idx value
           Just (Right _) ->
             updateResource $ NewItem value
           Nothing -> pure unit

    deleteHandler this _ = do
      { isDisabled, updateResource, itemIndex } <- getProps this
      { mediaType } <- getState this

      isConfirmed <- DOM.window >>= DOM.confirm
        ("Вы действительно хотите удалить " <> showAccusative mediaType <> "?")

      case guard (not isDisabled) *> guard isConfirmed *> itemIndex of
           Just idx -> updateResource $ DeleteItem idx
           _ -> pure unit

    changeTextHandler this event = do
      newText <- eventInputValue event
      modifyState this _ { text = newText, isChanged = true }

    enterEditingHandler this _ =
      modifyState this _ { isEditing = true }

    fileDroppedHandler this file = guardNotProcessing $ do
      { slideId } <- getProps this
      modifyState this _ { isProcessing = true, isUploadingFailed = false }

      launchAff_ $
        uploadFile slideId file >>=
          liftEffect <<< maybe failProcessing doneProcessing

      where
        guardNotProcessing m = do
          { isProcessing } <- getState this
          if isProcessing then pure unit else m

        failProcessing =
          modifyState this _
            { isProcessing = false, isUploadingFailed = true }

        doneProcessing attachment =
          modifyState this \s -> s
            { isProcessing = false
            , isUploadingFailed = false
            , isChanged = true
            , file = Just attachment { mediaType = s.mediaType }
            }

    mediaTypeSelectedHandler this =
      maybe (pure unit) \x -> modifyState this _ { mediaType = x }

    moveHandler this isUp _ = do
      { itemIndex, onMoveUp, onMoveDown } <- getProps this

      fromMaybe (pure unit) $
        itemIndex >>= \x -> (if isUp then onMoveUp else onMoveDown) <#> (_ $ x)

    buildIntervalValues
      :: Maybe DiagTreeSlideResource
      -> { text :: String
         , mediaType :: BackendAttachmentMediaType

         , file :: Maybe { id        :: Int
                         , hash      :: String
                         , filename  :: String
                         , mediaType :: BackendAttachmentMediaType
                         }
         }

    buildIntervalValues resource = go where
      go =
         { text: fromMaybe "" $ resource <#> _.text
         , mediaType: fromMaybe ImageMediaType $ file <#> _.mediaType
         , file
         }

      file =
        resource <#> _.attachment >>=
          case _ of
               Modern x -> Just x
               Legacy _ -> Nothing

    defineComponent renderFn = component name \this -> do
      let preBound =
            { enterEditing: enterEditingHandler this
            , onChangeText: changeTextHandler this
            , cancelEditing: cancelEditingHandler this
            , onFileDropped: fileDroppedHandler this
            , onFilesRejected: rejectedFilesAlert
            , onMediaTypeSelected: mediaTypeSelectedHandler this
            , onMoveUp: moveHandler this true
            , onMoveDown: moveHandler this false
            , save: saveHandler this
            , delete: deleteHandler this
            }

      state <-
        getProps this <#> _.resource <#> buildIntervalValues <#> \values ->
          { text: values.text
          , file: values.file
          , mediaType: values.mediaType
          , isEditing: false
          , isChanged: false
          , isProcessing: false
          , isUploadingFailed: false
          }

      let r = renderFn preBound

      pure
        { state
        , render: map wrapper $ r <$> getProps this <*> getState this

        , unsafeComponentWillReceiveProps: \nextProps -> do
            prevProps <- getProps this

            if isJust nextProps.resource == isJust prevProps.resource &&
               Just true == (eqDiagTreeSlideResource <$> nextProps.resource
                                                     <*> prevProps.resource)
               then pure unit
               else let values = buildIntervalValues nextProps.resource
                     in modifyState this _
                          { text      = values.text
                          , file      = values.file
                          , mediaType = values.mediaType
                          , isEditing = false
                          , isChanged = false
                          }
        }


diagTreeEditorSlideEditorResource
  :: forall state action. ReactClass (Props state action)

diagTreeEditorSlideEditorResource = diagTreeEditorSlideEditorResourceRender
