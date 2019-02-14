module Component.DiagTree.Editor.SlideEditor.Resource
     ( diagTreeEditorSlideEditorResource
     ) where

import Prelude hiding (div, id)

import Data.Monoid (mempty)
import Data.Maybe (Maybe (..), maybe, fromMaybe, isJust, isNothing)
import Data.Either (Either (..))
import Data.Nullable (Nullable, toNullable)
import Data.Array (head, snoc)

import Control.Alt ((<|>))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Aff (launchAff_)
import Control.MonadZero (guard)

import DOM.HTML (window) as DOM
import DOM.HTML.Window (confirm) as DOM

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
     ( ReactClass, EventHandler
     , createClass, createElement, spec'
     , getProps, readState, transformState
     , handle
     )

import App.Store (AppContext)
import Bindings.ReactDropzone (dropzone, handle2)
import Component.Generic.Spinner (spinner)
import Component.Generic.DropDownSelect (dropDownSelect)
import Component.DiagTree.Editor.SlideEditor.Helpers (ItemModification (..))

import Utils
     ( (<.>)
     , showNominative, showGenitive, showDative, showAccusative
     , getSex, sexyShow, capitalize
     , unfoldrBoundedEnum
     , eventInputValue
     , callEventHandler
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


type Props =
  { appContext :: AppContext
  , key        :: Nullable String
  , slideId    :: DiagTreeSlideId
  , itemIndex  :: Maybe Int
  , isDisabled :: Boolean

  , resource   :: Maybe DiagTreeSlideResource
    -- ^ When resource is `Nothing` is means adding new one

  , updateResource
      :: EventHandler
           ( ItemModification Int
               { text :: String
               , file :: Maybe BackendAttachment
               } )

  , onCancel :: Maybe (EventHandler Unit)
    -- ^ Only for adding new one (when `resource` prop is `Nothing`)

  , onMoveUp   :: Maybe (EventHandler Int)
  , onMoveDown :: Maybe (EventHandler Int)
  }


diagTreeEditorSlideEditorResourceRender :: ReactClass Props
diagTreeEditorSlideEditorResourceRender = createClass $ spec $
  \ { appContext, resource, isDisabled, onMoveUp, onMoveDown }
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
            flip spinnerEl mempty
              { withLabel: Right "Загрузка…"
              , appContext
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
        flip img mempty
          [ className $ classSfx "image"
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
          [ source [src filePath] mempty
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
          [ source [src filePath] mempty
          , text "Ваш браузер не поддерживает отображение видеофайлов"
          ]

    previewEls = fromMaybe mempty $ imgEls <|> audioEls <|> videoEls
    isBlocked = isDisabled || isProcessing
  in
    if isEditing || isNothing resource
       then editRender isBlocked appContext resource state previewEls
       else viewRender isBlocked onMoveUp onMoveDown state previewEls

  where
    name = "DiagTreeEditorSlideEditorResource"
    classSfx s = name <> "--" <> s

    wrapper =
      li [className $ "list-group-item" <.> name] <<< pure <<<
        div [className $ "list-group-item" <.> classSfx "wrap"]

    spinnerEl        = createElement spinner
    dropDownSelectEl = createElement dropDownSelect
    dropzoneEl       = createElement dropzone

    viewRender isDisabled onMoveUp onMoveDown state previewEls = go where
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
                      , onClick state.onMoveUp
                      ]
                      [ i [className "glyphicon glyphicon-arrow-up"] mempty ]

                  , button
                      [ className "btn btn-default"
                      , title "Опустить вниз"
                      , disabled $ isNothing onMoveDown
                      , onClick state.onMoveDown
                      ]
                      [ i [className "glyphicon glyphicon-arrow-down"] mempty ]
                  ]

          `snoc`

          button
            [ className "btn btn-success"
            , title "Редактировать"
            , disabled isDisabled
            , onClick state.enterEditing
            ]
            [ i [className "glyphicon glyphicon-pencil"] mempty ]

          `snoc`

          button
            [ className "btn btn-danger"
            , title "Удалить"
            , disabled isDisabled
            , onClick state.delete
            ]
            [ i [className "glyphicon glyphicon-trash"] mempty ]

    editRender isDisabled appContext resource state previewEls =
      [ div
          [ className "form-group" ]
          [ div' $ pure $
              flip dropDownSelectEl mempty
                { appContext
                , isDisabled: isDisabled || isJust state.file || isJust resource
                , variants:
                    (unfoldrBoundedEnum :: Array BackendAttachmentMediaType)
                , selected: Just state.mediaType
                , variantView: showNominative >>> capitalize
                , onSelected: Just state.onMediaTypeSelected
                , placeholder: Just "Тип прикрепляемого файла"
                , notSelectedTitle: Nothing
                }

          , dropzoneEl (dropzoneDefaultProps state.mediaType)
              { disabled = isDisabled

              , onDropAccepted = toNullable $ Just $ handle2 $
                  \files _ -> maybe (pure unit) state.onFileDropped $ head files

              , onDropRejected = toNullable $ Just $ handle2 $
                  \files _ -> state.onFilesRejected files
              }
              [ text $
                  "Нажмите для добавления " <> showGenitive state.mediaType <>
                  " или перетащите " <>
                  sexyShow "его" "файл" "её" (getSex state.mediaType) <> " сюда"
              ]

          , span [className $ classSfx "edit-image-wrap"] previewEls

          , flip input mempty
              [ className "form-control"
              , _type "text"
              , placeholder $ "Подпись к " <> showDative state.mediaType
              , value state.text
              , onChange state.onChangeText
              , disabled isDisabled
              ]
          ]

      , div
          [ className "btn-toolbar" ]
          [ button
              [ className "btn btn-default"
              , _type "button"
              , onClick state.cancelEditing
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
                , onClick state.save
                , disabled isSaveBlocked
                ]
                [ text "Сохранить" ]
          ]
      ]

    cancelEditingHandler this _ = do
      { resource, onCancel } <- getProps this

      if isNothing resource

         then case onCancel of
                   Nothing -> pure unit
                   Just f  -> callEventHandler f unit

         else let values = buildIntervalValues resource
               in transformState this _
                    { text      = values.text
                    , file      = values.file
                    , isEditing = false
                    , isChanged = false
                    }

    saveHandler this _ = do
      state@{ isChanged } <- readState this
      { isDisabled, resource, updateResource, itemIndex } <- getProps this

      let existing = resource *> (Left <$> itemIndex)
          new = Right <$> state.file -- To create new one `file` must be set
          value = { text: state.text, file: state.file }

      case guard (not isDisabled) *> guard isChanged *> (existing <|> new) of
           Just (Left idx) ->
             callEventHandler updateResource $ ChangeItem idx value
           Just (Right _) ->
             callEventHandler updateResource $ NewItem value
           Nothing -> pure unit

    deleteHandler this _ = do
      { isDisabled, updateResource, itemIndex } <- getProps this
      { mediaType } <- readState this

      isConfirmed <- DOM.window >>= DOM.confirm
        ("Вы действительно хотите удалить " <> showAccusative mediaType <> "?")

      case guard (not isDisabled) *> guard isConfirmed *> itemIndex of
           Just idx -> callEventHandler updateResource $ DeleteItem idx
           _ -> pure unit

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
          transformState this \s -> s
            { isProcessing = false
            , isUploadingFailed = false
            , isChanged = true
            , file = Just attachment { mediaType = s.mediaType }
            }

    mediaTypeSelectedHandler this =
      maybe (pure unit) \x -> transformState this _ { mediaType = x }

    moveHandler this isUp _ = do
      { itemIndex, onMoveUp, onMoveDown } <- getProps this

      fromMaybe (pure unit) $
        itemIndex >>= \x ->
          (if isUp then onMoveUp else onMoveDown) <#>
            \f -> callEventHandler f x

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

    getInitialState this = do
      { resource } <- getProps this
      let values = buildIntervalValues resource

      pure { text: values.text
           , file: values.file
           , mediaType: values.mediaType
           , isEditing: false
           , isChanged: false
           , isProcessing: false
           , isUploadingFailed: false
           , enterEditing: enterEditingHandler this
           , onChangeText: changeTextHandler this
           , cancelEditing: cancelEditingHandler this
           , onFileDropped: fileDroppedHandler this
           , onFilesRejected: rejectedFilesAlert
           , onMediaTypeSelected: handle $ mediaTypeSelectedHandler this
           , onMoveUp: moveHandler this true
           , onMoveDown: moveHandler this false
           , save: saveHandler this
           , delete: deleteHandler this
           }

    spec renderFn = go where
      renderHandler this =
        map wrapper $ renderFn <$> getProps this <*> readState this

      go
        = spec' getInitialState renderHandler # _
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
                          , mediaType = values.mediaType
                          , isEditing = false
                          , isChanged = false
                          }
        }


diagTreeEditorSlideEditorResource :: ReactClass Props
diagTreeEditorSlideEditorResource = diagTreeEditorSlideEditorResourceRender
