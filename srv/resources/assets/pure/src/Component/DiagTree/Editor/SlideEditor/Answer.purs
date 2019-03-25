module Component.DiagTree.Editor.SlideEditor.Answer
     ( diagTreeEditorSlideEditorAnswer
     ) where

import Prelude hiding (div)

import Data.Maybe (Maybe (..), maybe, fromMaybe, isJust, isNothing)
import Data.Either (Either (..), isLeft)
import Data.Nullable (toNullable)
import Data.Array (head, snoc)
import Data.String (null)

import Control.Alt ((<|>))
import Control.MonadZero (guard)

import Effect (Effect)
import Effect.Uncurried (mkEffectFn2)
import Effect.Class (liftEffect)
import Effect.Aff (launchAff_)

import Web.HTML (window) as DOM
import Web.HTML.Window (confirm) as DOM

import React.DOM
     ( text, div, div', img, span, span', button, i, p, p', h4, input, li
     , audio, video, source
     )

import React.DOM.Props
     ( className, src, role, title, disabled, _type, placeholder, value
     , onClick, onChange
     , controls
     )

import React
     ( ReactClass, component, getProps, getState, modifyState
     , createLeafElement, unsafeCreateElement
     )

import App.Store (AppContext)
import Bindings.ReactDropzone (dropzone)
import Component.Generic.Spinner (spinner)
import Component.Generic.DropDownSelect (dropDownSelect)
import Component.DiagTree.Editor.SlideEditor.Helpers (ItemModification (..))

import Utils
     ( (<.>)
     , showNominative, showGenitive, showAccusative
     , getSex, sexyShow, capitalize
     , eventInputValue
     , unfoldrBoundedEnum
     )

import Utils.DiagTree.Editor
     ( getDiagTreeSlideAttachmentPath
     , uploadFile
     , dropzoneDefaultProps
     , rejectedFilesAlert
     )

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlideId
     , DiagTreeSlideAttachment (..)
     )

import App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAttachment
     ( BackendAttachment
     , BackendAttachmentMediaType (..)
     )


type Props =
   { appContext :: AppContext
   , slideId    :: DiagTreeSlideId
   , identity   :: Maybe (Either DiagTreeSlideId Int)
   , isDisabled :: Boolean

   , answer
       :: Maybe { header     :: String
                , text       :: String
                , attachment :: Maybe DiagTreeSlideAttachment
                }

   , updateAnswer
       :: ItemModification (Either DiagTreeSlideId Int)
            { header     :: String
            , text       :: String
            , attachment :: Maybe BackendAttachment

            , isAttachmentDeleted :: Boolean
              -- ^ Makes sense only for legacy `file` field.
              --   Also it's only for previously created answers, saving
              --   legacy type of attachment is not allowed for new answers.
              --   TODO FIXME Remove this flag after removing
              --              deprecated `file` field.
            }
       -> Effect Unit

   , onCancel :: Maybe (Effect Unit)
     -- ^ Only for adding new one (when `answer` prop is `Nothing`)

   , onMoveUp   :: Maybe (Either DiagTreeSlideId Int -> Effect Unit)
     -- ^ `Maybe` indicates whether an answer could be moved up
   , onMoveDown :: Maybe (Either DiagTreeSlideId Int -> Effect Unit)
     -- ^ `Maybe` indicates whether an answer could be moved down
   }


diagTreeEditorSlideEditorAnswerRender :: ReactClass Props
diagTreeEditorSlideEditorAnswerRender = defineComponent $
  \ preBound
    { appContext, identity, answer, isDisabled, onMoveUp, onMoveDown }
    state@{ mediaType
          , attachment
          , isEditing
          , isProcessing
          , isUploadingFailed
          , isAttachmentDeleted
          } ->

  ( if isJust answer && map isLeft identity == Just true
       then mempty
       else pure $
            p' $ pure $
              span [className "label label-primary"] [text "Новый ответ"]
  )

  <>

  ( if not isUploadingFailed
       then mempty
       else pure $
            div'
              [ span [className "label label-danger"] [text "Ошибка"]
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
              , appContext
              }
  )

  <>

  let
    legacyWarnM =
      case (Modern <$> attachment) <|> (answer >>= _.attachment) of
           Just (Legacy _) -> pure $
             span
               [ className $ classSfx "deprecation-warning" ]
               [ span [className "label label-warning"] [text "Внимание"]
               , -- It can be only `ImageMediaType` in case of legacy attachment
                 text " Картинка хранится в базе неэффективным образом,\
                      \ рекомендуется загрузить её заново."
               ]

           _ -> mempty

    imgSrc = if isAttachmentDeleted then Nothing else modern <|> legacy where
      modern = attachment <#> getDiagTreeSlideAttachmentPath

      legacy =
        answer >>= _.attachment >>=
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
      filePath <- attachment <#> getDiagTreeSlideAttachmentPath

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
      filePath <- attachment <#> getDiagTreeSlideAttachmentPath

      pure $ pure $
        video
          [ className $ classSfx "video"
          , controls true
          ]
          [ source [src filePath]
          , text "Ваш браузер не поддерживает отображение видеофайлов"
          ]

    previewEls = fromMaybe mempty $ imgEls <|> audioEls <|> videoEls
    hasAttachment = isJust imgSrc || isJust (imgEls <|> audioEls <|> videoEls)
    isBlocked = isDisabled || isProcessing
  in
    if isEditing || isNothing answer
       then editRender isBlocked appContext hasAttachment
                       preBound state previewEls
       else viewRender isBlocked onMoveUp onMoveDown
                       preBound state previewEls

  where
    name = "DiagTreeEditorSlideEditorAnswer"
    classSfx s = name <> "--" <> s

    wrapper =
      li [className $ "list-group-item" <.> name] <<< pure <<<
        div [className $ "list-group-item" <.> classSfx "wrap"]

    spinnerEl        = createLeafElement spinner
    dropDownSelectEl = createLeafElement dropDownSelect
    dropzoneEl       = unsafeCreateElement dropzone

    viewRender isDisabled onMoveUp onMoveDown preBound state previewEls =
      [ h4 [className "list-group-item-heading"] [text state.header]

      , p
          [ className "list-group-item-text" ]
          $ previewEls `snoc` span' [text state.text]

      , div
          [ className $ "btn-toolbar" <.> classSfx "buttons"
          , role "toolbar"
          ]
          $

          ( if isNothing onMoveUp && isNothing onMoveDown
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
                        [ i [className "glyphicon glyphicon-arrow-down"]
                            mempty
                        ]
                    ]
          )

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
      ]

    editRender isDisabled appContext hasAttachment preBound state previewEls =
      [ div [className "form-group"] $ pure $
          input
            [ className "form-control"
            , _type "text"
            , placeholder "Ответ"
            , value state.header
            , onChange preBound.onChangeHeader
            , disabled isDisabled
            ]

      , div [className "form-group"] $ pure $
          input
            [ className "form-control"
            , _type "text"
            , placeholder "Комментарий"
            , value state.text
            , onChange preBound.onChangeText
            , disabled isDisabled
            ]

      , div
          [ className "form-group" ]
          $
          [ div' $ pure $
              dropDownSelectEl
                { appContext
                , isDisabled:
                    isDisabled || isJust state.attachment || hasAttachment
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
          ]
          <>
          previewEls
          <>
          if not hasAttachment
             then mempty
             else [ text " "
                  , button
                      [ className "btn btn-danger"
                      , disabled isDisabled
                      , onClick preBound.deleteAttachment
                      ]
                      [ i [className "glyphicon glyphicon-trash"] mempty
                      , text $ " Удалить " <> showAccusative state.mediaType
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

          , button
              [ className "btn btn-success"
              , _type "button"
              , onClick preBound.save
              , disabled $
                  isDisabled || not state.isChanged || null state.header
              ]
              [ text "Сохранить ответ" ]
          ]
      ]

    enterEditingHandler this _ =
      modifyState this _ { isEditing = true }

    cancelEditingHandler this _ = do
      { answer, onCancel } <- getProps this

      if isNothing answer
         then fromMaybe (pure unit) onCancel
         else let values = buildIntervalValues answer
               in modifyState this _
                    { header              = values.header
                    , text                = values.text
                    , attachment          = values.attachment
                    , mediaType           = values.mediaType
                    , isEditing           = false
                    , isChanged           = false
                    , isAttachmentDeleted = false
                    }

    changeTextHandler this HeaderField event = do
      x <- eventInputValue event
      modifyState this _ { header = x, isChanged = true }

    changeTextHandler this TextField event = do
      x <- eventInputValue event
      modifyState this _ { text = x, isChanged = true }

    deleteAttachmentHandler this _ = do
      { mediaType } <- getState this

      guardConfirmed mediaType $ modifyState this _
        { attachment          = Nothing
        , isAttachmentDeleted = true
        , isChanged           = true
        }

      where
        guardConfirmed mediaType m = do
          isConfirmed <- DOM.window >>= DOM.confirm
            ("Вы действительно хотите удалить " <>
              showAccusative mediaType <> "?")

          if isConfirmed then m else pure unit

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
            , isAttachmentDeleted = false
            , isChanged = true
            , attachment = Just attachment { mediaType = s.mediaType }
            }

    saveHandler this _ = do
      state@{ isChanged } <- getState this
      { isDisabled, answer, updateAnswer, identity } <- getProps this

      let existing = answer *> (Left <$> identity)
          new = Just $ Right unit

          value = { header: state.header
                  , text: state.text
                  , attachment: state.attachment
                  , isAttachmentDeleted: state.isAttachmentDeleted
                  }

          able = do guard $ not isDisabled
                    guard isChanged
                    guard $ not $ null state.header

      case able *> (existing <|> new) of
           Just (Left ident) -> updateAnswer $ ChangeItem ident value
           Just (Right _)    -> updateAnswer $ NewItem value
           Nothing           -> pure unit

    deleteHandler this _ = do
      { isDisabled, updateAnswer, identity } <- getProps this

      isConfirmed <-
        DOM.window >>= DOM.confirm "Вы действительно хотите удалить ответ?"

      case guard (not isDisabled) *> guard isConfirmed *> identity of
           Just ident -> updateAnswer $ DeleteItem ident
           _ -> pure unit

    mediaTypeSelectedHandler this =
      maybe (pure unit) \x -> modifyState this _ { mediaType = x }

    moveHandler this isUp _ = do
      { identity, onMoveUp, onMoveDown } <- getProps this

      fromMaybe (pure unit) $
        identity >>= \x -> (if isUp then onMoveUp else onMoveDown) <#> (_ $ x)

    buildIntervalValues answer = go where
      go = { header     : fromMaybe "" $ answer <#> _.header
           , text       : fromMaybe "" $ answer <#> _.text
           , mediaType  : fromMaybe ImageMediaType $ attachment <#> _.mediaType
           , attachment
           }

      attachment =
        answer <#> _.attachment >>=
          case _ of
               Just (Modern x) -> Just x
               _ -> Nothing

    eqAnswer a b =
      a.header     == b.header &&
      a.text       == b.text &&
      a.attachment == b.attachment

    defineComponent renderFn = component name \this -> do
      let preBound =
            { enterEditing: enterEditingHandler this
            , cancelEditing: cancelEditingHandler this
            , onChangeHeader: changeTextHandler this HeaderField
            , onChangeText: changeTextHandler this TextField
            , deleteAttachment: deleteAttachmentHandler this
            , onFileDropped: fileDroppedHandler this
            , onFilesRejected: rejectedFilesAlert
            , onMediaTypeSelected: mediaTypeSelectedHandler this
            , onMoveUp: moveHandler this true
            , onMoveDown: moveHandler this false
            , save: saveHandler this
            , delete: deleteHandler this
            }

      state <-
        getProps this <#> _.answer <#> buildIntervalValues <#> \values ->
          { header: values.header
          , text: values.text
          , attachment: values.attachment
          , mediaType: values.mediaType -- For attachment
          , isEditing: false
          , isChanged: false
          , isAttachmentDeleted: false
          , isProcessing: false
          , isUploadingFailed: false
          }

      let r = renderFn preBound

      pure
        { state
        , render: map wrapper $ r <$> getProps this <*> getState this

        , unsafeComponentWillReceiveProps: \nextProps -> do
            prevProps <- getProps this

            if isJust nextProps.answer == isJust prevProps.answer &&
               Just true == (eqAnswer <$> nextProps.answer <*> prevProps.answer)
               then pure unit
               else let values = buildIntervalValues nextProps.answer
                     in modifyState this _
                          { header              = values.header
                          , text                = values.text
                          , attachment          = values.attachment
                          , mediaType           = values.mediaType
                          , isEditing           = false
                          , isChanged           = false
                          , isAttachmentDeleted = false
                          }
        }


diagTreeEditorSlideEditorAnswer :: ReactClass Props
diagTreeEditorSlideEditorAnswer = diagTreeEditorSlideEditorAnswerRender


data Field = HeaderField | TextField
