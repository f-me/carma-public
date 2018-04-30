module Component.DiagTree.Editor.SlideEditor.Resource
     ( diagTreeEditorSlideEditorResource
     ) where

import Prelude hiding (div, id)

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Aff (launchAff_)
import Control.MonadZero (guard)

import Data.Maybe (Maybe (..), maybe, fromMaybe, isJust, isNothing)
import Data.Either (Either (..))
import Data.Nullable (Nullable, toNullable)
import Data.Array (head)

import DOM.HTML (window) as DOM
import DOM.HTML.Window (confirm) as DOM
import React.DOM (text, li) as R
import React.Spaces ((!), (!.), renderIn, element, text, empty)

import React.Spaces.DOM
     ( div, img, span, button, i, input, p
     , audio, video, source
     )

import React.DOM.Props
     ( className, role, src, title, placeholder, _type, value, disabled
     , onClick, onChange
     , controls
     )

import React
     ( ReactClass, ReactProps, ReactState, ReactRefs, ReadWrite, ReadOnly
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


type MoveHandler eff = Int -> ParentHandler eff

type ParentHandler eff =
  Eff ( props :: ReactProps
      , state :: ReactState ReadWrite
      , refs  :: ReactRefs  ReadOnly
      | eff
      ) Unit

type Props eff =
  { appContext :: AppContext
  , key        :: Nullable String
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

      -> ParentHandler eff

  , onCancel :: Maybe (ParentHandler eff)
    -- ^ Only for adding new one (when `resource` prop is `Nothing`)

  , onMoveUp   :: Maybe (MoveHandler eff)
  , onMoveDown :: Maybe (MoveHandler eff)
  }


diagTreeEditorSlideEditorResourceRender :: forall eff. ReactClass (Props eff)
diagTreeEditorSlideEditorResourceRender = createClass $ spec $
  \ { appContext, resource, isDisabled, onMoveUp, onMoveDown }
    state@{ file, mediaType, isEditing, isProcessing, isUploadingFailed } -> do

  case resource of
       Nothing ->
         p $ span !. "label label-primary" $ text $
           "Нов" <> sexyShow "ый" "ое" "ая" (getSex mediaType) <> " " <>
           showNominative mediaType

       _ -> empty

  if not isUploadingFailed
     then empty
     else div $ do
            span !. "label label-danger" $ text "Ошибка"
            text $
              " Произошла ошибка при попытке загрузить " <>
              showAccusative mediaType <> "."

  if not isProcessing
     then empty
     else element $
            spinnerEl
              { withLabel: Right "Загрузка…"
              , appContext
              } []

  let legacyWarnM =
        case (Modern <$> file) <|> (resource <#> _.attachment) of
             Just (Legacy _) -> span !. classSfx "deprecation-warning" $ do
               span !. "label label-warning" $ text "Внимание"
               -- It can be only `ImageMediaType` in case of legacy attachment
               text " Картинка хранится в базе неэффективным образом,\
                    \ рекомендуется загрузить её заново."

             _ -> empty

      imgSrc =
        let
          modern = file <#> getDiagTreeSlideAttachmentPath
          legacy = resource <#> _.attachment >>= case _ of
                                                      Legacy x -> Just x
                                                      Modern _ -> Nothing
        in
          modern <|> legacy

      imgM = do
        guard $ mediaType == ImageMediaType
        x <- imgSrc

        pure $ do
          legacyWarnM
          img !. classSfx "image" ! role "presentation" ! src x

      audioM = do
        guard $ mediaType == AudioMediaType
        filePath <- file <#> getDiagTreeSlideAttachmentPath

        pure $
          audio !. classSfx "audio" ! controls true $ do
            source ! src filePath $ empty
            text "Ваш браузер не поддерживает отображение аудиофайлов"

      videoM = do
        guard $ mediaType == VideoMediaType
        filePath <- file <#> getDiagTreeSlideAttachmentPath

        pure $ do
          video !. classSfx "video" ! controls true $ do
            source ! src filePath $ empty
            text "Ваш браузер не поддерживает отображение видеофайлов"

      previewM = fromMaybe empty $ imgM <|> audioM <|> videoM
      isBlocked = isDisabled || isProcessing

  if isEditing || isNothing resource
     then editRender isBlocked appContext resource state previewM
     else viewRender isBlocked onMoveUp onMoveDown state previewM

  where
    name = "DiagTreeEditorSlideEditorResource"
    classSfx s = name <> "--" <> s
    wrapper = R.li [className $ "list-group-item" <.> name]

    spinnerEl        = createElement spinner
    dropDownSelectEl = createElement dropDownSelect
    dropzoneEl       = createElement dropzone

    viewRender isDisabled onMoveUp onMoveDown state previewM = do
      previewM
      span $ text state.text

      div !. "btn-toolbar" <.> classSfx "buttons" ! role "toolbar" $ do

        if isNothing onMoveUp && isNothing onMoveDown
           then empty
           else do
                button !. "btn btn-default"
                       ! title "Поднять вверх"
                       ! disabled (isNothing onMoveUp)
                       ! onClick state.onMoveUp
                       $ i !. "glyphicon glyphicon-arrow-up" $ empty

                button !. "btn btn-default"
                       ! title "Опустить вниз"
                       ! disabled (isNothing onMoveDown)
                       ! onClick state.onMoveDown
                       $ i !. "glyphicon glyphicon-arrow-down" $ empty

        button !. "btn btn-success"
               ! title "Редактировать"
               ! disabled isDisabled
               ! onClick state.enterEditing
               $ i !. "glyphicon glyphicon-pencil" $ empty

        button !. "btn btn-danger"
               ! title "Удалить"
               ! disabled isDisabled
               ! onClick state.delete
               $ i !. "glyphicon glyphicon-trash" $ empty

    editRender isDisabled appContext resource state previewM = do
      div !. "form-group" $ do
        div $ element $
          dropDownSelectEl
            { appContext
            , isDisabled: isDisabled || isJust state.file || isJust resource
            , variants: (unfoldrBoundedEnum :: Array BackendAttachmentMediaType)
            , selected: Just state.mediaType
            , variantView: showNominative >>> capitalize
            , onSelected: Just state.onMediaTypeSelected
            , placeholder: Just "Тип прикрепляемого файла"
            , notSelectedTitle: Nothing
            } []

        element $
          dropzoneEl (dropzoneDefaultProps state.mediaType)
            { disabled = isDisabled

            , onDropAccepted = toNullable $ Just $ handle2 $
                \files _ -> case head files of
                                 Nothing -> pure unit
                                 Just x  -> state.onFileDropped x

            , onDropRejected = toNullable $ Just $ handle2 $
                \files _ -> state.onFilesRejected files
            } [ R.text $
                "Нажмите для добавления " <> showGenitive state.mediaType <>
                " или перетащите " <>
                sexyShow "его" "файл" "её" (getSex state.mediaType) <> " сюда" ]

        span !. classSfx "edit-image-wrap" $ previewM

        input !. "form-control"
              ! _type "text"
              ! placeholder ("Подпись к " <> showDative state.mediaType)
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
      { isDisabled, resource, updateResource, itemIndex } <- getProps this

      let existing = resource *> (Left <$> itemIndex)
          new = Right <$> state.file -- To create new one `file` must be set
          value = { text: state.text, file: state.file }

      case guard (not isDisabled) *> guard isChanged *> (existing <|> new) of
           Just (Left idx) -> updateResource $ ChangeItem idx value
           Just (Right _)  -> updateResource $ NewItem value
           Nothing         -> pure unit

    deleteHandler this _ = do
      { isDisabled, updateResource, itemIndex } <- getProps this
      { mediaType } <- readState this

      isConfirmed <- DOM.window >>= DOM.confirm
        ("Вы действительно хотите удалить " <> showAccusative mediaType <> "?")

      case guard (not isDisabled) *> guard isConfirmed *> itemIndex of
           Just idx ->
             -- Coercing to not infect parent handler
             -- with DOM and CONFIRM effects.
             unsafeCoerceEff $ updateResource $ DeleteItem idx

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
        itemIndex >>= \x -> map (_ $ x) (if isUp then onMoveUp else onMoveDown)

    buildIntervalValues
      :: Maybe DiagTreeSlideResource
      -> { text :: String
         , file :: Maybe { id        :: Int
                         , hash      :: String
                         , filename  :: String
                         , mediaType :: BackendAttachmentMediaType
                         }

         , mediaType :: BackendAttachmentMediaType
         }

    buildIntervalValues resource =
      { text: fromMaybe "" $ resource <#> _.text
      , mediaType: fromMaybe ImageMediaType $ file <#> _.mediaType
      , file
      }
      where
        file =
          resource <#> _.attachment >>= case _ of
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
                          , mediaType = values.mediaType
                          , isEditing = false
                          , isChanged = false
                          }
        }

      where
        wrap = div !. "list-group-item" <.> classSfx "wrap"

        renderHandler this = do
          props <- getProps  this
          state <- readState this
          pure $ renderIn wrapper $ wrap $ renderFn props state


diagTreeEditorSlideEditorResource :: forall eff. ReactClass (Props eff)
diagTreeEditorSlideEditorResource = diagTreeEditorSlideEditorResourceRender
