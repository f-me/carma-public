module Component.DiagTree.Editor.SlideEditor.Answer
     ( diagTreeEditorSlideEditorAnswer
     ) where

import Prelude hiding (div)

import Control.Alt ((<|>))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Unsafe (unsafeCoerceEff)
import Control.Monad.Aff (launchAff_)
import Control.MonadZero (guard)

import Data.Maybe (Maybe (..), maybe, fromMaybe, isJust, isNothing)
import Data.Either (Either (..), isRight)
import Data.Nullable (Nullable, toNullable)
import Data.Array (head)
import Data.String (null)

import DOM.HTML (window) as DOM
import DOM.HTML.Window (confirm) as DOM
import React.DOM (li) as R
import React.Spaces ((!), (!.), (^), (^^), renderIn, text, empty)
import React.Spaces.DOM (div, img, span, button, i, p, h4, input)

import React.DOM.Props
     ( className, src, role, title, disabled, _type, placeholder, value
     , onClick, onChange
     )

import React
     ( ReactClass, ReactProps, ReactState, ReactRefs, ReadWrite, ReadOnly
     , getProps, readState, createClass, spec'
     , transformState
     )

import Utils ((<.>), eventInputValue)
import App.Store (AppContext)
import Bindings.ReactDropzone (dropzone, handle2)
import Component.Generic.Spinner (spinner)
import Component.DiagTree.Editor.SlideEditor.Helpers (ItemModification (..))

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
     )


type Props eff =
  { appContext :: AppContext
  , key        :: Nullable String
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
      -- ^ Only for adding new one (when `answer` prop is `Nothing`)
  }


diagTreeEditorSlideEditorAnswerRender :: forall eff. ReactClass (Props eff)
diagTreeEditorSlideEditorAnswerRender = createClass $ spec $
  \ { appContext, identity, answer, isDisabled }
    state@{ isEditing, isProcessing, isUploadingFailed, isAttachmentDeleted }
    -> do

  if isNothing answer || map isRight identity == Just true
     then p $ span !. "label label-primary" $ text "Новый ответ"
     else empty

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

  let legacyWarnM =
        case (Modern <$> state.attachment) <|> (answer >>= _.attachment) of
             Just (Legacy _) -> span !. classSfx "deprecation-warning" $ do
               span !. "label label-warning" $ text "Внимание"
               text " Картинка хранится в базе неэффективным образом,\
                    \ рекомендуется загрузить её заново."

             _ -> empty

      imgSrc =
        let
          modern = state.attachment <#> getDiagTreeSlideAttachmentPath
          legacy = answer >>= _.attachment >>= case _ of
                                                    Legacy x -> Just x
                                                    Modern _ -> Nothing
        in
          if isAttachmentDeleted
             then Nothing
             else modern <|> legacy

      imgM =
        case imgSrc of
             Nothing -> empty
             Just x  -> do
               legacyWarnM
               img !. classSfx "image" ! role "presentation" ! src x

      isBlocked = isDisabled || isProcessing

  if isEditing || isNothing answer
     then editRender isBlocked (isJust imgSrc) state imgM
     else viewRender isBlocked state imgM

  where
    name = "DiagTreeEditorSlideEditorAnswer"
    classSfx s = name <> "--" <> s
    wrapper = R.li [className $ "list-group-item" <.> name]

    viewRender isDisabled state imgM = do

      h4 !. "list-group-item-heading" $ text state.header

      p !. "list-group-item-text" $ do
        imgM
        span $ text state.text

      div !. "btn-toolbar" <.> classSfx "buttons" ! role "toolbar" $ do

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

    editRender isDisabled hasImage state imgM = do
      div !. "form-group" $
        input !. "form-control"
              ! _type "text"
              ! placeholder "Ответ"
              ! value state.header
              ! onChange state.onChangeHeader
              ! disabled isDisabled

      div !. "form-group" $
        input !. "form-control"
              ! _type "text"
              ! placeholder "Комментарий"
              ! value state.text
              ! onChange state.onChangeText
              ! disabled isDisabled

      div !. "form-group" $ do
        dropzone ^^ dropzoneDefaultProps
          { disabled = isDisabled

          , onDropAccepted = toNullable $ Just $ handle2 $
              \files _ -> case head files of
                               Nothing -> pure unit
                               Just x  -> state.onFileDropped x

          , onDropRejected = toNullable $ Just $ handle2 $
              \files _ -> state.onFilesRejected files
          }
          $ text "Нажмите для добавления картинки или перетащите её сюда"

        imgM

        if not hasImage
           then empty
           else do text " "
                   button !. "btn btn-danger"
                          ! disabled isDisabled
                          ! onClick state.deleteAttachment
                          $ do i !. "glyphicon glyphicon-trash" $ empty
                               text " Удалить картинку"

      div !. "btn-toolbar" $ do
        button !. "btn btn-default"
               ! _type "button"
               ! onClick state.cancelEditing
               ! disabled isDisabled
               $ text "Отменить"

        let isSaveBlocked =
              isDisabled || not state.isChanged || null state.header

        button !. "btn btn-success"
               ! _type "button"
               ! onClick state.save
               ! disabled isSaveBlocked
               $ text "Сохранить ответ"

    enterEditingHandler this _ =
      transformState this _ { isEditing = true }

    cancelEditingHandler this _ = do
      { answer, onCancel } <- getProps this

      if isNothing answer

         then case onCancel of
                   Nothing -> pure unit
                   Just f  -> f

         else let values = buildIntervalValues answer
               in transformState this _
                    { header              = values.header
                    , text                = values.text
                    , attachment          = values.attachment
                    , isEditing           = false
                    , isChanged           = false
                    , isAttachmentDeleted = false
                    }

    changeTextHandler this HeaderField event = do
      let x = eventInputValue event
      transformState this _ { header = x, isChanged = true }

    changeTextHandler this TextField event = do
      let x = eventInputValue event
      transformState this _ { text = x, isChanged = true }

    deleteAttachmentHandler this _ = guardConfirmed $ transformState this _
      { attachment = Nothing
      , isAttachmentDeleted = true
      , isChanged = true
      }

      where
        guardConfirmed m = do
          isConfirmed <- DOM.window >>= DOM.confirm
            "Вы действительно хотите удалить картинку?"

          if isConfirmed then m else pure unit

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
            , isAttachmentDeleted = false
            , isChanged = true
            , attachment = Just attachment
            }

    saveHandler this _ = do
      state@{ isChanged } <- readState this
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
           Just ident ->
             -- Coercing to not infect parent handler
             -- with DOM and CONFIRM effects.
             unsafeCoerceEff $ updateAnswer $ DeleteItem ident

           _ -> pure unit

    buildIntervalValues answer =
      { header     : fromMaybe "" $ answer <#> _.header
      , text       : fromMaybe "" $ answer <#> _.text
      , attachment : answer <#> _.attachment >>=
                       case _ of
                            Just (Modern x) -> Just x
                            _ -> Nothing
      }

    getInitialState this = do
      { answer } <- getProps this
      let values = buildIntervalValues answer

      pure { header: values.header
           , text: values.text
           , attachment: values.attachment
           , isEditing: false
           , isChanged: false
           , isAttachmentDeleted: false
           , isProcessing: false
           , isUploadingFailed: false
           , enterEditing: enterEditingHandler this
           , cancelEditing: cancelEditingHandler this
           , onChangeHeader: changeTextHandler this HeaderField
           , onChangeText: changeTextHandler this TextField
           , deleteAttachment: deleteAttachmentHandler this
           , onFileDropped: fileDroppedHandler this
           , onFilesRejected: rejectedFilesAlert
           , save: saveHandler this
           , delete: deleteHandler this
           }

    spec renderFn =
      spec' getInitialState renderHandler # _
        { displayName = name

        , componentWillReceiveProps = \this nextProps -> do
            prevProps <- getProps this

            if isJust nextProps.answer == isJust prevProps.answer &&
               Just true == (eqAnswer <$> nextProps.answer <*> prevProps.answer)
               then pure unit
               else let values = buildIntervalValues nextProps.answer
                     in transformState this _
                          { header              = values.header
                          , text                = values.text
                          , attachment          = values.attachment
                          , isEditing           = false
                          , isChanged           = false
                          , isAttachmentDeleted = false
                          }
        }

      where
        wrap = div !. "list-group-item" <.> classSfx "wrap"

        renderHandler this = do
          props <- getProps  this
          state <- readState this
          pure $ renderIn wrapper $ wrap $ renderFn props state

        eqAnswer a b =
          a.header     == b.header &&
          a.text       == b.text &&
          a.attachment == b.attachment


diagTreeEditorSlideEditorAnswer :: forall eff. ReactClass (Props eff)
diagTreeEditorSlideEditorAnswer = diagTreeEditorSlideEditorAnswerRender


data Field = HeaderField | TextField
