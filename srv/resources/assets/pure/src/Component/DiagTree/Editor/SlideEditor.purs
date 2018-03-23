module Component.DiagTree.Editor.SlideEditor
     ( diagTreeEditorSlideEditor
     ) where

import Prelude hiding (div)

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.MonadZero (guard)
import Control.Alt ((<|>))

import Data.Tuple (Tuple (Tuple), snd)
import Data.Array ((!!), snoc, updateAt, modifyAt, deleteAt, null)
import Data.Foldable (class Foldable, foldl, foldM, length)
import Data.Record.Builder (merge)
import Data.Maybe (Maybe (..), isJust, maybe)
import Data.Either (Either (..))
import Data.Map as Map
import Data.Nullable (toNullable)
import Data.String (joinWith)

import React.DOM (form, div) as R
import React.Spaces ((!), (!.), (^), renderIn, text, empty, elements)
import React.Spaces.DOM (div, input, button, label, i, p, span)
import React.Spaces.DOM.Dynamic (ul) as SDyn

import React.DOM.Props
     ( className, _type, placeholder, value, disabled
     , onChange, onClick
     )

import React
     ( ReactClass, ReactProps, ReactState, ReactRefs, ReadWrite, ReadOnly
     , createClass, spec', createElement
     , getProps, readState, transformState
     , handle
     )

import Utils
     ( (<.>), storeConnect, toMaybeT, eventInputValue
     , createClassStatelessWithName
     , unfoldrBoundedEnum
     , showAccusative
     )

import Utils.DiagTree.Editor
     ( getSlideByBranch
     , eqDiagTreeSlideResources
     , eqIshDiagTreeSlideAnswers
     )

import Component.Generic.DropDownSelect (OnSelectedEff, dropDownSelect)
import App.Store (AppContext, dispatch)
import App.Store.Actions (AppAction (DiagTree))
import App.Store.DiagTree.Actions (DiagTreeAction (Editor))
import Component.DiagTree.Editor.SlideEditor.Helpers (ItemModification (..))

import Bindings.ReactRichTextEditor
     ( RTE
     , EditorValue
     , EditorValueFormat (Markdown)
     , createValueFromString
     , richTextEditor
     , richTextEditorDefaultProps
     , valueToString
     )

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (SaveSlideRequest)
     )

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlide (DiagTreeSlide)
     , DiagTreeSlideId
     , DiagTreeSlideResource
     , DiagTreeSlideAttachment (..)
     , DiagTreeSlideAction
     , DiagTreeSlideAnswer
     )

import App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAttachment
     ( BackendAttachment
     , BackendAttachmentMediaType
     )

import Component.DiagTree.Editor.SlideEditor.Resource
     ( diagTreeEditorSlideEditorResource
     )

import Component.DiagTree.Editor.SlideEditor.Answer
     ( diagTreeEditorSlideEditorAnswer
     )


resourcesRender
  :: forall f eff
   . Foldable f
  => ReactClass
       { appContext :: AppContext
       , slideId    :: DiagTreeSlideId
       , isDisabled :: Boolean
       , resources  :: f DiagTreeSlideResource

       , updateResource -- See item component for details
           :: ItemModification Int
                { text :: String
                , file :: Maybe BackendAttachment
                }

           -> Eff ( props :: ReactProps
                  , state :: ReactState ReadWrite
                  , refs  :: ReactRefs  ReadOnly
                  | eff
                  ) Unit

       , onMoveUp   :: Int -> Eff ( props :: ReactProps
                                  , state :: ReactState ReadWrite
                                  , refs  :: ReactRefs  ReadOnly
                                  | eff
                                  ) Unit

       , onMoveDown :: Int -> Eff ( props :: ReactProps
                                  , state :: ReactState ReadWrite
                                  , refs  :: ReactRefs  ReadOnly
                                  | eff
                                  ) Unit
       }

resourcesRender = createClass $ spec $
  \ { appContext, slideId, isDisabled, resources
    , updateResource, onMoveUp, onMoveDown
    }
    { isAdding, turnAddingOn, turnAddingOff } -> do

  label !. "control-label" $ text "Прикреплённые файлы"

  SDyn.ul !. "list-group" <.> classSfx "list" $

    let itemReducer (Tuple itemIndex list) resource =
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
                            if itemIndex >= (length resources - 1)
                               then Nothing
                               else Just onMoveDown
                        }

             in createElement diagTreeEditorSlideEditorResource props []

     in elements $ snd $ foldl itemReducer (Tuple 0 []) resources

  if isAdding
     then diagTreeEditorSlideEditorResource ^
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

    turnAddingHandler this isOn =
      transformState this _ { isAdding = isOn }

    getInitialState this = pure
      { isAdding: false
      , turnAddingOn: const $ turnAddingHandler this true
      , turnAddingOff: turnAddingHandler this false
      }

    spec renderFn =
      spec' getInitialState renderHandler # _ { displayName = name }
      where
        renderHandler this = do
          props <- getProps  this
          state <- readState this
          pure $ renderer $ renderFn props state


answersRender
  :: forall f f2 eff
   . Foldable f
  => Foldable f2
  => ReactClass
       { appContext :: AppContext
       , slideId    :: DiagTreeSlideId
       , isDisabled :: Boolean
       , answers    :: f DiagTreeSlideAnswer

       , newAnswers :: f2 { header     :: String
                          , text       :: String
                          , attachment :: Maybe DiagTreeSlideAttachment
                          }

       , updateAnswer -- See answer item props type component for details
           :: ItemModification (Either DiagTreeSlideId Int)
                { header              :: String
                , text                :: String
                , attachment          :: Maybe BackendAttachment
                , isAttachmentDeleted :: Boolean
                }

           -> Eff ( props :: ReactProps
                  , state :: ReactState ReadWrite
                  , refs  :: ReactRefs  ReadOnly
                  | eff
                  ) Unit
       }

answersRender = createClass $ spec $
  \ { appContext, slideId, isDisabled, answers, newAnswers, updateAnswer }
    { isAdding, turnAddingOn, turnAddingOff } -> do

  label !. "control-label" $ text "Ответы"

  SDyn.ul !. "list-group" <.> classSfx "list" $

    let
      reducer list answer = list `snoc`
        let
          p = props (Left $ answer.nextSlide # \(DiagTreeSlide x) -> x.id)
            { header: answer.header
            , text: answer.text
            , attachment: answer.attachment
            }
        in
          createElement diagTreeEditorSlideEditorAnswer p []

      newReducer (Tuple itemIndex list) answer =
        Tuple (itemIndex + 1) $ list `snoc`
          let
            p = props (Right itemIndex)
              { header: answer.header
              , text: answer.text
              , attachment: answer.attachment
              }
          in
            createElement diagTreeEditorSlideEditorAnswer p []

      props identity item =
        { appContext
        , slideId
        , key: toNullable $ Just $ show identity
        , identity: Just identity
        , isDisabled
        , answer: Just item
        , updateAnswer
        , onCancel: Nothing
        }

    in do
      elements $ foldl reducer [] answers
      elements $ snd $ foldl newReducer (Tuple 0 []) newAnswers

  if isAdding
     then diagTreeEditorSlideEditorAnswer ^
            { appContext
            , slideId
            , key: toNullable Nothing
            , identity: Nothing
            , isDisabled
            , answer: Nothing
            , updateAnswer
            , onCancel: Just turnAddingOff
            }

     else button !. "btn btn-default" <.> classSfx "add-button"
                 ! _type "button"
                 ! onClick turnAddingOn
                 ! disabled isDisabled
                 $ do i !. "glyphicon glyphicon-plus" $ empty
                      text " Добавить ответ"

  where
    name = "DiagTreeEditorSlideEditorAnswers"
    classSfx s = name <> "--" <> s
    wrapper = R.div [className $ "form-group" <.> name]
    renderer = renderIn wrapper

    turnAddingHandler this isOn =
      transformState this _ { isAdding = isOn }

    getInitialState this = pure
      { isAdding: false
      , turnAddingOn: const $ turnAddingHandler this true
      , turnAddingOff: turnAddingHandler this false
      }

    spec renderFn =
      spec' getInitialState renderHandler # _ { displayName = name }
      where
        renderHandler this = do
          props <- getProps  this
          state <- readState this
          pure $ renderer $ renderFn props state


actionRender
  :: forall eff
   . ReactClass { appContext :: AppContext
                , isDisabled :: Boolean
                , action     :: Maybe DiagTreeSlideAction

                , onSelected :: Maybe DiagTreeSlideAction
                             -> Eff (OnSelectedEff eff) Unit
                }

actionRender = createClassStatelessWithName name $
  \ { appContext, isDisabled, action, onSelected } -> renderer $ do

  label !. "control-label" $ text "Рекомендация"

  div $ dropDownSelect ^
    { appContext
    , isDisabled
    , variants
    , selected: action
    , variantView: show
    , onSelected: Just onSelected
    , placeholder: Just "Что делать?"
    , notSelectedTitle: Just "(не выбрано)"
    }

  where
    name = "DiagTreeEditorSlideEditorActions"
    classSfx s = name <> "--" <> s
    wrapper = R.div [className $ "form-group" <.> name]
    renderer = renderIn wrapper
    variants = (unfoldrBoundedEnum :: Array DiagTreeSlideAction)


diagTreeEditorSlideEditorRender
  :: ReactClass
       { appContext   :: AppContext
       , slide        :: Maybe DiagTreeSlide
       , slidePath    :: Maybe (Array DiagTreeSlideId)
       , isProcessing :: Boolean
       , isFailed     :: Boolean
       }

diagTreeEditorSlideEditorRender = createClass $ spec $
  \ { appContext, isProcessing, isFailed }
    { slide: (DiagTreeSlide slide)
    , rteSlideBody
    , newAnswers
    , isChanged
    , onChangeHeader
    , onChangeBody
    , updateResource, onResourceMoveUp, onResourceMoveDown
    , updateAnswer
    , onSelectAction
    , onCancel
    , onSave
    } -> do

  if not isFailed
     then pure unit
     else div $ p $ do
            span !. "label label-danger" $ text "Ошибка"
            text " Произошла ошибка при сохранении изменений."

  div !. "form-group" $
    input !. "form-control" <.> classSfx "header"
          ! _type "text"
          ! placeholder "Заголовок"
          ! disabled isProcessing
          ! value slide.header
          ! onChange onChangeHeader

  div !. "form-group" $
    richTextEditor ^ (richTextEditorDefaultProps rteSlideBody)
      { placeholder = toNullable $ Just "Описание"
      , disabled    = toNullable $ Just isProcessing
      , onChange    = toNullable $ Just $ handle onChangeBody
      }

  resourcesRender ^
    { appContext
    , slideId: slide.id
    , isDisabled: isProcessing
    , resources: slide.resources
    , updateResource
    , onMoveUp: onResourceMoveUp
    , onMoveDown: onResourceMoveDown
    }

  let hasAction  = isJust slide.action
      hasAnswers = not $ Map.isEmpty slide.answers && null newAnswers
      hasBoth    = hasAction && hasAnswers

  -- Rendering "answers" only if "action" is not set
  if hasAction && not hasBoth
     then empty
     else answersRender ^ { appContext
                          , slideId: slide.id
                          , isDisabled: isProcessing
                          , answers: slide.answers
                          , newAnswers
                          , updateAnswer
                          }

  -- Rendering "action" only if "answers" is empty
  if hasAnswers && not hasBoth
     then empty
     else actionRender ^ { appContext
                         , isDisabled: isProcessing
                         , action: slide.action
                         , onSelected: onSelectAction
                         }

  if not hasBoth
     then pure unit
     else div $ p $ do
            span !. "label label-danger" $ text "Ошибка"
            text " Одновременно заданы «рекомендация» и «ответы»\
                 \ (необходимо оставить либо «рекомендацию», либо «ответы»)."

  div !. "btn-toolbar" $ do

    let isBlocked = isProcessing || not isChanged

    button !. "btn btn-default"
           ! _type "button"
           ! disabled isBlocked
           ! onClick onCancel
           $ text "Отменить изменения"

    button !. "btn btn-success"
           ! _type "button"
           ! disabled (isBlocked || hasBoth)
           ! onClick onSave
           $ text "Сохранить"

  where
    name = "DiagTreeEditorSlideEditor"
    classSfx s = name <> "--" <> s
    wrapper = R.form [className name]
    renderer = renderIn wrapper

    changeHeaderHandler this event = do
      let x = eventInputValue event

      transformState this $
        \s -> s { slide = s.slide <#> updater x, isChanged = true }

      where updater x (DiagTreeSlide s) = DiagTreeSlide s { header = x }

    changeBodyHandler this value = do
      let x = valueToString value Markdown

      transformState this $ \s -> s
        { slide = s.slide <#> updater x
        , rteSlideBody = value
        , isChanged = true
        }

      where updater x (DiagTreeSlide s) = DiagTreeSlide s { body = x }

    selectActionHandler this action =
      transformState this $
        \s -> s { slide = s.slide <#> updater, isChanged = true }

      where updater (DiagTreeSlide s) = DiagTreeSlide s { action = action }

    updateResourceHandler this (NewItem resource) =
      transformState this $ \s ->
        let
          newSlide = do
            (DiagTreeSlide slide) <- s.slide

            newResource <- resource.file <#> Modern <#>
              { text: resource.text, attachment: _ }

            pure $ DiagTreeSlide
                 $ slide { resources = _ }
                 $ slide.resources `snoc` newResource
        in
          s { slide = newSlide <|> s.slide, isChanged = isJust newSlide }

    updateResourceHandler this (DeleteItem itemIndex) =
      transformState this $ \s ->
        let
          newSlide = do
            (DiagTreeSlide slide) <- s.slide

            DiagTreeSlide <$> slide { resources = _ }
                          <$> deleteAt itemIndex slide.resources
        in
          s { slide = newSlide <|> s.slide, isChanged = isJust newSlide }

    updateResourceHandler this (ChangeItem itemIndex resource) =
      transformState this $ \s ->
        let
          newSlide = do
            (DiagTreeSlide slide) <- s.slide
            old <- slide.resources !! itemIndex

            let newResource =
                  { text       : resource.text
                  , attachment : maybe old.attachment Modern resource.file
                  }

            DiagTreeSlide <$> slide { resources = _ }
                          <$> updateAt itemIndex newResource slide.resources
        in
          s { slide = newSlide <|> s.slide, isChanged = isJust newSlide }

    moveResourceHandler this isUp idx =
      transformState this $ \s ->
        let
          newSlide = do
            (DiagTreeSlide slide@{ resources }) <- s.slide

            DiagTreeSlide <$> slide { resources = _ } <<< snd
                          <$> foldM (reducer resources) (Tuple 0 []) resources

          swapIdx = if isUp then idx - 1 else idx + 1

          reducer resources (Tuple n acc) x =
            Tuple (n + 1) <<< snoc acc <$>
              if idx     == n then resources !! swapIdx else
              if swapIdx == n then resources !! idx     else
              pure x
        in
          s { slide = newSlide <|> s.slide, isChanged = isJust newSlide }

    updateAnswerHandler this (NewItem answer) =
      transformState this $ \s -> s
        { isChanged = true

        , newAnswers = s.newAnswers `snoc`
            { header     : answer.header
            , text       : answer.text
            , attachment : Modern <$> answer.attachment
            }
        }

    -- Deleting existing answer
    updateAnswerHandler this (DeleteItem (Left nextSlideId)) =
      transformState this $ \s ->
        let
          f (DiagTreeSlide slide) = DiagTreeSlide
            slide { answers = nextSlideId `Map.delete` slide.answers }

          newSlide = s.slide <#> f
        in
          s { slide = newSlide <|> s.slide, isChanged = isJust newSlide }

    -- Updating existing answer
    updateAnswerHandler this (ChangeItem (Left nextSlideId) answer) =
      transformState this $ \s ->
        let
          updater = Just <<< _
            { header     = answer.header
            , text       = answer.text
            , attachment = (Modern <$> answer.attachment) <|> legacyAttachment
            }

          legacyAttachment = do
            guard $ not answer.isAttachmentDeleted
            (DiagTreeSlide slide) <- s.slide
            foundAnswer <- nextSlideId `Map.lookup` slide.answers

            case foundAnswer.attachment of
                 x@(Just (Legacy _)) -> x
                 _ -> Nothing

          f (DiagTreeSlide slide) = DiagTreeSlide
            slide { answers = Map.update updater nextSlideId slide.answers }

          newSlide = s.slide <#> f
        in
          s { slide = newSlide <|> s.slide, isChanged = isJust newSlide }

    -- Deleting new added answer
    updateAnswerHandler this (DeleteItem (Right itemIndex)) =
      transformState this $ \s ->
        case itemIndex `deleteAt` s.newAnswers of
             Nothing -> s
             Just x  -> s { isChanged = true, newAnswers = x }

    -- Updating new added answer
    updateAnswerHandler this (ChangeItem (Right itemIndex) answer) =
      transformState this $ \s ->
        let
          updater = _
            { header     = answer.header
            , text       = answer.text
            , attachment = Modern <$> answer.attachment
            }
        in
          case modifyAt itemIndex updater s.newAnswers of
               Nothing -> s
               Just x  -> s { isChanged = true, newAnswers = x }

    cancelHandler this event = do
      { slide } <- getProps this
      resetChanges this slide

    saveHandler this event = do
      { appContext, slidePath } <- getProps this
      { slide, newAnswers } <- readState this

      case Tuple <$> slidePath <*> slide of
           Nothing -> pure unit
           Just (Tuple x y) -> launchAff_ $
             dispatch appContext $ DiagTree $ Editor $
               SaveSlideRequest x { slide: y, newAnswers }

    resetChanges this slide = do
      rteSlideBody <- buildRteSlideBody slide

      transformState this _
        { slide        = slide
        , rteSlideBody = rteSlideBody
        , newAnswers   = []
        , isChanged    = false
        }

    fetchSlide this state props = do
      (DiagTreeSlide prevSlide) <- toMaybeT state.slide
      (DiagTreeSlide nextSlide) <- toMaybeT props.slide

      if nextSlide.header == prevSlide.header &&
         nextSlide.body   == prevSlide.body   &&
         nextSlide.action == prevSlide.action &&

         eqDiagTreeSlideResources  nextSlide.resources prevSlide.resources &&
         eqIshDiagTreeSlideAnswers nextSlide.answers   prevSlide.answers

         then toMaybeT $ pure unit
         else liftEff $ resetChanges this props.slide

    buildRteSlideBody
      :: forall eff. Maybe DiagTreeSlide -> Eff (rte :: RTE | eff) EditorValue
    buildRteSlideBody slide =
      let f = maybe "" (\(DiagTreeSlide x) -> x.body) slide
       in createValueFromString f Markdown

    getInitialState this = do
      { slide } <- getProps this
      rteSlideBody <- buildRteSlideBody slide

      pure { slide
           , rteSlideBody

           , newAnswers: [] -- It hasn't `nextSlide` yet,
                            -- that's why it's separated.

           , isChanged          : false
           , onChangeHeader     : changeHeaderHandler   this
           , onChangeBody       : changeBodyHandler     this
           , updateResource     : updateResourceHandler this
           , onResourceMoveUp   : moveResourceHandler   this true
           , onResourceMoveDown : moveResourceHandler   this false
           , updateAnswer       : updateAnswerHandler   this
           , onSelectAction     : selectActionHandler   this
           , onCancel           : cancelHandler         this
           , onSave             : saveHandler           this
           }

    spec renderFn =
      spec' getInitialState renderHandler # _
        { displayName = name

        , componentWillReceiveProps = \this nextProps -> do
            state <- readState this
            void $ runMaybeT $ fetchSlide this state nextProps
        }

      where
        renderHandler this = do
          props <- getProps  this
          state <- readState this

          pure $ renderer $
            case state.slide of
                 Nothing -> text "…"
                 Just x  -> renderFn props state { slide = x }


diagTreeEditorSlideEditor :: ReactClass { appContext :: AppContext }
diagTreeEditorSlideEditor = storeConnect f diagTreeEditorSlideEditorRender
  where
    f appState = let branch = appState.diagTree.editor in merge
      { slide:
          branch.selectedSlideBranch >>= getSlideByBranch branch.slides

      , slidePath: branch.selectedSlideBranch

      -- This component supposed to be rendered only when any upper editor
      -- processing is done, so we check processing only of slide editing
      -- actions.
      , isProcessing: branch.slideSaving.isProcessing

      , isFailed: branch.slideSaving.isFailed
      }
