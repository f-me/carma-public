module Component.DiagTree.Editor.SlideEditor
     ( diagTreeEditorSlideEditor
     ) where

import Prelude hiding (div)

import Data.Monoid (mempty)
import Data.Tuple (Tuple (Tuple), fst, snd)
import Data.Foldable (foldM)
import Data.Record.Builder (merge)
import Data.Maybe (Maybe (..), isJust, maybe, fromMaybe)
import Data.Either (Either (..))
import Data.Map as Map
import Data.Nullable (toNullable)
import Data.Array ((!!), index, snoc, updateAt, modifyAt, deleteAt, null)

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.MonadZero (guard)
import Control.Alt ((<|>))

import React.DOM (text, form, div, div', input, button, p', span)

import React.DOM.Props
     ( className, _type, placeholder, value, disabled
     , onChange, onClick
     )

import React
     ( ReactClass, EventHandler
     , createClass, spec', createElement
     , getProps, readState, transformState
     , handle
     )

import Utils
     ( (<.>), storeConnect, toMaybeT, eventInputValue, callEventHandler
     )

import Utils.Debouncer
     ( newDebouncer
     , subscribeToDebouncer
     , unsubscribeFromDebouncer
     , sendToDebouncer
     )

import Utils.DiagTree.Editor
     ( getSlideByBranch
     , eqDiagTreeSlideResources
     , eqIshDiagTreeSlideAnswers
     )

import App.Store (AppContext, dispatch)
import App.Store.Actions (AppAction (DiagTree))
import App.Store.DiagTree.Actions (DiagTreeAction (Editor))
import Component.DiagTree.Editor.SlideEditor.Helpers (ItemModification (..))

import Bindings.ReactRichTextEditor
     ( EditorValueFormat (Markdown)
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
     , DiagTreeSlideAttachment (..)
     , fromIndexedAnswers
     , toIndexedAnswers
     )

import Component.DiagTree.Editor.SlideEditor.Resources
     ( diagTreeEditorSlideEditorResources
     )

import Component.DiagTree.Editor.SlideEditor.Answers
     ( diagTreeEditorSlideEditorAnswers
     )

import Component.DiagTree.Editor.SlideEditor.Action
     ( diagTreeEditorSlideEditorAction
     )


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
    { newAnswers
    , isChanged
    , onChangeHeader
    , onChangeBody
    , updateResource, onResourceMoveUp, onResourceMoveDown
    , updateAnswer,   onAnswerMoveUp,   onAnswerMoveDown
    , onSelectAction
    , onCancel
    , onSave
    } (DiagTreeSlide slide) ->

  ( if not isFailed
       then mempty
       else pure $
            div' $ pure $
              p' [ span [className "label label-danger"] [text "Ошибка"]
                 , text " Произошла ошибка при сохранении изменений."
                 ]
  )

  <>

  [ div [className "form-group"] $ pure $
      flip input mempty
        [ className $ "form-control" <.> classSfx "header"
        , _type "text"
        , placeholder "Заголовок"
        , disabled isProcessing
        , value slide.header
        , onChange onChangeHeader
        ]

  , flip rteWrapEl mempty
      { appContext
      , slideId: slide.id
      , isProcessing
      , value: slide.body
      , onChange: onChangeBody
      }

  , flip resourcesEl mempty
      { appContext
      , slideId: slide.id
      , isDisabled: isProcessing
      , resources: slide.resources
      , updateResource
      , onMoveUp: onResourceMoveUp
      , onMoveDown: onResourceMoveDown
      }
  ]

  <>

  let
    hasAction  = isJust slide.action
    answers    = fst $ fromIndexedAnswers slide.answers
    hasAnswers = not $ null answers && null newAnswers
    hasBoth    = hasAction && hasAnswers
  in
    -- Rendering "answers" only if "action" is not set
    ( if hasAction && not hasBoth
         then mempty
         else pure $
              flip answersEl mempty
                { appContext
                , slideId: slide.id
                , isDisabled: isProcessing
                , answers: fst $ fromIndexedAnswers slide.answers
                , newAnswers
                , updateAnswer
                , onMoveUp: onAnswerMoveUp
                , onMoveDown: onAnswerMoveDown
                }
    )

    <>

    ( -- Rendering "action" only if "answers" is empty
      if hasAnswers && not hasBoth
         then mempty
         else pure $
              flip actionEl mempty
                { appContext
                , isDisabled: isProcessing
                , action: slide.action
                , onSelected: onSelectAction
                }
    )

    <>

    ( if not hasBoth
         then mempty
         else pure $
              div' $ pure $
                p' [ span [className "label label-danger"] [text "Ошибка"]
                   , text " Одновременно заданы «рекомендация» и «ответы»\
                          \ (необходимо оставить либо «рекомендацию»,\
                          \ либо «ответы»)."
                   ]
    )

    <>

    [ div
        [ className "btn-toolbar" ]
        $
        let
          isBlocked = isProcessing || not isChanged
        in
          [ button
              [ className "btn btn-default"
              , _type "button"
              , disabled isBlocked
              , onClick onCancel
              ]
              [ text "Отменить изменения" ]

          , button
              [ className "btn btn-success"
              , _type "button"
              , disabled $ isBlocked || hasBoth
              , onClick onSave
              ]
              [ text "Сохранить" ]
          ]
      ]

  where
    name = "DiagTreeEditorSlideEditor"
    classSfx s = name <> "--" <> s
    wrapper = form [className name]

    rteWrapEl   = createElement rteWrap
    resourcesEl = createElement diagTreeEditorSlideEditorResources
    answersEl   = createElement diagTreeEditorSlideEditorAnswers
    actionEl    = createElement diagTreeEditorSlideEditorAction

    changeHeaderHandler this event = go where
      go = transformState this
         $ \s -> s { slide = s.slide <#> updater x, isChanged = true }

      updater x' (DiagTreeSlide s) = DiagTreeSlide s { header = x' }
      x = eventInputValue event

    changeBodyHandler this valueStr = do
      transformState this $ \s ->
        let
          newState = do
            (DiagTreeSlide slide) <- s.slide

            -- This checks whether something is changed.
            -- This check solves the issue when RTE field just focused and it
            -- triggered change event and then slide marked as it have changes.
            guard $ slide.body /= valueStr

            pure s { slide = pure $ DiagTreeSlide slide { body = valueStr }
                   , isChanged = true
                   }
        in
          fromMaybe s newState

    selectActionHandler this action = go where
      updater (DiagTreeSlide s) = DiagTreeSlide s { action = action }
      go = transformState this
         $ \s -> s { slide = s.slide <#> updater, isChanged = true }

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
          s { slide     = newSlide <|> s.slide
            , isChanged = s.isChanged || isJust newSlide
            }

    updateResourceHandler this (DeleteItem itemIndex) =
      transformState this $ \s ->
        let
          newSlide = do
            (DiagTreeSlide slide) <- s.slide

            DiagTreeSlide <$> slide { resources = _ }
                          <$> deleteAt itemIndex slide.resources
        in
          s { slide     = newSlide <|> s.slide
            , isChanged = s.isChanged || isJust newSlide
            }

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
          s { slide     = newSlide <|> s.slide
            , isChanged = s.isChanged || isJust newSlide
            }

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
          s { slide     = newSlide <|> s.slide
            , isChanged = s.isChanged || isJust newSlide
            }

    moveAnswerHandler this isUp (Left slideId) = -- For existing answer
      transformState this $ \s ->
        let
          newSlide = do
            (DiagTreeSlide slide) <- s.slide
            let Tuple answers answersIndexes = fromIndexedAnswers slide.answers
            idx <- slideId `Map.lookup` answersIndexes

            let swapIdx = if isUp then idx - 1 else idx + 1
                reducerApplied = reducer idx swapIdx answers

            newAnswers <- snd <$> foldM reducerApplied (Tuple 0 []) answers
            pure $ DiagTreeSlide slide { answers = toIndexedAnswers newAnswers }

          reducer idx swapIdx answers (Tuple n acc) x =
            Tuple (n + 1) <<< snoc acc <$>
              if idx     == n then answers !! swapIdx else
              if swapIdx == n then answers !! idx     else
              pure x
        in
          s { slide     = newSlide <|> s.slide
            , isChanged = s.isChanged || isJust newSlide
            }

    moveAnswerHandler this isUp (Right idx) = -- For one of new answers
      transformState this $ \s ->
        let
          newAnswers = foldM reducer (Tuple 0 []) s.newAnswers
          swapIdx = if isUp then idx - 1 else idx + 1

          reducer (Tuple n acc) x =
            Tuple (n + 1) <<< snoc acc <$>
              if idx     == n then s.newAnswers !! swapIdx else
              if swapIdx == n then s.newAnswers !! idx     else
              pure x
        in
          s { newAnswers = maybe s.newAnswers snd newAnswers
            , isChanged  = s.isChanged || isJust newAnswers
            }

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
    updateAnswerHandler this (DeleteItem (Left slideId)) =
      transformState this $ \s ->
        let
          f (DiagTreeSlide slide) = do
            let Tuple answers answersIndexes = fromIndexedAnswers slide.answers
            idx     <- slideId `Map.lookup` answersIndexes
            answers <- deleteAt idx answers
            pure $ DiagTreeSlide slide { answers = toIndexedAnswers answers }

          newSlide = s.slide >>= f
        in
          s { slide     = newSlide <|> s.slide
            , isChanged = s.isChanged || isJust newSlide
            }

    -- Updating existing answer
    updateAnswerHandler this (ChangeItem (Left slideId) answer) =
      transformState this $ \s ->
        let
          updater = _
            { header     = answer.header
            , text       = answer.text
            , attachment = (Modern <$> answer.attachment) <|> legacyAttachment
            }

          legacyAttachment = do
            guard $ not answer.isAttachmentDeleted
            (DiagTreeSlide slide) <- s.slide
            let Tuple answers answersIndexes = fromIndexedAnswers slide.answers
            foundAnswer <- slideId `Map.lookup` answersIndexes >>= index answers

            case foundAnswer.attachment of
                 x@(Just (Legacy _)) -> x
                 _ -> Nothing

          f (DiagTreeSlide slide) = do
            let Tuple answers answersIndexes = fromIndexedAnswers slide.answers
            idx     <- slideId `Map.lookup` answersIndexes
            answers <- modifyAt idx updater answers
            pure $ DiagTreeSlide slide { answers = toIndexedAnswers answers }

          newSlide = s.slide >>= f
        in
          s { slide     = newSlide <|> s.slide
            , isChanged = s.isChanged || isJust newSlide
            }

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
      transformState this _
        { slide      = slide
        , newAnswers = []
        , isChanged  = false
        }

    fetchSlide this state props = do
      (DiagTreeSlide prevSlide) <- toMaybeT state.slide
      (DiagTreeSlide nextSlide) <- toMaybeT props.slide

      if nextSlide.header == prevSlide.header &&
         nextSlide.body   == prevSlide.body   &&
         nextSlide.action == prevSlide.action &&

         eqDiagTreeSlideResources nextSlide.resources prevSlide.resources &&

         let nextAnswers = fst $ fromIndexedAnswers nextSlide.answers
             prevAnswers = fst $ fromIndexedAnswers prevSlide.answers

          in eqIshDiagTreeSlideAnswers nextAnswers prevAnswers

         then toMaybeT $ pure unit
         else liftEff $ resetChanges this props.slide

    getInitialState this = do
      { slide } <- getProps this

      pure { slide

           , newAnswers: [] -- It hasn't `nextSlide` yet,
                            -- that's why it's separated.

           , isChanged          :          false
           , onChangeHeader     :          changeHeaderHandler   this
           , onChangeBody       : handle $ changeBodyHandler     this
           , updateResource     : handle $ updateResourceHandler this
           , onResourceMoveUp   : handle $ moveResourceHandler   this true
           , onResourceMoveDown : handle $ moveResourceHandler   this false
           , updateAnswer       : handle $ updateAnswerHandler   this
           , onAnswerMoveUp     : handle $ moveAnswerHandler     this true
           , onAnswerMoveDown   : handle $ moveAnswerHandler     this false
           , onSelectAction     : handle $ selectActionHandler   this
           , onCancel           :          cancelHandler         this
           , onSave             :          saveHandler           this
           }

    spec renderFn = x where
      renderHandler this = do
        state <- readState this

        map wrapper $
          case state.slide of
               Nothing -> pure [text "…"]
               Just x' -> do
                 props <- getProps this
                 pure $ renderFn props state x'

      x = spec' getInitialState renderHandler # _
        { displayName = name

        , componentWillReceiveProps = \this nextProps -> do
            state <- readState this
            void $ runMaybeT $ fetchSlide this state nextProps
        }


diagTreeEditorSlideEditor :: ReactClass { appContext :: AppContext }
diagTreeEditorSlideEditor = go where
  go = storeConnect f diagTreeEditorSlideEditorRender

  f appState
    = let branch = appState.diagTree.editor in merge
    { slide:
        branch.selectedSlideBranch >>= getSlideByBranch branch.slides

    , slidePath: branch.selectedSlideBranch

    -- This component supposed to be rendered only when any upper editor
    -- processing is done, so we check processing only of slide editing
    -- actions.
    , isProcessing: branch.slideSaving.isProcessing

    , isFailed: branch.slideSaving.isFailed
    }


-- For debouncing `onChange` triggering (for optimization purposes)
rteWrap
  :: ReactClass { appContext   :: AppContext
                , slideId      :: DiagTreeSlideId
                , isProcessing :: Boolean
                , value        :: String -- ^ Markdown
                , onChange     :: EventHandler String
                }

rteWrap = createClass $ spec $
  \ { isProcessing } { onRTEChange, rteValue } -> pure $

  flip rteEl mempty (richTextEditorDefaultProps rteValue)
    { placeholder = toNullable $ Just "Описание"
    , disabled    = toNullable $ Just isProcessing
    , onChange    = toNullable $ Just $ handle onRTEChange
    }

  where
    name = "DiagTreeEditorSlideEditorRTEWrap"
    wrapper = div [className $ name <.> "form-group"]
    rteEl = createElement richTextEditor

    onRTEChangeHandler this value = do
      { changeDebouncer } <- readState this
      let valueStr = valueToString value Markdown
      transformState this _ { rteValue = value }
      sendToDebouncer changeDebouncer valueStr

    getInitialState this = do
      { appContext, value } <- getProps this
      rteValue <- createValueFromString value Markdown
      changeDebouncer <- newDebouncer 500

      pure { changeDebouncer
           , changeSubscription : Nothing
           , onRTEChange        : onRTEChangeHandler this
           , rteValue
           }

    spec renderFn = x where
      renderHandler this =
        map wrapper $ renderFn <$> getProps this <*> readState this

      x = spec' getInitialState renderHandler # _
        { displayName = name

        , componentWillMount = \this -> do
            { onChange } <- getProps this
            { changeDebouncer } <- readState this

            subscription <-
              changeDebouncer `subscribeToDebouncer` callEventHandler onChange

            transformState this _ { changeSubscription = Just subscription }

        , componentWillReceiveProps = \this { value: newValue } -> do
            { changeDebouncer, rteValue } <- readState this
            let oldValue = valueToString rteValue Markdown

            if oldValue == newValue
               then pure unit
               else do newRTEValue <- createValueFromString newValue Markdown
                       sendToDebouncer changeDebouncer newValue
                       transformState this _ { rteValue = newRTEValue }

        -- This fixes a bug when you edit RTE value and change other value of a
        -- field really quick before debounced `onChange` is triggered and then
        -- your RTE value changes is resetted.
        , componentDidUpdate = \this { value: prevValue
                                     , slideId: prevSlideId
                                     , onChange
                                     } _ -> do

            { value, slideId } <- getProps this
            { rteValue } <- readState this
            let rteValueStr = valueToString rteValue Markdown

            if slideId == prevSlideId && -- If another slide selected we musn't
                                         -- trigger a change that was indended
                                         -- for the previous one.
               value == prevValue && -- If a value updated for some reason, for
                                     -- example slide changes was resetted, we
                                     -- don't trigger a change.
               value /= rteValueStr -- If RTE value and slide value are the same
                                    -- no need to trigger a change.
               then callEventHandler onChange $ rteValueStr
               else pure unit

        , componentWillUnmount = \this -> do
            { changeDebouncer, changeSubscription } <- readState this

            case changeSubscription of
                 Nothing -> pure unit
                 Just x' -> unsubscribeFromDebouncer changeDebouncer x'
        }
