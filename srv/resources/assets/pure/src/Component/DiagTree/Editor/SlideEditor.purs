module Component.DiagTree.Editor.SlideEditor
     ( diagTreeEditorSlideEditor
     ) where

import Prelude hiding (div)

import Data.Tuple (Tuple (Tuple), fst, snd)
import Data.Foldable (foldM)
import Data.Maybe (Maybe (..), isJust, maybe, fromMaybe)
import Data.Either (Either (..))
import Data.Map as Map
import Data.Nullable (toNullable)
import Data.Array ((!!), index, snoc, updateAt, modifyAt, deleteAt, null)

import Effect.Uncurried (mkEffectFn1)

import Record.Builder (merge)

import Control.Monad.Maybe.Trans (runMaybeT)
import Control.MonadZero (guard)
import Control.Alt ((<|>))

import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Aff (launchAff_)

import React.DOM (text, form, div, div', input, button, p', span)

import React.DOM.Props
     ( className, _type, placeholder, value, disabled
     , onChange, onClick
     )

import React
     ( ReactClass, component, createLeafElement
     , getProps, getState, modifyState
     )

import Utils ((<.>), storeConnect, toMaybeT, eventInputValue)

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

diagTreeEditorSlideEditorRender = defineComponent $
  \ { onChangeHeader, onChangeBody
    , updateResource, onResourceMoveUp, onResourceMoveDown
    , updateAnswer,   onAnswerMoveUp,   onAnswerMoveDown
    , onSelectAction, onCancel,         onSave
    }
    { appContext, isProcessing, isFailed }
    { newAnswers, isChanged }
    ( DiagTreeSlide slide ) ->

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
      input
        [ className $ "form-control" <.> classSfx "header"
        , _type "text"
        , placeholder "Заголовок"
        , disabled isProcessing
        , value slide.header
        , onChange onChangeHeader
        ]

  , rteWrapEl
      { appContext
      , slideId: slide.id
      , isProcessing
      , value: slide.body
      , onChange: onChangeBody
      }

  , resourcesEl
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
              answersEl
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
              actionEl
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

    rteWrapEl   = createLeafElement rteWrap
    resourcesEl = createLeafElement diagTreeEditorSlideEditorResources
    answersEl   = createLeafElement diagTreeEditorSlideEditorAnswers
    actionEl    = createLeafElement diagTreeEditorSlideEditorAction

    changeHeaderHandler this event = go where
      updater x' (DiagTreeSlide s) = DiagTreeSlide s { header = x' }

      go = do
        x <- eventInputValue event
        modifyState this
          \s -> s { slide = s.slide <#> updater x, isChanged = true }

    changeBodyHandler this valueStr = do
      modifyState this \s ->
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

      go =
        modifyState this
          \s -> s { slide = s.slide <#> updater, isChanged = true }

    updateResourceHandler this (NewItem resource) =
      modifyState this \s ->
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
      modifyState this \s ->
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
      modifyState this \s ->
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
      modifyState this \s ->
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
      modifyState this \s ->
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
      modifyState this \s ->
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
      modifyState this \s -> s
        { isChanged = true

        , newAnswers = s.newAnswers `snoc`
            { header     : answer.header
            , text       : answer.text
            , attachment : Modern <$> answer.attachment
            }
        }

    -- Deleting existing answer
    updateAnswerHandler this (DeleteItem (Left slideId)) =
      modifyState this \s ->
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
      modifyState this \s ->
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
      modifyState this \s ->
        case itemIndex `deleteAt` s.newAnswers of
             Nothing -> s
             Just x  -> s { isChanged = true, newAnswers = x }

    -- Updating new added answer
    updateAnswerHandler this (ChangeItem (Right itemIndex) answer) =
      modifyState this \s ->
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
      { slide, newAnswers } <- getState this

      case Tuple <$> slidePath <*> slide of
           Nothing -> pure unit
           Just (Tuple x y) -> launchAff_ $
             dispatch appContext $ DiagTree $ Editor $
               SaveSlideRequest x { slide: y, newAnswers }

    resetChanges this slide =
      modifyState this _
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
         else liftEffect $ resetChanges this props.slide

    defineComponent renderFn = component name \this -> do
      let preBound =
            { onChangeHeader     : changeHeaderHandler   this
            , onChangeBody       : changeBodyHandler     this
            , updateResource     : updateResourceHandler this
            , onResourceMoveUp   : moveResourceHandler   this true
            , onResourceMoveDown : moveResourceHandler   this false
            , updateAnswer       : updateAnswerHandler   this
            , onAnswerMoveUp     : moveAnswerHandler     this true
            , onAnswerMoveDown   : moveAnswerHandler     this false
            , onSelectAction     : selectActionHandler   this
            , onCancel           : cancelHandler         this
            , onSave             : saveHandler           this
            }

      state <-
        getProps this <#> \ { slide } ->
          { slide
          , newAnswers: []
            -- ^ It has no `nextSlide` yet, that's why it's separated.
          , isChanged: false
          }

      let r = renderFn preBound

      pure
        { state

        , render: do
            s <- getState this

            map wrapper $
              case s.slide of
                   Nothing -> pure [text "…"]
                   Just x' -> getProps this <#> \props -> r props s x'

        , unsafeComponentWillReceiveProps: \nextProps -> do
            s <- getState this
            void $ runMaybeT $ fetchSlide this s nextProps
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
                , onChange     :: String -> Effect Unit
                }

rteWrap = defineComponent $
  \ { onRTEChange } { isProcessing } { rteValue } -> pure $

  rteEl (richTextEditorDefaultProps rteValue)
    { placeholder = toNullable $ Just "Описание"
    , disabled    = toNullable $ Just isProcessing
    , onChange    = toNullable $ Just onRTEChange
    }

  where
    name = "DiagTreeEditorSlideEditorRTEWrap"
    wrapper = div [className $ name <.> "form-group"]
    rteEl = createLeafElement richTextEditor

    onRTEChangeHandler this changeDebouncer value = do
      let valueStr = valueToString value Markdown
      modifyState this _ { rteValue = value }
      sendToDebouncer changeDebouncer valueStr

    defineComponent renderFn = component name \this -> do
      changeDebouncer <- newDebouncer 500

      let preBound =
            { onRTEChange: mkEffectFn1 $ onRTEChangeHandler this changeDebouncer
            }

      state <- do
        rteValue <-
          getProps this <#> _.value >>= flip createValueFromString Markdown

        pure
          { changeSubscription: Nothing
          , rteValue
          }

      let r = renderFn preBound

      pure
        { state
        , render: map wrapper $ r <$> getProps this <*> getState this

        , unsafeComponentWillMount: do
            { onChange } <- getProps this
            subscription <- changeDebouncer `subscribeToDebouncer` onChange
            modifyState this _ { changeSubscription = Just subscription }

        , unsafeComponentWillReceiveProps: \ { value: newValue } -> do
            { rteValue } <- getState this
            let oldValue = valueToString rteValue Markdown

            if oldValue == newValue
               then pure unit
               else do newRTEValue <- createValueFromString newValue Markdown
                       sendToDebouncer changeDebouncer newValue
                       modifyState this _ { rteValue = newRTEValue }

        , -- This fixes a bug when you edit RTE value and change other value of
          -- a field really quick before debounced `onChange` is triggered and
          -- then your RTE value changes is resetted.
          componentDidUpdate: \ { value: prevValue
                                , slideId: prevSlideId
                                , onChange
                                } _ _ -> do

            { value, slideId } <- getProps this
            { rteValue } <- getState this
            let rteValueStr = valueToString rteValue Markdown

            if slideId == prevSlideId && -- If another slide selected we musn't
                                         -- trigger a change that was indended
                                         -- for the previous one.
               value == prevValue && -- If a value updated for some reason, for
                                     -- example slide changes was resetted, we
                                     -- don't trigger a change.
               value /= rteValueStr -- If RTE value and slide value are the same
                                    -- no need to trigger a change.
               then onChange rteValueStr
               else pure unit

        , componentWillUnmount: do
            { changeSubscription } <- getState this
            maybe (pure unit) unsubscribeFromDebouncer changeSubscription
        }
