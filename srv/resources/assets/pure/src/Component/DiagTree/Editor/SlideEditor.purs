module Component.DiagTree.Editor.SlideEditor
     ( diagTreeEditorSlideEditor
     ) where

import Prelude hiding (div)

import Control.Monad.Aff (launchAff_)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Alt ((<|>))

import Data.Tuple (Tuple (Tuple), snd)
import Data.Array ((!!), snoc, updateAt, modifyAt, deleteAt)
import Data.Foldable (class Foldable, foldl)
import Data.Record.Builder (merge)
import Data.Maybe (Maybe (..), isJust, maybe)
import Data.Either (Either (..))
import Data.Map as Map
import Data.Nullable (toNullable)

import React.DOM (form, div) as R
import React.Spaces ((!), (!.), (^), renderIn, text, empty, elements)
import React.Spaces.DOM (div, input, button, textarea, label, i)
import React.Spaces.DOM.Dynamic (ul) as SDyn

import React.DOM.Props
     ( className, _type, placeholder, value, disabled
     , onChange, onClick
     )

import React
     ( ReactClass, ReactProps, ReactState, ReactRefs, ReadWrite, ReadOnly
     , createClass, spec', createElement
     , getProps, readState, transformState
     )

import Utils
     ( (<.>), storeConnect, toMaybeT, eventInputValue
     , createClassStatelessWithName
     , unfoldrBoundedEnum
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

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (SaveSlideRequest)
     )

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlide (DiagTreeSlide)
     , DiagTreeSlideId
     , DiagTreeSlideResource
     , DiagTreeSlideAttachment (Modern)
     , DiagTreeSlideAction
     , DiagTreeSlideAnswer
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
  => ReactClass { appContext :: AppContext
                , slideId    :: DiagTreeSlideId
                , isDisabled :: Boolean
                , resources  :: f DiagTreeSlideResource

                , updateResource -- See item component for details
                    :: Maybe Int

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
                }

resourcesRender = createClass $ spec $
  \ { appContext, slideId, isDisabled, resources, updateResource }
    { isAdding, turnAddingOn, turnAddingOff } -> do

  label !. "control-label" $ text "Картинки"

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
            }

     else button !. "btn btn-default" <.> classSfx "add-button"
                 ! _type "button"
                 ! onClick turnAddingOn
                 ! disabled isDisabled
                 $ do

            i !. "glyphicon glyphicon-plus" $ empty
            text " Добавить картинку"

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
  => ReactClass { appContext :: AppContext
                , slideId    :: DiagTreeSlideId
                , isDisabled :: Boolean
                , answers    :: f DiagTreeSlideAnswer

                , newAnswers :: f2 { header     :: String
                                   , text       :: String
                                   , attachment :: Maybe DiagTreeSlideAttachment
                                   }

                , updateAnswer -- See item component for details
                    :: Maybe (Either DiagTreeSlideId Int)

                    -> Maybe { header     :: String
                             , text       :: String
                             , attachment :: Maybe { id       :: Int
                                                   , hash     :: String
                                                   , filename :: String
                                                   }
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

  dropDownSelect ^
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
       }

diagTreeEditorSlideEditorRender = createClass $ spec $
  \ { appContext, isProcessing }
    { slide: (DiagTreeSlide slide)
    , newAnswers
    , isChanged
    , onChangeHeader
    , onChangeBody
    , updateResource
    , updateAnswer
    , onSelectAction
    , onCancel
    , onSave
    } -> do

  div !. "form-group" $
    input !. "form-control" <.> classSfx "header"
          ! _type "text"
          ! placeholder "Заголовок"
          ! value slide.header
          ! onChange onChangeHeader

  div !. "form-group" $
    -- TODO write bindings to RichTextEditor (react-rte) and use it here
    textarea !. "form-control"
             ! placeholder "Описание"
             ! value slide.body
             ! onChange onChangeBody

  resourcesRender ^
    { appContext
    , slideId: slide.id
    , isDisabled: isProcessing
    , resources: slide.resources
    , updateResource
    }

  -- Rendering "answers" only if "action" is not set
  if isJust slide.action
     then empty
     else answersRender ^ { appContext
                          , slideId: slide.id
                          , isDisabled: isProcessing
                          , answers: slide.answers
                          , newAnswers
                          , updateAnswer
                          }

  -- Rendering "action" only if "answers" is empty
  if not $ Map.isEmpty slide.answers
     then empty
     else actionRender ^ { appContext
                         , isDisabled: isProcessing
                         , action: slide.action
                         , onSelected: onSelectAction
                         }

  div !. "btn-toolbar" $ do

    let isBlocked = isProcessing || not isChanged

    button !. "btn btn-default"
           ! _type "button"
           ! disabled isBlocked
           ! onClick onCancel
           $ text "Отменить изменения"

    button !. "btn btn-success"
           ! _type "button"
           ! disabled isBlocked
           ! onClick onSave
           $ text "Сохранить"

    -- TODO save message here

  where
    name = "DiagTreeEditorSlideEditor"
    classSfx s = name <> "--" <> s
    wrapper = R.form [className name]
    renderer = renderIn wrapper

    changeHeaderHandler this event = do
      let x = eventInputValue event

      transformState this $
        \s -> s { slide = s.slide <#> updater x, isChanged = true }

      where updater x (DiagTreeSlide s) = DiagTreeSlide $ s { header = x }

    changeBodyHandler this event = do
      let x = eventInputValue event

      transformState this $
        \s -> s { slide = s.slide <#> updater x, isChanged = true }

      where updater x (DiagTreeSlide s) = DiagTreeSlide $ s { body = x }

    selectActionHandler this action =
      transformState this $
        \s -> s { slide = s.slide <#> updater, isChanged = true }

      where updater (DiagTreeSlide s) = DiagTreeSlide $ s { action = action }

    updateResourceHandler this itemIndex resource = transformState this $ \s ->
      let
        newSlide =
          case resource <#> Tuple itemIndex of
               -- `Nothing` means deleting a resource
               Nothing -> do
                 (DiagTreeSlide slide) <- s.slide
                 idx <- itemIndex

                 DiagTreeSlide <$> slide { resources = _ }
                               <$> deleteAt idx slide.resources

               -- Updating an existing resource
               Just (Tuple (Just idx) x) -> do
                 (DiagTreeSlide slide) <- s.slide
                 old <- slide.resources !! idx

                 let newResource =
                       { text       : x.text
                       , attachment : maybe old.attachment Modern x.file
                       }

                 DiagTreeSlide <$> slide { resources = _ }
                               <$> updateAt idx newResource slide.resources

               -- Adding a new resources
               Just (Tuple Nothing x) -> do
                 (DiagTreeSlide slide) <- s.slide

                 newResource <-
                   x.file <#> Modern <#> { text: x.text, attachment: _ }

                 pure $ DiagTreeSlide
                      $ slide { resources = _ }
                      $ slide.resources `snoc` newResource
      in
        s { slide = newSlide <|> s.slide, isChanged = isJust newSlide }

    -- Adding new answer
    updateAnswerHandler this Nothing (Just answer) =
      transformState this $ \s -> s
        { isChanged = true

        , newAnswers = s.newAnswers `snoc`
            { header     : answer.header
            , text       : answer.text
            , attachment : Modern <$> answer.attachment
            }
        }

    -- Deleting existing answer
    updateAnswerHandler this (Just (Left nextSlideId)) Nothing =
      transformState this $ \s ->
        let
          f (DiagTreeSlide slide) = DiagTreeSlide $
            slide { answers = nextSlideId `Map.delete` slide.answers }

          newSlide = s.slide <#> f
        in
          s { slide = newSlide <|> s.slide, isChanged = isJust newSlide }

    -- Updating existing answer
    updateAnswerHandler this (Just (Left nextSlideId)) (Just answer) =
      transformState this $ \s ->
        let
          updater = Just <<< _
            { header     = answer.header
            , text       = answer.text
            , attachment = Modern <$> answer.attachment
            }

          f (DiagTreeSlide slide) = DiagTreeSlide $
            slide { answers = Map.update updater nextSlideId slide.answers }

          newSlide = s.slide <#> f
        in
          s { slide = newSlide <|> s.slide, isChanged = isJust newSlide }

    -- Deleting new added answer
    updateAnswerHandler this (Just (Right itemIndex)) Nothing =
      transformState this $ \s ->
        case itemIndex `deleteAt` s.newAnswers of
             Nothing -> s
             Just x  -> s { isChanged = true, newAnswers = x }

    -- Updating new added answer
    updateAnswerHandler this (Just (Right itemIndex)) (Just answer) =
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

    -- Unexpected behavior (may be just better type it?)
    updateAnswerHandler this Nothing Nothing = pure unit

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

    resetChanges this slide =
      transformState this _
        { slide = slide, newAnswers = [], isChanged = false }

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

    getInitialState this = do
      { slide } <- getProps this

      pure { slide

           , newAnswers: [] -- It hasn't `nextSlide` yet,
                            -- that's why it's separated.

           , isChanged      : false
           , onChangeHeader : changeHeaderHandler   this
           , onChangeBody   : changeBodyHandler     this
           , updateResource : updateResourceHandler this
           , updateAnswer   : updateAnswerHandler   this
           , onSelectAction : selectActionHandler   this
           , onCancel       : cancelHandler         this
           , onSave         : saveHandler           this
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
      }
