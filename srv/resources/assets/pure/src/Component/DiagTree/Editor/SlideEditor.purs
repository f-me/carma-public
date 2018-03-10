module Component.DiagTree.Editor.SlideEditor
     ( diagTreeEditorSlideEditor
     ) where

import Prelude hiding (div)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Maybe.Trans (runMaybeT)
import Control.Alt ((<|>))

import Data.Tuple (Tuple (Tuple), snd)
import Data.Array ((!!), snoc, updateAt, deleteAt)
import Data.Foldable (class Foldable, foldl)
import Data.Record.Builder (merge)
import Data.Maybe (Maybe (..), isJust, maybe)
import Data.Map as Map

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
     , preventDefault
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

import App.Store (AppContext)
import Component.Generic.DropDownSelect (OnSelectedEff, dropDownSelect)

import App.Store.DiagTree.Editor.Types
     ( DiagTreeSlide (DiagTreeSlide)
     , DiagTreeSlideResource
     , DiagTreeSlideResourceAttachment (Modern)
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
                , isDisabled :: Boolean
                , resources  :: f DiagTreeSlideResource

                , updateResource
                    :: Int

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

resourcesRender = createClassStatelessWithName name $
  \ { appContext, isDisabled, resources, updateResource } -> renderer $ do

  label !. "control-label" $ text "Картинки"

  SDyn.ul !. "list-group" <.> classSfx "list" $

    let itemReducer (Tuple itemIndex list) resource =
          Tuple (itemIndex + 1) $ list `snoc`

            let props = { appContext
                        , itemIndex
                        , isDisabled
                        , resource: Just resource
                        , updateResource
                        , key: show itemIndex
                        }

             in createElement diagTreeEditorSlideEditorResource props []

     in elements $ snd $ foldl itemReducer (Tuple 0 []) resources

  button !. "btn btn-default" <.> classSfx "add-button"
         ! _type "button"
         ! disabled isDisabled
         $ do

    i !. "glyphicon glyphicon-plus" $ empty
    text " Добавить картинку"

  where
    name = "DiagTreeEditorSlideEditorResources"
    classSfx s = name <> "--" <> s
    wrapper = R.div [className $ "form-group" <.> name]
    renderer = renderIn wrapper


answersRender
  :: forall f
   . Foldable f
  => ReactClass { appContext :: AppContext
                , isDisabled :: Boolean
                , answers    :: f DiagTreeSlideAnswer
                }

answersRender = createClassStatelessWithName name $
  \ { appContext, isDisabled, answers } -> renderer $ do

  label !. "control-label" $ text "Ответы"

  SDyn.ul !. "list-group" <.> classSfx "list" $

    let itemReducer (Tuple key list) answer =
          Tuple (key + 1) $ list `snoc`
            createElement diagTreeEditorSlideEditorAnswer
                          { appContext, answer, key: show key } []

     in elements $ snd $ foldl itemReducer (Tuple 0 []) answers

  button !. "btn btn-default" <.> classSfx "add-button"
         ! _type "button"
         ! disabled isDisabled
         $ do

    i !. "glyphicon glyphicon-plus" $ empty
    text " Добавить ответ"

  where
    name = "DiagTreeEditorSlideEditorAnswers"
    classSfx s = name <> "--" <> s
    wrapper = R.div [className $ "form-group" <.> name]
    renderer = renderIn wrapper


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
       , isProcessing :: Boolean
       }

diagTreeEditorSlideEditorRender = createClass $ spec $
  \ { appContext, isProcessing }
    { slide: (DiagTreeSlide slide)
    , isChanged
    , onChangeHeader
    , onChangeBody
    , updateResource
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
    , isDisabled: isProcessing
    , resources: slide.resources
    , updateResource
    }

  -- Rendering "answers" only if "action" is not set
  if isJust slide.action
     then empty
     else answersRender ^ { appContext
                          , isDisabled: isProcessing
                          , answers: slide.answers
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

    updateResourceHandler this idx resource =
      transformState this $ \s ->
        let
          newSlide =
            case resource of
                 -- `Nothing` means deleting a resource
                 Nothing -> do
                   (DiagTreeSlide slide) <- s.slide

                   DiagTreeSlide <$> slide { resources = _ }
                                 <$> deleteAt idx slide.resources

                 -- `Just` means updating a resource
                 Just x -> do
                   (DiagTreeSlide slide) <- s.slide
                   old <- slide.resources !! idx

                   let newResource =
                         { text       : x.text
                         , attachment : maybe old.attachment Modern x.file
                         }

                   DiagTreeSlide <$> slide { resources = _ }
                                 <$> updateAt idx newResource slide.resources
        in
          s { slide = newSlide <|> s.slide, isChanged = true }

    cancelHandler this event = do
      { slide } <- getProps this
      resetChanges this slide
      preventDefault event

    saveHandler this event = do
      -- TODO
      preventDefault event

    resetChanges this slide =
      transformState this _ { slide = slide, isChanged = false }

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
           , isChanged      : false
           , onChangeHeader : changeHeaderHandler   this
           , onChangeBody   : changeBodyHandler     this
           , updateResource : updateResourceHandler this
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

      -- This component supposed to be rendered only when any upper editor
      -- processing is done, so we check processing only of slide editing
      -- actions.
      , isProcessing: false
      }
