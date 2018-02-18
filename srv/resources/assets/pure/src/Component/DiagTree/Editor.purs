module Component.DiagTree.Editor
     ( diagTreeEditor
     ) where

import Prelude hiding (div)

import Data.Maybe (Maybe (..))
import Data.Array (snoc, fromFoldable)
import Data.Foldable (foldlDefault)
import Data.Record.Builder (merge)

import Control.Monad.Aff (launchAff_)

import React (ReactClass, getProps, createElement)
import React.DOM.Props (className)

import React.DOM
     ( IsDynamic (IsDynamic)
     , mkDOM, text, div', div, p', span, b', button, i
     )

import Utils (createClassStatelessWithSpec, storeConnect)
import Component.Spinner (spinner)
import App.Store (AppContext, dispatch)
import App.Store.Actions (AppAction (DiagTree))
import App.Store.DiagTree.Actions (DiagTreeAction (Editor))
import App.Store.DiagTree.Editor.Types (DiagTreeSlides, DiagTreeSlideId)

import App.Store.DiagTree.Editor.Actions
     ( DiagTreeEditorAction (LoadSlidesRequest)
     )



diagTreeEditorRender
  :: ReactClass { isSlidesLoading           :: Boolean
                , isSlidesLoaded            :: Boolean
                , isSlidesLoadingFailed     :: Boolean
                , isParsingSlidesDataFailed :: Boolean
                , appContext                :: AppContext
                , slides                    :: DiagTreeSlides
                , selectedSlide             :: Maybe DiagTreeSlideId
                }

diagTreeEditorRender = f $ \props ->
  [ div [className "container"]
    [ div [className "row"]
      [ div [className $ "col-md-4 " <> classSfx "tree-panel"]
        [ div [className "btn-toolbar"]
          [ button [className "btn btn-success"]
            [ i [className "glyphicon glyphicon-plus"] []
            , text " Новое дерево"
            ]
          ]
        ]
      , div [className $ "col-md-8 " <> classSfx "slide-editor-panel"] $
        map (renderSlide props) $ fromFoldable props.slides
      ]
    ]
  ]

  where
    wrapper = mkDOM (IsDynamic false) name []
    f render = spec $ \props -> wrapper $ branching render props
    classSfx s = name <> "--" <> s
    name = "diag-tree-editor"

    renderSlide props slide = div' $
      [ text ("id: " <> show slide.id)
      , text (" isRoot: " <> show slide.isRoot)
      ] <> case props.selectedSlide of
                Nothing -> []
                Just x  -> if x == slide.id
                              then [b' [text " SELECTED"]]
                              else []

    branching render props
      | props.isSlidesLoadingFailed =
          [ div'
            [ p'
              [ span [className "label label-danger"] [text "Ошибка"]
              , if props.isParsingSlidesDataFailed
                   then text " Произошла ошибка при обработке\
                             \ полученных от сервера данных"
                   else text " Произошла ошибка при загрузке данных"
              ]
            ]
          ]

      | props.isSlidesLoading =
          [ createElement spinner { withLabel: true
                                  , appContext: props.appContext
                                  } []
          ]

      | props.isSlidesLoaded = render props
      | otherwise =
          [ div'
            [ p'
              [ span [className "label label-warning"] [text "Ожидание"]
              , text " Данные ещё не загружены…"
              ]
            ]
          ]

    spec = createClassStatelessWithSpec $ _
      { displayName = "DiagTreeEditor"

      , componentDidMount = \this -> do
          props <- getProps this

          if props.isSlidesLoaded || props.isSlidesLoading
             then pure unit
             else launchAff_
                $ dispatch props.appContext
                $ DiagTree $ Editor $ LoadSlidesRequest
      }


diagTreeEditor :: ReactClass { appContext :: AppContext }
diagTreeEditor = storeConnect f diagTreeEditorRender
  where
    f appState = merge $ let branch = appState.diagTree.editor in
      { isSlidesLoading           : branch.isSlidesLoading
      , isSlidesLoaded            : branch.isSlidesLoaded
      , isSlidesLoadingFailed     : branch.isSlidesLoadingFailed
      , isParsingSlidesDataFailed : branch.isParsingSlidesDataFailed
      , slides                    : branch.slides
      , selectedSlide             : branch.selectedSlide
      }
