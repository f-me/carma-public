module App.Store.DiagTree.Reducers
     ( DiagTreeState
     , diagTreeInitialState
     , diagTreeReducer
     ) where

import Prelude

import Data.Maybe (Maybe)

import App.Store.DiagTree.Actions (DiagTreeAction (..))

import App.Store.DiagTree.Editor.Reducers
     ( DiagTreeEditorState
     , diagTreeEditorInitialState
     , diagTreeEditorReducer
     )


type DiagTreeState =
  { editor :: DiagTreeEditorState
  }

diagTreeInitialState :: DiagTreeState
diagTreeInitialState =
  { editor: diagTreeEditorInitialState
  }


diagTreeReducer :: DiagTreeState -> DiagTreeAction -> Maybe DiagTreeState
diagTreeReducer state (Editor x) =
  diagTreeEditorReducer state.editor x <#> state { editor = _ }
