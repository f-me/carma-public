module App.Store.DiagTree
     ( DiagTreeState
     , DiagTreeAction (..)
     , diagTreeInitialState
     , diagTreeReducer
     ) where

import Prelude

import Data.Maybe (Maybe)

import App.Store.DiagTree.Editor
     ( DiagTreeEditorState
     , DiagTreeEditorAction
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


data DiagTreeAction
  = Editor DiagTreeEditorAction

diagTreeReducer :: DiagTreeState -> DiagTreeAction -> Maybe DiagTreeState
diagTreeReducer state (Editor x) =
  diagTreeEditorReducer state.editor x <#> state { editor = _ }
