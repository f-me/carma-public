module App.Store.DiagTree.Actions
     ( DiagTreeAction (..)
     ) where

import App.Store.DiagTree.Editor.Actions (DiagTreeEditorAction)


data DiagTreeAction
  = Editor DiagTreeEditorAction
