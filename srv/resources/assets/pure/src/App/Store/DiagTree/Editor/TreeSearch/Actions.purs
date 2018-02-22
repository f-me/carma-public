module App.Store.DiagTree.Editor.TreeSearch.Actions
     ( DiagTreeEditorTreeSearchAction (..)
     ) where

import Data.String.NonEmpty (NonEmptyString)


data DiagTreeEditorTreeSearchAction
  = SearchByQuery NonEmptyString
  | ResetSearch
