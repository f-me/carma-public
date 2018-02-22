module App.Store.DiagTree.Editor.TreeSearch.Reducers
     ( DiagTreeEditorTreeSearchState
     , diagTreeEditorTreeSearchInitialState
     , diagTreeEditorTreeSearchReducer
     ) where

import Prelude

import Data.Maybe (Maybe (..))
import Data.String (trim)
import Data.String.NonEmpty (NonEmptyString, fromString, toString)

import App.Store.DiagTree.Editor.TreeSearch.Actions
     ( DiagTreeEditorTreeSearchAction (..)
     )


type DiagTreeEditorTreeSearchState =
  { searchQuery :: Maybe NonEmptyString
  }

diagTreeEditorTreeSearchInitialState :: DiagTreeEditorTreeSearchState
diagTreeEditorTreeSearchInitialState =
  { searchQuery : Nothing
  }


diagTreeEditorTreeSearchReducer
  :: DiagTreeEditorTreeSearchState
  -> DiagTreeEditorTreeSearchAction
  -> Maybe DiagTreeEditorTreeSearchState

diagTreeEditorTreeSearchReducer state (SearchByQuery q) = do
  cleanQuery <- toString q # trim # fromString

  case state.searchQuery of
       Nothing -> Just state { searchQuery = Just cleanQuery }
       Just x  -> if x /= cleanQuery
                     then Just state { searchQuery = Just cleanQuery }
                     else Nothing

diagTreeEditorTreeSearchReducer state ResetSearch =
  state.searchQuery <#> const state { searchQuery = Nothing }
