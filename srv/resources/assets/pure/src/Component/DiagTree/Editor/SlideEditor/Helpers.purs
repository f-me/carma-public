module Component.DiagTree.Editor.SlideEditor.Helpers
     ( ItemModification (..)
     ) where


data ItemModification identity value
   = NewItem value
   | DeleteItem identity
   | ChangeItem identity value
