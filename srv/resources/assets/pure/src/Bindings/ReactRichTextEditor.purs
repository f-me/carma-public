-- See https://github.com/sstur/react-rte
module Bindings.ReactRichTextEditor
     ( Props
     , RTE
     , EditorValue
     , EditorValueFormat (..)
     , richTextEditor
     , richTextEditorDefaultProps
     , createEmptyValue
     , createValueFromString
     , valueToString
     ) where

import Prelude

import Control.Monad.Eff (kind Effect, Eff)

import Data.Generic (class Generic, gShow)
import Data.Nullable (Nullable, toNullable)
import Data.Maybe (Maybe (..))

import React (ReactClass, EventHandler)


foreign import data RTE :: Effect
foreign import data EditorValue :: Type

type Props rootStyle editorStyle toolbarStyle =
  { className        :: Nullable String -- className?: string;
  , toolbarClassName :: Nullable String -- toolbarClassName?: string;
  , editorClassName  :: Nullable String -- editorClassName?: string;
  , value            :: EditorValue -- value: EditorValue;
  , onChange         :: Nullable (EventHandler EditorValue)
                     -- onChange?: ChangeHandler;
                     -- type ChangeHandler = (value: EditorValue) => any;
  , placeholder      :: Nullable String -- placeholder?: string;
  -- customStyleMap?: {[style: string]: {[key: string]: any}};
  -- handleReturn?: (event: Object) => boolean;
  -- customControls?: Array<CustomControl>;
  , readOnly         :: Nullable Boolean -- readOnly?: boolean;
  , disabled         :: Nullable Boolean
                        -- disabled?: boolean; // Alias of readOnly
  -- toolbarConfig?: ToolbarConfig;
  -- blockStyleFn?: (block: ContentBlock) => ?string;
  , autoFocus        :: Nullable Boolean -- autoFocus?: boolean;
  -- keyBindingFn?: (event: Object) => ?string;
  , rootStyle        :: Nullable (Record rootStyle) -- rootStyle?: Object;
  , editorStyle      :: Nullable (Record editorStyle) -- editorStyle?: Object;
  , toolbarStyle     :: Nullable (Record toolbarStyle) -- toolbarStyle?: Object;
  }

richTextEditorDefaultProps :: EditorValue -> Props () () ()
richTextEditorDefaultProps value =
  { className        : toNullable Nothing
  , toolbarClassName : toNullable Nothing
  , editorClassName  : toNullable Nothing
  , value
  , onChange         : toNullable Nothing
  , placeholder      : toNullable Nothing
  -- customStyleMap?: {[style: string]: {[key: string]: any}};
  -- handleReturn?: (event: Object) => boolean;
  -- customControls?: Array<CustomControl>;
  , readOnly         : toNullable Nothing
  , disabled         : toNullable Nothing
  -- toolbarConfig?: ToolbarConfig;
  -- blockStyleFn?: (block: ContentBlock) => ?string;
  , autoFocus        : toNullable Nothing
  -- keyBindingFn?: (event: Object) => ?string;
  , rootStyle        : toNullable Nothing
  , editorStyle      : toNullable Nothing
  , toolbarStyle     : toNullable Nothing
  }


foreign import richTextEditor
  :: forall rootStyle editorStyle toolbarStyle
   . ReactClass (Props rootStyle editorStyle toolbarStyle)

foreign import createEmptyValue
  :: forall eff. Eff (rte :: RTE | eff) EditorValue

foreign import createValueFromStringRaw
  :: forall eff. String -> String -> Eff (rte :: RTE | eff) EditorValue

createValueFromString
  :: forall eff
   . String
  -> EditorValueFormat
  -> Eff (rte :: RTE | eff) EditorValue

createValueFromString markup = createValueFromStringRaw markup <<< rawFormat

foreign import valueToStringRaw :: EditorValue -> String -> String

valueToString :: EditorValue -> EditorValueFormat -> String
valueToString value = valueToStringRaw value <<< rawFormat

data EditorValueFormat = HTML | Markdown | RAW
derive instance eqEditorValueFormat :: Eq EditorValueFormat
derive instance genericEditorValueFormat :: Generic EditorValueFormat
instance showEditorValueFormat :: Show EditorValueFormat where show = gShow

rawFormat :: EditorValueFormat -> String
rawFormat HTML     = "html"
rawFormat Markdown = "markdown"
rawFormat RAW      = "raw"
