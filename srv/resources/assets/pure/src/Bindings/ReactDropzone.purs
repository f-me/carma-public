module Bindings.ReactDropzone
     ( Props
     , dropzone
     , dropzoneDefaultProps
     , dropzoneName

     , Bytes
     , toBytes
     , bytesInfinity

     , EventHandler2
     , handle2

     , EventHandler3
     , handle3
     ) where

import Prelude

import Data.Maybe (Maybe (..))
import Data.Nullable (Nullable, toNullable)

import DOM.File.Types (File)
import React (ReactClass, EventHandlerContext, EventHandler, Event)


newtype Bytes = Bytes Int
foreign import bytesInfinity :: Bytes

toBytes :: Int -> Bytes
toBytes = Bytes

type Props inputProps style activeStyle acceptStyle rejectStyle disabledStyle =
  { accept                :: Nullable String -- PropTypes.string
  -- children: PropTypes.oneOfType([PropTypes.node, PropTypes.func])
  , disableClick          :: Boolean -- PropTypes.bool
  , disabled              :: Boolean -- PropTypes.bool
  , disablePreview        :: Boolean -- PropTypes.bool
  , preventDropOnDocument :: Boolean -- PropTypes.bool
  , inputProps            :: Nullable (Record inputProps) -- PropTypes.object
  , multiple              :: Boolean -- PropTypes.bool
  , name                  :: Nullable String -- PropTypes.string
  , maxSize               :: Bytes -- PropTypes.number
  , minSize               :: Bytes -- PropTypes.number
  , className             :: Nullable String -- PropTypes.string
  , activeClassName       :: Nullable String -- PropTypes.string
  , acceptClassName       :: Nullable String -- PropTypes.string
  , rejectClassName       :: Nullable String -- PropTypes.string
  , disabledClassName     :: Nullable String -- PropTypes.string
  , style                 :: Nullable (Record style) -- PropTypes.object
  , activeStyle           :: Nullable (Record activeStyle) -- PropTypes.object
  , acceptStyle           :: Nullable (Record acceptStyle) -- PropTypes.object
  , rejectStyle           :: Nullable (Record rejectStyle) -- PropTypes.object
  , disabledStyle         :: Nullable (Record disabledStyle) -- PropTypes.object
  , onClick               :: Nullable (EventHandler Event) -- PropTypes.func

  , onDrop                :: Nullable (EventHandler3 (Array File)
                                                     (Array File)
                                                     Event)
                          -- PropTypes.func

  , onDropAccepted        :: Nullable (EventHandler2 (Array File) Event)
                          -- PropTypes.func

  , onDropRejected        :: Nullable (EventHandler2 (Array File) Event)
                          -- PropTypes.func

  , onDragStart           :: Nullable (EventHandler Event) -- PropTypes.func
  , onDragEnter           :: Nullable (EventHandler Event) -- PropTypes.func
  , onDragOver            :: Nullable (EventHandler Event) -- PropTypes.func
  , onDragLeave           :: Nullable (EventHandler Event) -- PropTypes.func
  , onFileDialogCancel    :: Nullable (EventHandler Unit) -- PropTypes.func
  }

dropzoneDefaultProps :: Props () () () () () ()
dropzoneDefaultProps =
  { accept                : toNullable Nothing
  -- children
  , disableClick          : false
  , disabled              : false
  , disablePreview        : false
  , preventDropOnDocument : true
  , inputProps            : toNullable Nothing
  , multiple              : true
  , name                  : toNullable Nothing
  , maxSize               : bytesInfinity
  , minSize               : Bytes 0
  , className             : toNullable Nothing
  , activeClassName       : toNullable Nothing
  , acceptClassName       : toNullable Nothing
  , rejectClassName       : toNullable Nothing
  , disabledClassName     : toNullable Nothing
  , style                 : toNullable Nothing
  , activeStyle           : toNullable Nothing
  , acceptStyle           : toNullable Nothing
  , rejectStyle           : toNullable Nothing
  , disabledStyle         : toNullable Nothing
  , onClick               : toNullable Nothing
  , onDrop                : toNullable Nothing
  , onDropAccepted        : toNullable Nothing
  , onDropRejected        : toNullable Nothing
  , onDragStart           : toNullable Nothing
  , onDragEnter           : toNullable Nothing
  , onDragOver            : toNullable Nothing
  , onDragLeave           : toNullable Nothing
  , onFileDialogCancel    : toNullable Nothing
  }

  -- Default props from "react-dropzone":
  {-
    preventDropOnDocument: true
    disabled: false
    disablePreview: false
    disableClick: false
    multiple: true
    maxSize: Infinity
    minSize: 0
  -}

dropzoneName :: String
dropzoneName = "ReactDropzone"


foreign import dropzone
  :: forall inputProps style activeStyle acceptStyle rejectStyle disabledStyle
   . ReactClass ( Props inputProps style
                                   activeStyle
                                   acceptStyle
                                   rejectStyle
                                   disabledStyle )


foreign import data EventHandler2 :: Type -> Type -> Type

foreign import handle2
  :: forall a b eff props state result
   . (a -> b -> EventHandlerContext eff props state result)
  -> EventHandler2 a b


foreign import data EventHandler3 :: Type -> Type -> Type -> Type

foreign import handle3
  :: forall a b c eff props state result
   . (a -> b -> c -> EventHandlerContext eff props state result)
  -> EventHandler3 a b c
