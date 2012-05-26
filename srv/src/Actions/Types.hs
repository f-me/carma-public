
module Actions.Types where

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Set (Set)
import Data.Map (Map)

import Snap.Snaplet.Redson.Snapless.Metamodel


data Action = Action
  { a'on  :: Map ByteString (Set FieldValue)
  , a'new :: [Map FieldName Template]
  , a'set :: Map ByteString Template
  , a'close :: Bool
  }

newtype Template = Template [TmpPart]
data TmpPart = Str Text | Expr Expr

data Expr
  = Var ByteString
  | TimeOffset Int -- UTCDiff
  | Call ByteString Expr
