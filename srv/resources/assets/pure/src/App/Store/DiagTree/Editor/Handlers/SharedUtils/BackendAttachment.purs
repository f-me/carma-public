-- Generalized attachment converters (both for "resources" and "answers").
module App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAttachment
     ( BackendAttachment
     , BackendAttachmentFields

     , module
         App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAttachment.Types

     , fromBackendAttachment
     , toBackendAttachment
     ) where

import Prelude
import Prim.Row (class Cons)

import Data.Int (fromNumber, toNumber)
import Data.Tuple (Tuple (Tuple))
import Data.Maybe (Maybe (..), isJust)
import Data.Set (Set)
import Data.Set as Set
import Data.Argonaut.Core as A
import Data.Symbol (SProxy (SProxy), class IsSymbol, reflectSymbol)
import Data.String.Read (read)
import Foreign.Object as FObj

import Record (get)

import Control.MonadZero (guard)

import App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAttachment.Types
     ( BackendAttachmentMediaType (..)
     )


type BackendAttachment = Record BackendAttachmentFields

type BackendAttachmentFields =
   ( id        :: Int
   , hash      :: String
   , filename  :: String
   , mediaType :: BackendAttachmentMediaType
   )

class AttachmentKeyToBackendKey k where
  resourceAttachmentKeyToBackendKey
    :: forall a r'
     . Cons k a r' BackendAttachmentFields
    => IsSymbol k
    => SProxy k -> String

instance resourceAttachmentKeyToBackendKeyGeneric
  :: IsSymbol k => AttachmentKeyToBackendKey k
  where
  resourceAttachmentKeyToBackendKey = reflectSymbol

backendAttachmentValidKeys :: Set String
backendAttachmentValidKeys = Set.fromFoldable
  [ k (SProxy :: SProxy "id")
  , k (SProxy :: SProxy "hash")
  , k (SProxy :: SProxy "filename")
  , k (SProxy :: SProxy "mediaType")
  , "ctime"
  ]
  where
    k = resourceAttachmentKeyToBackendKey


fromBackendAttachment :: A.Json -> Maybe BackendAttachment
fromBackendAttachment json = do
  obj <- A.toObject json

  guard $
    Set.fromFoldable (FObj.keys obj) `Set.subset` backendAttachmentValidKeys

  let l :: forall k a r'
         . Cons k a r' BackendAttachmentFields
        => IsSymbol k
        => AttachmentKeyToBackendKey k
        => SProxy k -> Maybe A.Json

      l key = resourceAttachmentKeyToBackendKey key `FObj.lookup` obj

  id       <- l (SProxy :: SProxy "id")       >>= A.toNumber >>= fromNumber
  hash     <- l (SProxy :: SProxy "hash")     >>= A.toString
  filename <- l (SProxy :: SProxy "filename") >>= A.toString

  mediaType <- let x = l (SProxy :: SProxy "mediaType") in
    if isJust x
       then x >>= A.toString >>= read
       else Just ImageMediaType -- If `mediaType` isn't set

  pure { id, hash, filename, mediaType }


toBackendAttachment :: BackendAttachment -> A.Json
toBackendAttachment y = A.fromObject $ FObj.fromFoldable
  [ fAtt y (SProxy :: SProxy "id")        $ toNumber >>> A.fromNumber
  , fAtt y (SProxy :: SProxy "hash")      A.fromString
  , fAtt y (SProxy :: SProxy "filename")  A.fromString
  , fAtt y (SProxy :: SProxy "mediaType") $ show >>> A.fromString
  ]

  where
    fAtt -- "Att" is for "Attachment"
      :: forall k a r'
       . Cons k a r' BackendAttachmentFields
      => IsSymbol k
      => AttachmentKeyToBackendKey k
      => BackendAttachment
      -> SProxy k -> (a -> A.Json) -> Tuple String A.Json

    fAtt record key converter =
      Tuple (resourceAttachmentKeyToBackendKey key) $
        converter $ key `get` record
