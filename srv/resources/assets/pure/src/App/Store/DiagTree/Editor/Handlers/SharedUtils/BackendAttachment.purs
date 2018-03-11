-- Generalized attachment converters (both for "resources" and "answers").
module App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAttachment
     ( BackendAttachment
     , BackendAttachmentFields
     , fromBackendAttachment
     , toBackendAttachment
     ) where

import Prelude hiding (id)

import Control.MonadZero (guard)

import Data.Int (fromNumber, toNumber)
import Data.Tuple (Tuple (Tuple))
import Data.Maybe (Maybe)
import Data.StrMap as StrMap
import Data.Set (Set)
import Data.Set as Set
import Data.Argonaut.Core as A
import Data.Record (get)
import Data.Symbol (SProxy (SProxy), class IsSymbol, reflectSymbol)


type BackendAttachment = Record BackendAttachmentFields

type BackendAttachmentFields =
  ( id       :: Int
  , hash     :: String
  , filename :: String
  )


class AttachmentKeyToBackendKey k where
  resourceAttachmentKeyToBackendKey
    :: forall a r'
     . RowCons k a r' BackendAttachmentFields
    => IsSymbol k
    => SProxy k -> String

instance resourceAttachmentKeyToBackendKeyGeneric
  :: (IsSymbol k) => AttachmentKeyToBackendKey k
  where
  resourceAttachmentKeyToBackendKey = reflectSymbol

backendAttachmentValidKeys :: Set String
backendAttachmentValidKeys = Set.fromFoldable
  [ k (SProxy :: SProxy "id")
  , k (SProxy :: SProxy "hash")
  , k (SProxy :: SProxy "filename")
  , "ctime"
  ]
  where
    k = resourceAttachmentKeyToBackendKey


fromBackendAttachment :: A.Json -> Maybe BackendAttachment
fromBackendAttachment json = do
  obj <- A.toObject json

  guard $
    Set.fromFoldable (StrMap.keys obj)
      `Set.subset` backendAttachmentValidKeys

  let l :: forall k a r'
         . RowCons k a r' BackendAttachmentFields
        => IsSymbol k
        => AttachmentKeyToBackendKey k
        => SProxy k -> Maybe A.Json

      l key = resourceAttachmentKeyToBackendKey key `StrMap.lookup` obj

  id       <- l (SProxy :: SProxy "id")       >>= A.toNumber >>= fromNumber
  hash     <- l (SProxy :: SProxy "hash")     >>= A.toString
  filename <- l (SProxy :: SProxy "filename") >>= A.toString

  pure { id, hash, filename }


toBackendAttachment :: BackendAttachment -> A.Json
toBackendAttachment y = A.fromObject $ StrMap.fromFoldable
  [ fAtt y (SProxy :: SProxy "id")       $ toNumber >>> A.fromNumber
  , fAtt y (SProxy :: SProxy "hash")     A.fromString
  , fAtt y (SProxy :: SProxy "filename") A.fromString
  ]

  where
    fAtt -- "Att" is for "Attachment"
      :: forall k a r'
       . RowCons k a r' BackendAttachmentFields
      => IsSymbol k
      => AttachmentKeyToBackendKey k
      => BackendAttachment
      -> SProxy k -> (a -> A.Json) -> Tuple String A.Json

    fAtt record key converter =
      Tuple (resourceAttachmentKeyToBackendKey key) $
        converter $ key `get` record
