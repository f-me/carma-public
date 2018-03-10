module App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendResource
     ( BackendResource
     , BackendResourceFields
     , BackendResourceAttachment
     , BackendResourceAttachmentFields
     , fromBackendResource
     , fromAttachment
     , toBackendResource
     , toAttachment
     ) where

import Prelude hiding (id)
import Prelude as Prelude

import Control.MonadZero (guard)

import Data.Int (fromNumber, toNumber)
import Data.Tuple (Tuple (Tuple))
import Data.Maybe (Maybe (..))
import Data.StrMap as StrMap
import Data.Set (Set)
import Data.Set as Set
import Data.Argonaut.Core as A
import Data.Record (get)
import Data.Symbol (SProxy (SProxy), class IsSymbol, reflectSymbol)
import Data.Array (cons)


type BackendResource           = Record BackendResourceFields
type BackendResourceAttachment = Record BackendResourceAttachmentFields

type BackendResourceFields =
  ( text       :: String
  , file       :: Maybe String
  , attachment :: Maybe BackendResourceAttachment
  )

type BackendResourceAttachmentFields =
  ( id       :: Int
  , hash     :: String
  , filename :: String
  )


class ResourceKeyToBackendKey k where
  resourceKeyToBackendKey
    :: forall a r'
     . RowCons k a r' BackendResourceFields
    => IsSymbol k
    => SProxy k -> String

instance resourceKeyToBackendKeyGeneric
  :: (IsSymbol k) => ResourceKeyToBackendKey k
  where
  resourceKeyToBackendKey = reflectSymbol

backendResourceValidKeys :: Set String
backendResourceValidKeys = Set.fromFoldable
  [ k (SProxy :: SProxy "text")
  , k (SProxy :: SProxy "file")
  , k (SProxy :: SProxy "attachment")
  ]
  where
    k = resourceKeyToBackendKey


class ResourceAttachmentKeyToBackendKey k where
  resourceAttachmentKeyToBackendKey
    :: forall a r'
     . RowCons k a r' BackendResourceAttachmentFields
    => IsSymbol k
    => SProxy k -> String

instance resourceAttachmentKeyToBackendKeyGeneric
  :: (IsSymbol k) => ResourceAttachmentKeyToBackendKey k
  where
  resourceAttachmentKeyToBackendKey = reflectSymbol

backendResourceAttachmentValidKeys :: Set String
backendResourceAttachmentValidKeys = Set.fromFoldable
  [ k (SProxy :: SProxy "id")
  , k (SProxy :: SProxy "hash")
  , k (SProxy :: SProxy "filename")
  , "ctime"
  ]
  where
    k = resourceAttachmentKeyToBackendKey


fromBackendResource :: A.Json -> Maybe BackendResource
fromBackendResource json = do
  obj <- A.toObject json

  guard $
    Set.fromFoldable (StrMap.keys obj) `Set.subset` backendResourceValidKeys

  let l :: forall k a r'
         . RowCons k a r' BackendResourceFields
        => IsSymbol k
        => ResourceKeyToBackendKey k
        => SProxy k -> Maybe A.Json

      l key = resourceKeyToBackendKey key `StrMap.lookup` obj

  text <- l (SProxy :: SProxy "text") >>= A.toString

  -- TODO See comment for `DiagTreeSlideResourceAttachment`,
  --      remove legacy `file` field and make `attachment` field be required.
  attachment <-
    case l (SProxy :: SProxy "attachment") of
         Nothing -> pure Nothing -- TODO Temporarly optional
         Just jsonAttachment -> fromAttachment jsonAttachment <#> Just

  -- TODO Get rid of this legacy deprecated field completely.
  file <-
    case l (SProxy :: SProxy "file") of
         Nothing -> pure Nothing -- Field is not set
         Just x  -> A.toString x <#> Just

  pure { text, attachment, file }


fromAttachment :: A.Json -> Maybe BackendResourceAttachment
fromAttachment json = do
  obj <- A.toObject json

  guard $
    Set.fromFoldable (StrMap.keys obj)
      `Set.subset` backendResourceAttachmentValidKeys

  let l :: forall k a r'
         . RowCons k a r' BackendResourceAttachmentFields
        => IsSymbol k
        => ResourceAttachmentKeyToBackendKey k
        => SProxy k -> Maybe A.Json

      l key = resourceAttachmentKeyToBackendKey key `StrMap.lookup` obj

  id       <- l (SProxy :: SProxy "id")       >>= A.toNumber >>= fromNumber
  hash     <- l (SProxy :: SProxy "hash")     >>= A.toString
  filename <- l (SProxy :: SProxy "filename") >>= A.toString

  pure { id, hash, filename }


toBackendResource :: BackendResource -> A.Json
toBackendResource x = A.fromObject $ StrMap.fromFoldable $
  [ f    x (SProxy :: SProxy "text")       A.fromString ]
  # fOpt x (SProxy :: SProxy "attachment") toAttachment
  # fOpt x (SProxy :: SProxy "file")       A.fromString

  where
    f :: forall k a r'
       . RowCons k a r' BackendResourceFields
      => IsSymbol k
      => ResourceKeyToBackendKey k
      => BackendResource -> SProxy k -> (a -> A.Json) -> Tuple String A.Json

    f record key converter =
      Tuple (resourceKeyToBackendKey key) $ converter $ key `get` record

    fOpt -- For fields that could be not set
      :: forall k a r'
       . RowCons k (Maybe a) r' BackendResourceFields
      => IsSymbol k
      => ResourceKeyToBackendKey k
      => BackendResource
      -> SProxy k
      -> (a -> A.Json)
      -> (Array (Tuple String A.Json) -> Array (Tuple String A.Json))

    fOpt record key converter =
      case key `get` record of
           Nothing -> Prelude.id
           Just y  -> cons $
             f x (SProxy :: SProxy "attachment") $ const $ converter y


toAttachment :: BackendResourceAttachment -> A.Json
toAttachment y = A.fromObject $ StrMap.fromFoldable
  [ fAtt y (SProxy :: SProxy "id")       $ toNumber >>> A.fromNumber
  , fAtt y (SProxy :: SProxy "hash")     A.fromString
  , fAtt y (SProxy :: SProxy "filename") A.fromString
  ]

  where
    fAtt -- "Att" is for "Attachment"
      :: forall k a r'
       . RowCons k a r' BackendResourceAttachmentFields
      => IsSymbol k
      => ResourceAttachmentKeyToBackendKey k
      => BackendResourceAttachment
      -> SProxy k -> (a -> A.Json) -> Tuple String A.Json

    fAtt record key converter =
      Tuple (resourceAttachmentKeyToBackendKey key) $
        converter $ key `get` record
