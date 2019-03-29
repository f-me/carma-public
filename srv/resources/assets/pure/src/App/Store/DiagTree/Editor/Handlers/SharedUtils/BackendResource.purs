module App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendResource
     ( BackendResource
     , BackendResourceFields
     , fromBackendResource
     , toBackendResource
     ) where

import Prelude
import Prim.Row (class Cons)

import Data.Tuple (Tuple (Tuple))
import Data.Maybe (Maybe (..))
import Data.Set (Set)
import Data.Set as Set
import Data.Argonaut.Core as A
import Data.Symbol (SProxy (SProxy), class IsSymbol, reflectSymbol)
import Data.Array (cons)
import Foreign.Object as FObj
import Record (get)

import Control.MonadZero (guard)

import App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAttachment
     ( BackendAttachment
     , fromBackendAttachment
     , toBackendAttachment
     )


type BackendResource = Record BackendResourceFields

type BackendResourceFields =
   ( text       :: String
   , file       :: Maybe String
   , attachment :: Maybe BackendAttachment
   )


class ResourceKeyToBackendKey k where
  resourceKeyToBackendKey
    :: forall a r'
     . Cons k a r' BackendResourceFields
    => IsSymbol k
    => SProxy k -> String

instance resourceKeyToBackendKeyGeneric
  :: IsSymbol k => ResourceKeyToBackendKey k
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


fromBackendResource :: A.Json -> Maybe BackendResource
fromBackendResource json = do
  obj <- A.toObject json
  guard $ Set.fromFoldable (FObj.keys obj) `Set.subset` backendResourceValidKeys

  let l :: forall k a r'
         . Cons k a r' BackendResourceFields
        => IsSymbol k
        => ResourceKeyToBackendKey k
        => SProxy k -> Maybe A.Json

      l key = resourceKeyToBackendKey key `FObj.lookup` obj

  text <- l (SProxy :: SProxy "text") >>= A.toString

  -- TODO See comment for `DiagTreeSlideAttachment`,
  --      remove legacy `file` field and make `attachment` field be required.
  attachment <-
    case l (SProxy :: SProxy "attachment") of
         Nothing -> pure Nothing -- TODO Temporarly optional
         Just jsonAttachment -> fromBackendAttachment jsonAttachment <#> Just

  -- TODO Get rid of this legacy deprecated field completely.
  file <-
    case l (SProxy :: SProxy "file") of
         Nothing -> pure Nothing -- Field is not set
         Just x  -> A.toString x <#> Just

  pure { text, attachment, file }


toBackendResource :: BackendResource -> A.Json
toBackendResource x = A.fromObject $ FObj.fromFoldable $
  [ f    x (SProxy :: SProxy "text")       A.fromString ]
  # fOpt x (SProxy :: SProxy "attachment") toBackendAttachment
  # fOpt x (SProxy :: SProxy "file")       A.fromString

  where
    f :: forall k a r'
       . Cons k a r' BackendResourceFields
      => IsSymbol k
      => ResourceKeyToBackendKey k
      => BackendResource -> SProxy k -> (a -> A.Json) -> Tuple String A.Json

    f record key converter =
      Tuple (resourceKeyToBackendKey key) $ converter $ key `get` record

    fOpt -- For fields that could be not set
      :: forall k a r'
       . Cons k (Maybe a) r' BackendResourceFields
      => IsSymbol k
      => ResourceKeyToBackendKey k
      => BackendResource
      -> SProxy k
      -> (a -> A.Json)
      -> (Array (Tuple String A.Json) -> Array (Tuple String A.Json))

    fOpt record key converter =
      case key `get` record of
           Nothing -> identity
           Just y  -> cons $ f x key $ const $ converter y
