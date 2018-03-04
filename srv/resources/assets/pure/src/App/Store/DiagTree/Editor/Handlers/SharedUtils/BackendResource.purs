module App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendResource
     ( BackendResource
     , BackendResourceFields
     , fromBackendResource
     , toBackendResource
     ) where

import Prelude

import Control.MonadZero (guard)

import Data.Tuple (Tuple (Tuple))
import Data.Maybe (Maybe)
import Data.StrMap as StrMap
import Data.Set (Set)
import Data.Set as Set
import Data.Argonaut.Core as A
import Data.Record (get)
import Data.Symbol (SProxy (SProxy), class IsSymbol, reflectSymbol)


type BackendResourceFields = (text :: String, file :: String)
type BackendResource = Record BackendResourceFields

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
  ]
  where
    k = resourceKeyToBackendKey

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
  file <- l (SProxy :: SProxy "file") >>= A.toString
  pure { text, file }

toBackendResource :: BackendResource -> A.Json
toBackendResource x = A.fromObject $ StrMap.fromFoldable
  [ f x (SProxy :: SProxy "text") A.fromString
  , f x (SProxy :: SProxy "file") A.fromString
  ]

  where
    f :: forall k a r'
       . RowCons k a r' BackendResourceFields
      => IsSymbol k
      => ResourceKeyToBackendKey k
      => BackendResource
      -> SProxy k
      -> (a -> A.Json)
      -> Tuple String A.Json

    f record key converter =
      Tuple (resourceKeyToBackendKey key) $ converter $ key `get` record
