module App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAction
     ( BackendAction
     , BackendActionFields
     , fromBackendAction
     , toBackendAction
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
import Data.Symbol (SProxy (SProxy), class IsSymbol)


type BackendActionFields = (label :: String, service :: String)
type BackendAction = Record BackendActionFields


class ActionKeyToBackendKey k where
  actionKeyToBackendKey
    :: forall a r'
     . RowCons k a r' BackendActionFields
    => IsSymbol k
    => SProxy k -> String

instance actionKeyToBackendKeyLabel :: ActionKeyToBackendKey "label" where
  actionKeyToBackendKey SProxy = "label"
instance actionKeyToBackendKeyService :: ActionKeyToBackendKey "service" where
  actionKeyToBackendKey SProxy = "svc"


backendActionValidKeys :: Set String
backendActionValidKeys = Set.fromFoldable
  [ k (SProxy :: SProxy "label")
  , k (SProxy :: SProxy "service")
  ]
  where
    k = actionKeyToBackendKey


fromBackendAction :: A.Json -> Maybe BackendAction
fromBackendAction json = do
  obj <- A.toObject json
  guard $ Set.fromFoldable (StrMap.keys obj) `Set.subset` backendActionValidKeys

  let l :: forall k a r'
         . RowCons k a r' BackendActionFields
        => IsSymbol k
        => ActionKeyToBackendKey k
        => SProxy k -> Maybe A.Json

      l key = actionKeyToBackendKey key `StrMap.lookup` obj

  label   <- l (SProxy :: SProxy "label")   >>= A.toString
  service <- l (SProxy :: SProxy "service") >>= A.toString

  pure { label, service }


toBackendAction :: BackendAction -> A.Json
toBackendAction x = A.fromObject $ StrMap.fromFoldable
  [ f x (SProxy :: SProxy "label")   A.fromString
  , f x (SProxy :: SProxy "service") A.fromString
  ]

  where
    f :: forall k a r'
       . RowCons k a r' BackendActionFields
      => IsSymbol k
      => ActionKeyToBackendKey k
      => BackendAction
      -> SProxy k
      -> (a -> A.Json)
      -> Tuple String A.Json

    f record key converter =
      Tuple (actionKeyToBackendKey key) $ converter $ key `get` record
