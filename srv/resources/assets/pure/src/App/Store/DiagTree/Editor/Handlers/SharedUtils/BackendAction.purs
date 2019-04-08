module App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAction
     ( BackendAction
     , BackendActionFields
     , fromBackendAction
     , toBackendAction
     ) where

import Prelude
import Prim.Row (class Cons)

import Data.Tuple (Tuple (Tuple))
import Data.Maybe (Maybe)
import Data.Set (Set)
import Data.Set as Set
import Data.Argonaut.Core as A
import Data.Symbol (SProxy (SProxy), class IsSymbol)
import Foreign.Object as FObj
import Record (get)

import Control.MonadZero (guard)


type BackendActionFields = (label :: String, service :: String)
type BackendAction = Record BackendActionFields


class ActionKeyToBackendKey k where
  actionKeyToBackendKey
    :: forall a r'
     . Cons k a r' BackendActionFields
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
  guard $ Set.fromFoldable (FObj.keys obj) `Set.subset` backendActionValidKeys

  let l :: forall k a r'
         . Cons k a r' BackendActionFields
        => IsSymbol k
        => ActionKeyToBackendKey k
        => SProxy k -> Maybe A.Json

      l key = actionKeyToBackendKey key `FObj.lookup` obj

  label   <- l (SProxy :: SProxy "label")   >>= A.toString
  service <- l (SProxy :: SProxy "service") >>= A.toString

  pure { label, service }


toBackendAction :: BackendAction -> A.Json
toBackendAction x = A.fromObject $ FObj.fromFoldable
  [ f x (SProxy :: SProxy "label")   A.fromString
  , f x (SProxy :: SProxy "service") A.fromString
  ]

  where
    f :: forall k a r'
       . Cons k a r' BackendActionFields
      => IsSymbol k
      => ActionKeyToBackendKey k
      => BackendAction -> SProxy k -> (a -> A.Json) -> Tuple String A.Json

    f record key converter =
      Tuple (actionKeyToBackendKey key) $ converter $ key `get` record
