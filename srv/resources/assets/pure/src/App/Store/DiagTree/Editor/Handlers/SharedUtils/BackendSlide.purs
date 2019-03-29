module App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendSlide
     ( BackendSlide
     , fromBackendSlide
     , fromBackendSlideObj
     , PartialBackendSlide
     , PartialBackendSlideFields
     , defaultPartialBackendSlide
     , toBackendSlideFromPartial
     , getBackendSlideId
     ) where

import Prelude
import Prim.Row (class Cons)

import Data.Int (fromNumber, toNumber)
import Data.Maybe (Maybe (..), maybe)
import Data.Foldable (foldM)
import Data.Array (snoc)
import Data.Set (Set)
import Data.Set as Set
import Data.Argonaut.Core as A
import Data.Symbol (SProxy (SProxy), class IsSymbol, reflectSymbol)
import Foreign.Object as FObj
import Record (get)

import Control.MonadZero (guard)

import App.Store.DiagTree.Editor.Types (DiagTreeSlideId)

import App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendResource
     ( BackendResource
     , fromBackendResource
     , toBackendResource
     )

import App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAction
     ( BackendAction
     , fromBackendAction
     , toBackendAction
     )

import App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAnswer
     ( BackendAnswer
     , fromBackendAnswer
     , toBackendAnswer
     )


type BackendSlide =
   { id        :: DiagTreeSlideId
   , isRoot    :: Boolean
   , ctime     :: String
   , header    :: String
   , body      :: String
   , resources :: Array BackendResource
   , actions   :: Array BackendAction
   , answers   :: Array BackendAnswer
   }

backendSlideValidKeys :: Set String
backendSlideValidKeys = Set.fromFoldable
  [ k (SProxy :: SProxy "id")
  , k (SProxy :: SProxy "isActive") -- Only active must be passed when loading
  , k (SProxy :: SProxy "isRoot")
  , k (SProxy :: SProxy "ctime")
  , k (SProxy :: SProxy "header")
  , k (SProxy :: SProxy "body")
  , k (SProxy :: SProxy "resources")
  , k (SProxy :: SProxy "actions")
  , k (SProxy :: SProxy "answers")
  , "files"
  ]
  where
    k = slideKeyToBackendKey

type PartialBackendSlideFields =
  ( id        :: Maybe DiagTreeSlideId
  , isActive  :: Maybe Boolean
  , isRoot    :: Maybe Boolean
  , ctime     :: Maybe String
  , header    :: Maybe String
  , body      :: Maybe String
  , resources :: Maybe (Array BackendResource)
  , actions   :: Maybe (Array BackendAction)
  , answers   :: Maybe (Array BackendAnswer)
  )

type PartialBackendSlide = Record PartialBackendSlideFields

class SlideKeyToBackendKey k where
  slideKeyToBackendKey
    :: forall a r'
     . Cons k a r' PartialBackendSlideFields
    => IsSymbol k
    => SProxy k -> String

instance slideKeyToBackendKeyGeneric
  :: (IsSymbol k) => SlideKeyToBackendKey k
  where
  slideKeyToBackendKey = reflectSymbol

defaultPartialBackendSlide :: PartialBackendSlide
defaultPartialBackendSlide =
  { id        : Nothing
  , isActive  : Nothing
  , isRoot    : Nothing
  , ctime     : Nothing
  , header    : Nothing
  , body      : Nothing
  , resources : Nothing
  , actions   : Nothing
  , answers   : Nothing
  }

fromBackendSlide :: A.Json -> Maybe BackendSlide
fromBackendSlide = A.toObject >=> fromBackendSlideObj

fromBackendSlideObj :: FObj.Object A.Json -> Maybe BackendSlide
fromBackendSlideObj obj = do
  guard $ Set.fromFoldable (FObj.keys obj) `Set.subset` backendSlideValidKeys

  let l :: forall k a r'
         . Cons k a r' PartialBackendSlideFields
        => IsSymbol k
        => SlideKeyToBackendKey k
        => SProxy k -> Maybe A.Json

      l key = slideKeyToBackendKey key `FObj.lookup` obj

  id        <- l (SProxy :: SProxy "id")        >>= A.toNumber >>= fromNumber
  isActive  <- l (SProxy :: SProxy "isActive")  >>= A.toBoolean
  guard     $  isActive -- Must be filtered behind or it will fail to parse
  isRoot    <- l (SProxy :: SProxy "isRoot")    >>= A.toBoolean
  ctime     <- l (SProxy :: SProxy "ctime")     >>= A.toString
  header    <- l (SProxy :: SProxy "header")    >>= A.toString
  body      <- l (SProxy :: SProxy "body")      >>= A.toString

  resources <- l (SProxy :: SProxy "resources") >>= A.toArray
                                                >>= foldM reduceResource []

  actions   <- l (SProxy :: SProxy "actions")   >>= A.toArray
                                                >>= foldM reduceAction []

  answers   <- l (SProxy :: SProxy "answers")   >>= A.toArray
                                                >>= foldM reduceAnswer []

  pure { id, isRoot, ctime, header, body, resources, actions, answers }

  where
    reduceResource acc jsonItem = snoc acc <$> fromBackendResource jsonItem
    reduceAction   acc jsonItem = snoc acc <$> fromBackendAction   jsonItem
    reduceAnswer   acc jsonItem = snoc acc <$> fromBackendAnswer   jsonItem

toBackendSlideFromPartial :: PartialBackendSlide -> A.Json
toBackendSlideFromPartial p
  = A.fromObject
  $ FObj.empty
  # f p (SProxy :: SProxy "id")        (toNumber >>> A.fromNumber)
  # f p (SProxy :: SProxy "isActive")  A.fromBoolean
  # f p (SProxy :: SProxy "isRoot")    A.fromBoolean
  # f p (SProxy :: SProxy "ctime")     A.fromString
  # f p (SProxy :: SProxy "header")    A.fromString
  # f p (SProxy :: SProxy "body")      A.fromString
  # f p (SProxy :: SProxy "resources") (map toBackendResource >>> A.fromArray)
  # f p (SProxy :: SProxy "actions")   (map toBackendAction   >>> A.fromArray)
  # f p (SProxy :: SProxy "answers")   (map toBackendAnswer   >>> A.fromArray)

  where
    f :: forall k a r'
       . IsSymbol k
      => Cons k (Maybe a) r' PartialBackendSlideFields
      => PartialBackendSlide
      -> SProxy k
      -> (a -> A.Json)
      -> (FObj.Object A.Json -> FObj.Object A.Json)

    f partial key converter =
      maybe identity (converter >>> inserter) $ key `get` partial
      where inserter = FObj.insert $ reflectSymbol key


getBackendSlideId :: A.Json -> Maybe DiagTreeSlideId
getBackendSlideId = A.toObject >=> \obj ->
  lookup (SProxy :: SProxy "id") obj >>= A.toNumber >>= fromNumber

  where
    lookup
      :: forall k a r'
       . Cons k a r' PartialBackendSlideFields
      => IsSymbol k
      => SlideKeyToBackendKey k
      => SProxy k -> FObj.Object A.Json -> Maybe A.Json

    lookup key = FObj.lookup $ slideKeyToBackendKey key
