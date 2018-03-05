module App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAnswer
     ( BackendAnswer
     , BackendAnswerFields
     , fromBackendAnswer
     , toBackendAnswer
     ) where

import Prelude

import Control.MonadZero (guard)

import Data.Int (fromNumber, toNumber)
import Data.String (null)
import Data.Tuple (Tuple (Tuple))
import Data.Maybe (Maybe (..), maybe)
import Data.StrMap as StrMap
import Data.Set (Set)
import Data.Set as Set
import Data.Argonaut.Core as A
import Data.Record (get)
import Data.Symbol (SProxy (SProxy), class IsSymbol, reflectSymbol)

import App.Store.DiagTree.Editor.Types (DiagTreeSlideId)


type BackendAnswerFields =
  ( nextSlide :: DiagTreeSlideId
  , header    :: String
  , text      :: String
  , file      :: Maybe String -- could be not set or `null`
  )

type BackendAnswer = Record BackendAnswerFields

class AnswerKeyToBackendKey k where
  answerKeyToBackendKey
    :: forall a r'
     . RowCons k a r' BackendAnswerFields
    => IsSymbol k
    => SProxy k -> String

instance answerKeyToBackendKeyGeneric
  :: (IsSymbol k) => AnswerKeyToBackendKey k
  where
  answerKeyToBackendKey = reflectSymbol


-- TODO FIXME Remove after cleanup migration.
--            Deprecated field that must be `{}` or just not set.
answerActionFieldName :: String
answerActionFieldName = "action"

backendAnswerValidKeys :: Set String
backendAnswerValidKeys = Set.fromFoldable
  [ k (SProxy :: SProxy "nextSlide")
  , k (SProxy :: SProxy "header")
  , k (SProxy :: SProxy "text")
  , k (SProxy :: SProxy "file")

  , answerActionFieldName -- TODO FIXME Remove after cleanup migration.
                          --            Deprecated field that must be `{}`
                          --            or just not set.
  ]
  where
    k = answerKeyToBackendKey

toBackendAnswer :: BackendAnswer -> A.Json
toBackendAnswer x = A.fromObject $ StrMap.fromFoldable
  [ f x (SProxy :: SProxy "nextSlide") $ toNumber >>> A.fromNumber
  , f x (SProxy :: SProxy "header")    A.fromString
  , f x (SProxy :: SProxy "text")      A.fromString
  , f x (SProxy :: SProxy "file")      $ maybe A.jsonNull A.fromString
  ]

  where
    f :: forall k a r'
       . RowCons k a r' BackendAnswerFields
      => IsSymbol k
      => AnswerKeyToBackendKey k
      => BackendAnswer
      -> SProxy k
      -> (a -> A.Json)
      -> Tuple String A.Json

    f record key converter =
      Tuple (answerKeyToBackendKey key) $ converter $ key `get` record

fromBackendAnswer :: A.Json -> Maybe BackendAnswer
fromBackendAnswer json = do
  obj <- A.toObject json
  guard $ Set.fromFoldable (StrMap.keys obj) `Set.subset` backendAnswerValidKeys

  let l :: forall k a r'
         . RowCons k a r' BackendAnswerFields
        => IsSymbol k
        => AnswerKeyToBackendKey k
        => SProxy k -> Maybe A.Json

      l key = answerKeyToBackendKey key `StrMap.lookup` obj

  nextSlide <- l (SProxy :: SProxy "nextSlide") >>= A.toNumber >>= fromNumber
  header    <- l (SProxy :: SProxy "header")    >>= A.toString
  text      <- l (SProxy :: SProxy "text")      >>= A.toString

  file <-
    case l (SProxy :: SProxy "file") of
         Nothing  -> pure Nothing  -- Field is not set (that's okay)
         Just raw -> if A.isNull raw -- Field is set to `null`
                        then pure Nothing
                        else A.toString raw <#>
                               \x -> if null x then Nothing else Just x

  -- TODO FIXME Remove after cleanup migration.
  --            Deprecated field that must be `{}` or just not set.
  case StrMap.lookup answerActionFieldName obj of
       Nothing -> pure unit -- Not set, the best way
       Just x  -> A.toObject x >>= (\y -> unit <$ guard (StrMap.isEmpty y))

  pure { nextSlide, header, text, file }
