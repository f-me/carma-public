module App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAnswer
     ( BackendAnswer
     , BackendAnswerFields
     , fromBackendAnswer
     , toBackendAnswer
     ) where

import Prelude
import Prim.Row (class Cons)

import Data.Int (fromNumber, toNumber)
import Data.String (null)
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

import App.Store.DiagTree.Editor.Types (DiagTreeSlideId)

import App.Store.DiagTree.Editor.Handlers.SharedUtils.BackendAttachment
     ( BackendAttachment
     , fromBackendAttachment
     , toBackendAttachment
     )


type BackendAnswer = Record BackendAnswerFields

type BackendAnswerFields =
   ( nextSlide  :: DiagTreeSlideId
   , header     :: String
   , text       :: String
   , attachment :: Maybe BackendAttachment
   , file       :: Maybe String -- could be not set or `null`
   )


class AnswerKeyToBackendKey k where
  answerKeyToBackendKey
    :: forall a r'
     . Cons k a r' BackendAnswerFields
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
  , k (SProxy :: SProxy "attachment")
  , k (SProxy :: SProxy "file")

  , answerActionFieldName -- TODO FIXME Remove after cleanup migration.
                          --            Deprecated field that must be `{}`
                          --            or just not set.
  ]
  where
    k = answerKeyToBackendKey


toBackendAnswer :: BackendAnswer -> A.Json
toBackendAnswer x = A.fromObject $ FObj.fromFoldable $
  [ f    x (SProxy :: SProxy "nextSlide")  $ toNumber >>> A.fromNumber
  , f    x (SProxy :: SProxy "header")     A.fromString
  , f    x (SProxy :: SProxy "text")       A.fromString
  ]
  # fOpt x (SProxy :: SProxy "attachment") toBackendAttachment
  # fOpt x (SProxy :: SProxy "file")       A.fromString

  where
    f :: forall k a r'
       . Cons k a r' BackendAnswerFields
      => IsSymbol k
      => AnswerKeyToBackendKey k
      => BackendAnswer -> SProxy k -> (a -> A.Json) -> Tuple String A.Json

    f record key converter =
      Tuple (answerKeyToBackendKey key) $ converter $ key `get` record

    fOpt -- For fields that could be not set
      :: forall k a r'
       . Cons k (Maybe a) r' BackendAnswerFields
      => IsSymbol k
      => AnswerKeyToBackendKey k
      => BackendAnswer
      -> SProxy k
      -> (a -> A.Json)
      -> (Array (Tuple String A.Json) -> Array (Tuple String A.Json))

    fOpt record key converter =
      case key `get` record of
           Nothing -> identity
           Just y  -> cons $ f x key $ const $ converter y


fromBackendAnswer :: A.Json -> Maybe BackendAnswer
fromBackendAnswer json = do
  obj <- A.toObject json
  guard $ Set.fromFoldable (FObj.keys obj) `Set.subset` backendAnswerValidKeys

  let l :: forall k a r'
         . Cons k a r' BackendAnswerFields
        => IsSymbol k
        => AnswerKeyToBackendKey k
        => SProxy k -> Maybe A.Json

      l key = answerKeyToBackendKey key `FObj.lookup` obj

  nextSlide <- l (SProxy :: SProxy "nextSlide") >>= A.toNumber >>= fromNumber
  header    <- l (SProxy :: SProxy "header")    >>= A.toString
  text      <- l (SProxy :: SProxy "text")      >>= A.toString

  -- TODO See comment for `DiagTreeSlideAttachment`, remove legacy `file` field.
  attachment <-
    case l (SProxy :: SProxy "attachment") of
         Nothing -> pure Nothing -- Field is not set (that's okay)
         Just jsonAttachment ->
           if A.isNull jsonAttachment -- Field is set to `null`
              then pure Nothing -- `null` also means field is not set
              else fromBackendAttachment jsonAttachment <#> Just

  -- TODO Get rid of this legacy deprecated field completely.
  file <-
    case l (SProxy :: SProxy "file") of
         Nothing  -> pure Nothing -- Field is not set (that's okay)
         Just raw -> if A.isNull raw -- Field is set to `null`
                        then pure Nothing -- `null` also means field is not set
                        else A.toString raw <#>
                               \x -> if null x then Nothing else Just x

  -- TODO FIXME Remove after cleanup migration.
  --            Deprecated field that must be `{}` or just not set.
  case FObj.lookup answerActionFieldName obj of
       Nothing -> pure unit -- Not set, the best way
       Just x  -> A.toObject x >>= \y -> unit <$ guard (FObj.isEmpty y)

  pure { nextSlide, header, text, attachment, file }
