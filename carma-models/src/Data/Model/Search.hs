
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model.Search where

import           Control.Exception

import           Data.Either (partitionEithers)
import           Data.String (fromString)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM

import qualified Data.Map as Map

import           Data.Vector (Vector)
import qualified Data.Vector as V

import           Data.Time.Clock (UTCTime)

import           Data.Aeson as Aeson
import           Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.ToField (ToField)

import           Text.Printf
import           GHC.TypeLits

import           Data.Model as Model hiding (fieldName, modelName)
import           Data.Model.CoffeeType
import qualified Data.Model      as Model
import qualified Data.Model.View as View
import           Carma.Model.Types


data Predicate m
  = Predicate
    { tableName :: Text
    , modelName :: Text
    , fieldName :: Text
    , fieldDesc :: FieldDesc m
    , matchType :: MatchType
    , escapeVal
      :: PG.Connection -> PG.Query -> Aeson.Value
      -> IO (Either String Text)
    }

data MatchType
  = MatchExact | MatchFuzzy | MatchArray | MatchInterval
  deriving Show


-- FIXME: check if field type \in {Text, Int, ..}
one
  :: forall m t nm desc
  . (FromJSON t, ToField t, CoffeeType t
    ,SingI nm, SingI desc, Model m)
  => (m -> F t nm desc) -> [Predicate m]
one f = Predicate
  { tableName = Model.tableName (modelInfo :: ModelInfo m)
  , modelName = Model.modelName (modelInfo :: ModelInfo m)
  , fieldName = Model.fieldName f
  , fieldDesc = modelFieldsMap modelInfo HM.! Model.fieldName f
  , matchType = MatchExact
  , escapeVal = \conn qTpl val ->
      case fromJSON val :: Aeson.Result t of
        Error err -> return $ Left $ "Aeson error: " ++ err
        Success pgVal
          -> try (PG.formatQuery conn qTpl (Only pgVal))
          >>= return . \case
            Left e  -> Left $ show (e :: PG.FormatError)
            Right q -> Right $ T.decodeUtf8 q
  } : []

listOf
  :: forall m t nm desc
  . (FromJSON t, ToField t
    ,SingI nm, SingI desc, Model m)
  => (m -> F t nm desc) -> [Predicate m]
listOf _
  = map (\p -> p {matchType = MatchArray})
  $ one (undefined :: m -> F (Vector t) nm desc)


fuzzy :: [Predicate m] -> [Predicate m]
fuzzy = map (\p -> p {matchType = MatchFuzzy})


matchAny :: [[Predicate m]] -> [Predicate m]
matchAny = concat

interval
 :: forall m nm desc
 . (SingI nm, SingI desc, Model m)
 => (m -> F UTCTime nm desc) -> [Predicate m]
interval _
 = map (\p -> p {matchType = MatchInterval})
 $ one (undefined :: m -> F (Interval UTCTime) nm desc)

renderPredicate
  :: PG.Connection -> HashMap Text [Predicate m] -> Aeson.Object
  -> IO (Either String Text)
renderPredicate conn pMap vals = do
  let parenthize s = T.concat ["(", s, ")"]

  let renderDisjunct v (Predicate{..})
        = (\q -> escapeVal conn (fromString q) v)
        $ case matchType of
          MatchExact
            -> printf "%s.%s = ?"
              (T.unpack tableName) (T.unpack fieldName)
          MatchFuzzy
            -> printf "lower(%s.%s) ilike ('%%' || lower(?) || '%%')"
              (T.unpack tableName) (T.unpack fieldName)
          MatchArray
            -> printf "%s.%s = ANY(?)"
              (T.unpack tableName) (T.unpack fieldName)
          MatchInterval
            -> printf "%s.%s <@ ?"
               (T.unpack tableName) (T.unpack fieldName)

  let renderConjunct (key,val) = case HM.lookup key pMap of
        Nothing -> return $ Left
          $ "Invalid search param: " ++ show (key,val)
        Just ps -> do
          djs <- mapM (renderDisjunct val) ps
          return $ case partitionEithers djs of
            (errs@(_:_), _) -> Left $ show errs
            ([], res)       -> Right $ T.intercalate " OR " $ map parenthize res

  cjs <- mapM renderConjunct $ HM.toList vals
  return $ case partitionEithers cjs of
    (errs@(_:_), _) -> Left $ show errs
    ([], res)       -> Right $ T.intercalate " AND " $ map parenthize res


searchView :: [(Text, [Predicate m])] -> ModelView m
searchView flds = ModelView
  { View.modelName = "search"
  , View.title = "Поиск"
  , View.fields
    = [ v
        {View.name = nm
        ,View.meta = Map.insert
                "search"
                (buildSearchMeta ps)
                (View.meta v)
        }
      | (nm, ps@(p:_)) <- flds
      , let v = View.defaultFieldView $ fieldDesc p
      ]
  }
  where
    buildSearchMeta ps@(p:_) =
      Aeson.Object $ HM.fromList
        [ ( "matchType"
          , Aeson.String $ T.pack $ show $ matchType p
          )
          ,("original"
          , Aeson.Array $ V.fromList $ concatNames ps
          )
        ]
    concatNames []     = []
    concatNames (p:ps) = (buildOriginal p) : concatNames ps
    buildOriginal p = Aeson.Object $ HM.fromList
      [ ("name",  Aeson.String $ fieldName p)
      , ("model", Aeson.String $ modelName p)
      ]
