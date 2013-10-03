
{-# LANGUAGE ScopedTypeVariables #-}

module Data.Model.Search where

import Control.Applicative
import Control.Exception

import Data.Either (partitionEithers)
import Data.String (fromString)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HM
import Data.Vector (Vector)

import Data.Aeson as Aeson
import Database.PostgreSQL.Simple as PG
import Database.PostgreSQL.Simple.ToField (ToField)

import Text.Printf
import GHC.TypeLits

import Data.Model as Model

{-
test = HM.fromList
  [("id",    one ident)
  ,("phone", fuzzy $ matchAny [one Case.phone1, one Case.phone2])
  ,("city",  listOf Case.city)
  ]
-}


data Predicate
  = Predicate
    { tableName :: Text
    , fieldName :: Text
    , matchType :: MatchType
    , escapeVal
      :: PG.Connection -> PG.Query -> Aeson.Value
      -> IO (Either String Text)
    }

data MatchType = MatchExact | MatchFuzzy | MatchArray


-- FIXME: check if field type \in {Text, Int, ..}
one
  :: forall m t nm desc
  . (FromJSON t, ToField t
    ,SingI nm, SingI desc, Model m)
  => (m -> F t nm desc) -> [Predicate]
one f = Predicate
  { tableName = Model.tableName (modelInfo :: ModelInfo m)
  , fieldName = Model.fieldName f
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
  => (m -> F t nm desc) -> [Predicate]
listOf _
  = map (\p -> p {matchType = MatchArray})
  $ one (undefined :: m -> F (Vector t) nm desc)


fuzzy :: [Predicate] -> [Predicate]
fuzzy = map (\p -> p {matchType = MatchFuzzy})


matchAny :: [[Predicate]] -> [Predicate]
matchAny = concat


renderPredicate
  :: PG.Connection -> HashMap Text [Predicate] -> Aeson.Object
  -> IO (Either String Text)
renderPredicate conn pMap vals = do
  let parenthize s = T.concat ["(", s, ")"]

  let renderDisjunct v (Predicate{..})
        = (\q -> escapeVal conn (fromString q) v)
        $ case matchType of
          MatchExact
            -> printf "%s.%s = ?"
              (show tableName) (T.unpack fieldName)
          MatchFuzzy
            -> printf "%s.%s ilike ('%' || ? || '%')"
              (show tableName) (T.unpack fieldName)
          MatchArray
            -> printf "%s.%s = ANY(?)"
              (show tableName) (T.unpack fieldName)

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
