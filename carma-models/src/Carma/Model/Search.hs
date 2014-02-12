
{-# LANGUAGE ScopedTypeVariables #-}

module Carma.Model.Search where

import           Control.Exception

import           Data.Typeable
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

import           Data.Model hiding (fieldName, modelName, fieldDesc)
import qualified Data.Model as Model
import           Data.Model.Types hiding (modelName)
import           Carma.Model.Types
import           Carma.Model.LegacyTypes (LegacyDatetime, Reference)

data Predicate m
  = Predicate
    { tableName :: Text
    , modelName :: Text
    , fieldName :: Text
    , fieldDesc :: FieldDesc :@ m
    , matchType :: MatchType
    , escapeVal
      :: PG.Connection -> PG.Query -> Aeson.Value
      -> IO (Either String Text)
    }

data MatchType
  = MatchExact | MatchFuzzy | MatchArray | MatchInterval | MatchRefExist
  deriving Show


-- FIXME: check if field type \in {Text, Int, ..}
one
  :: forall m t nm desc
  . (FromJSON t, ToField t, DefaultFieldView t
    ,SingI nm, SingI desc, Model m)
  => (m -> F t nm desc) -> [Predicate m]
one f =
  let mi = modelInfo :: ModelInfo m
  in Predicate
    { tableName = Model.tableName mi
    , modelName = Model.modelName mi
    , fieldName = Model.fieldName f
    , fieldDesc = Wrap $ (modelFieldsMap mi HM.! Model.fieldName f)
        { fd_view = defaultFieldView (const Field :: m -> F t nm desc) }
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
  . (FromJSON t, ToField t, DefaultFieldView (Vector t)
    ,SingI nm, SingI desc, Model m)
  => (m -> F t nm desc) -> [Predicate m]
listOf _
  = map (\p -> p {matchType = MatchArray})
  $ one (undefined :: m -> F (Vector t) nm desc)


fuzzy :: [Predicate m] -> [Predicate m]
fuzzy = map (\p -> p {matchType = MatchFuzzy})


matchAny :: [[Predicate m]] -> [Predicate m]
matchAny = concat

class IntervalPred t m where
  interval :: t -> [Predicate m]

instance forall m nm desc . (SingI nm, SingI desc, Model m)
         => IntervalPred (m -> F LegacyDatetime nm desc) m  where
  interval _ = map (\p -> p {matchType = MatchInterval})
               $ one (undefined :: m -> F (Interval UTCTime) nm desc)

instance forall m nm desc . (SingI nm, SingI desc, Model m)
         => IntervalPred (m -> F (Maybe LegacyDatetime) nm desc) m where
  interval _ =
    interval (undefined :: (m -> F LegacyDatetime nm desc)) :: [Predicate m]

refExist :: forall m nm desc
 . (SingI nm, SingI desc, Model m)
 => (m -> F (Maybe Reference) nm desc) -> [Predicate m]
refExist v
  = map (\p -> p {matchType = MatchRefExist})
  $ one v


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
            -> printf "lower(%s.%s) like ('%%' || lower(?) || '%%')"
              (T.unpack tableName) (T.unpack fieldName)
          MatchArray
            -> printf "%s.%s = ANY(?)"
              (T.unpack tableName) (T.unpack fieldName)
          MatchInterval
            -> printf "%s.%s <@ ?::%s"
               (T.unpack tableName)
               (T.unpack fieldName)
               (T.unpack $ hs2pgtype $ fd_type $ unWrap fieldDesc)
          MatchRefExist
            -> let fname :: String
                     = printf "%s.%s" (T.unpack tableName) (T.unpack fieldName)
               in printf ("case ? " ++
                          "when 'yes'    then coalesce(%s, '') != '' " ++
                          "when 'no'     then coalesce(%s, '') =  '' " ++
                          "when 'unspec' then true "                   ++
                          "end "
                         )
                  fname fname


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
    ([], [])        -> Right "true"
    ([], res)       -> Right $ T.intercalate " AND " $ map parenthize res

hs2pgtype :: TypeRep -> Text
hs2pgtype t
  | t == typeOf (undefined :: Interval UTCTime) = "tstzrange"
  | otherwise  = error "Unknown type in hs2pgtype"

predicatesFromParams
  ::PG.Connection -> Aeson.Object -> [(Text, [Predicate m])]
    -> IO (Either String Text)
predicatesFromParams c o modelParams =
  renderPredicate c params $ filterParams o
  where
    params = HM.fromList modelParams
    filterParams h = HM.intersection h params

concatPredStrings :: [Text] -> Text
concatPredStrings [] = "true"
concatPredStrings ps =
  case T.intercalate " AND " $ filter (/= "") ps of
    "" -> "true"
    v  -> v

searchView :: Model m => [(Text, [Predicate m])] -> ModelView m
searchView flds = ModelView
  { mv_modelName = "search"
  , mv_title = "Поиск"
  , mv_fields
    = [ v
        {fv_name = nm
        ,fv_meta = Map.insert
                "search"
                (buildSearchMeta ps)
                (fv_meta v)
        }
      | (nm, ps@(p:_)) <- flds
      , let v = fd_view $ unWrap $ fieldDesc p
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
