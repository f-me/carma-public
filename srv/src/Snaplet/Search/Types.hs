{-# LANGUAGE TemplateHaskell,
             ScopedTypeVariables,
             DeriveGeneric,
             TypeOperators,
             FlexibleInstances,
             FlexibleContexts,
             OverlappingInstances,
             UndecidableInstances
 #-}


module Snaplet.Search.Types where

import           Control.Applicative ((<$>), (<*>), (<|>), (*>),  pure)
import           Control.Lens (makeLenses)
import           Control.Monad.State

import           Prelude hiding (null)
import           Data.Maybe
import           Data.Text as T hiding (map, null, length)
import           Data.Pool
import           Data.Aeson

import           GHC.Generics

import           Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.Types
import           Database.PostgreSQL.Simple.FromRow

import           Snap.Snaplet
import           Snap.Snaplet.Auth hiding (Role)
import           Snap.Snaplet.PostgresqlSimple (Postgres(..), HasPostgres(..))

import qualified Data.Vector           as V
import qualified Data.HashMap.Strict   as HM

import           Data.Model       as M
import           Data.Model.Patch (Patch)

import           Carma.Model
import           Carma.Model.Role
import           Carma.Model.FieldPermission hiding (field)


data Search b = Search
  {pg      :: Pool Connection
  ,_postgres :: Snaplet Postgres
  ,_auth     :: Snaplet (AuthManager b)
  }

makeLenses ''Search

instance HasPostgres (Handler b (Search b)) where
    getPostgresState = with postgres get

type SearchHandler b t = Handler b (Search b) t



data SearchReq = SearchReq { predicates :: Object
                           , sorts      :: Order
                           } deriving (Show, Generic)
instance FromJSON SearchReq

data OrderType = Asc | Desc deriving Show
instance FromJSON OrderType where
  parseJSON (String "asc")  = return Asc
  parseJSON (String "desc") = return Desc
  parseJSON v = fail $ "Unknown OrderType, waiting 'asc'|'desc', but got: " ++
                show v

data Order = Order { fields :: [FieldIdent]
                   , order  :: OrderType
                   } deriving Show

instance FromJSON Order where
  parseJSON (Object o) = Order <$> o .: "fields" <*> o .: "order"

data FieldIdent = FieldIdent { tablename :: Text
                             , fname     :: Text
                             } deriving Show
instance FromJSON FieldIdent where
  parseJSON (Object o) = do
    model <- o .: "model"
    name  <- o .: "name"
    case FieldIdent <$> gettn model <*> getfn model name of
      Nothing ->
        fail $ "Can't parse FieldDesc, can't find field with model: "
        ++ show model ++ " and name: " ++ show name
      Just d  -> return $ d
    where
      getTable :: forall m.Model m => m -> Text
      getTable _ = tableName (modelInfo :: ModelInfo m)
      gettn model = dispatch model getTable

      getFMap :: forall m.Model m => m -> HM.HashMap Text FieldDesc
      getFMap _ = modelFieldsMap (modelInfo :: ModelInfo m)
      getfn model name =
        dispatch model (getFMap) >>= HM.lookup name >>= return . fd_name

  parseJSON v = fail $ "Can't parse FieldDesc, expecting object, but got: " ++
                show v

instance Show FieldDesc where
  show fd = "FieldDesc: " ++ (show $ fd_name fd)

data SearchResult t = SearchResult
  { values :: [t]
  , next :: Maybe Int
  , prev :: Maybe Int
  } deriving (Generic)
instance ToJSON t => ToJSON (SearchResult t)


instance forall m.(Model m) => ToJSON (Patch m :. ()) where
  toJSON (p :. ()) =
    object [(M.modelName (M.modelInfo :: M.ModelInfo m)) .= toJSON p]

instance forall m.(Model m) => ToJSON (Maybe (Patch m) :. ()) where
  toJSON (Just p :. ()) =
    object [(M.modelName (M.modelInfo :: M.ModelInfo m)) .= toJSON p]
  toJSON (Nothing :. ()) =
    object [(M.modelName (M.modelInfo :: M.ModelInfo m)) .= object []]


instance forall m b.(Model m, ToJSON b) => ToJSON (Patch m :. b) where
  toJSON (p :. ps) = merge
    (object [(M.modelName (M.modelInfo :: M.ModelInfo m)) .= toJSON p])
    (toJSON ps)
    where
      merge :: Value -> Value -> Value
      merge (Object o1) (Object o2) =
        Object $ HM.fromList $ (HM.toList o1) ++ (HM.toList o2)

instance forall m b.(Model m, ToJSON b) => ToJSON (Maybe (Patch m) :. b) where
  toJSON (Just p :. ps)  = toJSON (p :. ps)
  toJSON (Nothing :. ps) = toJSON ps


class StripRead p where
  stripRead :: Connection -> [IdentI Role] -> p -> IO p

instance (Model m, Model (M.Parent m)) => StripRead (Patch m) where
  stripRead = stripReadPatch

instance (Model m, Model (M.Parent m)) => StripRead (Maybe (Patch m)) where
  stripRead c rs Nothing  = return Nothing
  stripRead c rs (Just p) = Just <$> stripReadPatch c rs p

instance StripRead a => StripRead (a :. ()) where
  stripRead c rs (p :. ()) = (:.) <$> stripRead c rs p <*> pure ()

instance (StripRead p, StripRead ps) => StripRead (p :. ps) where
  stripRead c rs (p :. ps) =
    (:.) <$> stripRead c rs p <*> stripRead c rs ps

class MkSelect t where
  mkSel :: t -> Text

instance Model m => MkSelect (ModelInfo m) where
  mkSel _ = T.intercalate ", " $ map (tofldName.fd_name) $ modelFields mInfo
    where
      mInfo = modelInfo :: ModelInfo m
      tofldName f = T.concat ["\"", M.tableName mInfo, "\"", ".", f]

instance Model m => MkSelect (Patch m) where
  mkSel _ = mkSel (modelInfo :: ModelInfo m)

instance Model m => MkSelect (Maybe (Patch m)) where
  mkSel _ = mkSel (modelInfo :: ModelInfo m)

instance MkSelect a => MkSelect (a :. ())where
  mkSel _ = mkSel (undefined :: a)

instance (MkSelect a, MkSelect b) => MkSelect (a :. b) where
  mkSel _ = T.concat [ mkSel (undefined :: a)
                     , ", "
                     , mkSel (undefined :: b)
                     ]

null :: RowParser Null
null =  field

instance Model m => FromRow (Maybe (Patch m)) where
  fromRow =
    (replicateM_ n null *> pure Nothing) <|> (Just <$> fromRow)
    where
      n = length $ modelFields (modelInfo :: ModelInfo m)
