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

import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Lens (makeLenses)
import           Control.Monad.State

import           Data.Maybe
import           Data.Text
import           Data.Pool
import           Data.Aeson

import           GHC.Generics

import           Database.PostgreSQL.Simple as PG

import           Snap.Snaplet
import           Snap.Snaplet.Auth hiding (Role)
import           Snap.Snaplet.PostgresqlSimple (Postgres(..), HasPostgres(..))

import qualified Data.Vector           as V
import qualified Data.HashMap.Strict   as HM

import           Data.Model       as M
import           Data.Model.Patch (Patch)

import           Carma.Model
import           Carma.Model.Role
import           Carma.Model.FieldPermission


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

instance forall m b.(Model m, ToJSON b) => ToJSON (Patch m :. b) where
  toJSON (p :. ps) = merge
    (object [(M.modelName (M.modelInfo :: M.ModelInfo m)) .= toJSON p])
    (toJSON ps)
    where
      merge :: Value -> Value -> Value
      merge (Object o1) (Object o2) =
        Object $ HM.fromList $ (HM.toList o1) ++ (HM.toList o2)


class StripRead p where
  stripRead :: Connection -> [IdentI Role] -> p -> IO p

instance (Model m, Model (M.Parent m)) => StripRead (Patch m) where
  stripRead = stripReadPatch
instance (Model m, Model (M.Parent m)) => StripRead (Patch m :. ()) where
  stripRead c rs (p :. ()) = (:.) <$> stripReadPatch c rs p <*> pure ()
instance (Model m, Model (M.Parent m), StripRead ps)
         => StripRead (Patch m :. ps) where
  stripRead c rs (p :. ps) =
    (:.) <$> stripReadPatch c rs p <*> stripRead c rs ps
