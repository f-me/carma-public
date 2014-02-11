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

import           Data.Text (Text)
import           Data.Pool
import           Data.Aeson

import           GHC.Generics

-- import           Database.PostgreSQL.Simple.Types ((:.))
import           Database.PostgreSQL.Simple as PG

-- import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth hiding (Role)
import           Snap.Snaplet.PostgresqlSimple (Postgres(..), HasPostgres(..))

import qualified Data.HashMap.Strict   as HM

import           Data.Model       as M
-- import           Data.Model.Types as M
import           Data.Model.Patch (Patch)

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
                           , sorts      :: SearchSorts
                           } deriving (Show, Generic)

instance FromJSON SearchReq

data SearchSorts = SearchSorts { fields :: [SimpleField]
                               , order  :: Text
                               } deriving (Show, Generic)

instance FromJSON SearchSorts

data SimpleField = SimpleField { name :: Text, model :: Text }
                 deriving (Show, Generic)

instance FromJSON SimpleField

data SearchResult t = SearchResult 
  { values :: [t]
  , limit  :: Int
  , offset :: Int
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
