{-# LANGUAGE QuasiQuotes #-}
module AppHandlers.ModelFilter (filterPartner) where

import           Data.Maybe

import qualified Data.Aeson          as A
import qualified Data.HashMap.Strict as HM
import qualified Data.Vector         as V

import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text                  as T

import           Control.Monad
import           Control.Applicative

import Database.PostgreSQL.Simple (Query, query)
import Database.PostgreSQL.Simple.SqlQQ

import           Snap

import           Application
import           AppHandlers.Util

q :: Query
q = [sql|
SELECT contractfield, (case showform when true then 't' else 'f' end)
FROM programpermissionstbl
WHERE parentid is not null AND split_part(parentid, ':', 2) = ?
|]

filterPartner :: AppHandler ()
filterPartner = do
  f  <- liftIO $ L8.readFile "resources/site-config/models/contract.js"
  pid <- fromJust <$> getParam "id"
  perms <- (withPG pg_search $
           \conn -> query conn q [pid]) :: AppHandler [(T.Text, T.Text)]
  let o = fromJust $ join (A.decode f :: Maybe (Maybe A.Object))
      perms' = HM.fromList perms
      o' = HM.foldrWithKey (\k v a -> filterFields perms' k v a) HM.empty o
  writeJSON o'

filterFields :: HM.HashMap T.Text T.Text ->
                T.Text -> A.Value -> HM.HashMap T.Text A.Value -> A.Object
filterFields perms "fields" (A.Array v) a =
  let v' = A.Array $ V.filter (isCanShow perms) v
  in HM.insert "fields" v' a
filterFields _     k        v a = HM.insert k v a

isCanShow :: HM.HashMap T.Text T.Text -> A.Value -> Bool
isCanShow perms (A.Object f) =
  let A.String name = fromJust $ HM.lookup "name" f
  in case HM.lookup name perms >>= return . ("t" ==) of
    Nothing -> False
    Just _  -> True