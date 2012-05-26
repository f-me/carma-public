{-# LANGUAGE OverloadedStrings #-}

module Snap.Snaplet.Search
    ( Search
    , searchInit
    )
where

import Control.Monad (liftM)
import Data.Char
import Data.Functor ((<$>))
import Data.Maybe (fromMaybe)

import Data.Aeson as A
import Data.Lens.Common
import Database.Redis
import Snap.Core
import Snap.Snaplet
import Snap.Snaplet.Redson
import Snap.Snaplet.Redson.Search

import qualified Data.ByteString as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Char8 as BC
import qualified Data.Map as M

------------------------------------------------------------------------------
-- | Search snaplet state type.
data Search b =
    Search { redson :: Lens b (Snaplet (Redson b))
           }

rangeParse :: B.ByteString -> Maybe (Double, Double)
rangeParse str =
    monadPairZip md1 md2'
  where
    (h, t) = BU.break (== '-') str
    t' = BU.drop 1 t
    md1 = maybeRead $ BU.toString $ BC.filter (not . isSpace) h
    md2 = maybeRead $ BU.toString $ BC.filter (not . isSpace) t'
    md2' = maybe md1 Just md2
    monadPairZip f g = do { a <- f; b <- g; return (a, b); }

redisSearch :: [(FieldName, IndexType)] -> B.ByteString -> Redis [InstanceId]
redisSearch lst query =
    liftM concat $ mapM
      (\(typ, fNames) ->
        case typ of
          Sorted ->
            case rangeParse query of
              Just (d1, d2) ->
                redisRangeSearch m fNames d1 d2
              Nothing ->
                return []
          FullText -> redisFullTextSearch m fNames query
          Reverse -> redisReverseSearch m fNames query
      ) $ repack lst

repack :: Ord a => [(b, a)] -> [(a, [b])]
repack lst = 
    M.toList mp
  where
    mp = repack_ lst M.empty

    repack_ [] m = m
    repack_ ((x, y):zs) m = 
      repack_ zs $ M.insertWith (++) y [x] m

-- Fetch instance by id to JSON
fetchInstance id key = 
  runRedisDB database $ do
    Right r <- hgetall key
    return (M.fromList $ ("id", id):r)

-----------------------------------------------------------------------------
-- | Serve model instances which have index values containing supplied
-- search parameters.
--
-- Currently not available in transparent mode.
search :: Handler b (Redson b) ()
search =
    ifTop $ withCheckSecurity $ \_ mdl -> do
      case mdl of
        Nothing -> handleError notFound
        Just m -> 
          do
            -- TODO: Mark these field names as reserved
            outFields <- maybe [] (B.split comma) <$>
                              getParam "_fields"
            itemLimit   <- fromIntParam "_limit" defaultSearchLimit
            query       <- fromMaybe "" <$> getParam "q"

            let collater c = if c then CRUD.collateValue else Prelude.id

            termIds <- runRedisDB database $
              redisSearch (M.toList $ indices m) query
             
            modifyResponse $ setContentType "application/json"

            case termIds of
                [] -> writeLBS $ A.encode ([] :: [Value])
                tids -> do
                  -- Finally, list of matched instances
                  instances <- take itemLimit <$>
                                mapM (\id -> fetchInstance id $
                                        CRUD.instanceKey mname id)
                                        tids
                  -- If _fields provided, leave only requested
                  -- fields and serve array of arrays. Otherwise,
                  -- serve array of objects.
                  case outFields of
                    [] -> writeLBS $ A.encode instances
                    _ -> writeLBS $ A.encode $
                          map (`CRUD.onlyFields` outFields) instances
           where
             mname = modelName m
             comma = 0x2c

-----------------------------------------------------------------------------
-- | CRUD routes for models.
routes :: [(B.ByteString, Handler b (Search b) ())]
routes = [ (":model/search/", method GET search)
         ]

------------------------------------------------------------------------------
-- | Make Search snaplet.
searchInit :: Lens b (Snaplet (Redson b))
           -> SnapletInit b (Search b)
searchInit topRedson =
    makeSnaplet "snaplet-search" "Search snaplet." Nothing $ do
      addRoutes routes
      return $ Search topRedson
