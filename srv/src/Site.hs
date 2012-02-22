{-# LANGUAGE OverloadedStrings #-}


module Site
  ( app
  ) where

import           Control.Applicative
import           Control.Monad.Trans
import           Control.Monad.State
import           Data.ByteString (ByteString)
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import qualified Data.Text as T
import           Data.Lens.Common
import qualified Data.Map as M
import           Data.List (foldl', intersect)
import           Data.Maybe (catMaybes)
import           Data.Either (rights)
import           Snap.Core
import           Snap.Snaplet
import           Snap.Util.FileServe
import           Snap.Snaplet.RedisDB
import           Data.Aeson as A
import           Data.Aeson.Types as A
import           Database.Redis
import           Application


searchCase = do
  let response = A.encode $ object
        ["iTotalRecords" .= (0::Int)
        ,"iTotalDisplayRecords" .= (0::Int)
        ,"aaData" .= toJSON ([]::[T.Text])
        ]
  modifyResponse $ setContentType "application/json"
  writeLBS response

searchDealer = searchCase


searchContractor = do
  ps <- rqParams <$> getRequest
  let s0 = head $ (M.!) ps "sSearch_0"
  let s2 = head $ (M.!) ps "sSearch_2"
  let s3 = head $ (M.!) ps "sSearch_3"

  (vals,total) <- runRedisDB redisDB $ do
    let getMatch (k,s)
          = if B.null s
          then return Nothing
          else Just <$> (keys $ B.concat [k,"*",s,"*"])
    matchingKeys  <- rights . catMaybes
        <$> mapM getMatch
          [("partner:companyName:",s0)
          ,("partner:contactPerson:",s2)
          ,("partner:contactPhone:",s3)
          ]
    ks <- if null matchingKeys
          then fromRight <$> keys "partner:*"
          else return $ foldl' intersect [] matchingKeys

    vals <- (catMaybes . fromRight) <$> (mget $ take 100 ks)
    return (vals, length ks)

  let res = catMaybes $ flip map
          (catMaybes $ map (A.decode . L.fromChunks .(:[])) vals)
          $ A.parseMaybe (\o -> mapM (o .:)
            ["companyName","cityRu"
            ,"contactPerson","contactPhone"
            ,"serviceRu"]) :: [[A.Value]]

  let response = A.encode $ object
        ["iTotalRecords" .= total
        ,"iTotalDisplayRecords" .= (100::Int)
        ,"aaData" .= toJSON res
        ]
  modifyResponse $ setContentType "application/json"
  writeLBS response


fromRight = either (const []) id


------------------------------------------------------------------------------
-- | The application's routes.
routes :: [(ByteString, Handler App App ())]
routes = [("/s", serveDirectory "resources")
         ,("/",  serveFile "resources/index.html")
         ,("/api/search_case", method GET searchCase)
         ,("/api/search_dealer", method GET searchDealer)
         ,("/api/search_contractor", method GET searchContractor)
         ]

------------------------------------------------------------------------------
-- | The application initializer.
app :: SnapletInit App App
app = makeSnaplet "app" "" Nothing $ do
    addRoutes routes
    r <- nestSnaplet "" redisDB $ redisDBInit defaultConnectInfo
    return $ App r


