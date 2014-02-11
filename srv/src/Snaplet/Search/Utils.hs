{-# LANGUAGE ScopedTypeVariables,
             TypeOperators,
             ExistentialQuantification,
             MultiParamTypeClasses,
             FlexibleInstances
 #-}

module Snaplet.Search.Utils where

import           Control.Applicative ((<$>), (<*>), pure)
import           Control.Monad
import           Control.Monad.State

import           Data.Maybe
import           Data.Either

import           Data.Pool
import           Data.Aeson
import           Data.Text (Text, toLower, append)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as LB
import qualified Data.HashMap.Strict   as HM

import           Database.PostgreSQL.Simple as PG
import           Database.PostgreSQL.Simple.FromField

import           Data.Model       as M
import           Data.Model.Types as M

import           Data.Model.Patch (Patch, empty)

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth hiding (Role)

import           Util
import           Utils.Roles

import           Snaplet.Search.Types
import           Carma.Model.Search
import           Carma.Model.Role


data Params = forall m.Model m => Params [(Text, [Predicate m])]

class ParamPred m where
  predFromParam :: Value -> [(Text, [Predicate m])]



-- buildPreds :: ParamPred p
--            => Connection -> Value -> p -> IO (Either [String] Text)
-- buildPreds c preds ps = partitionEithers $ map buildPred ps
--   where
--     buildPred (Params p) = predicatesFromParams c preds p

-- searchPreds :: Connection -> Value -> IO (Either [String] t)
-- searchPreds c preds

class RenderPrms p where
  renderPrms :: Connection -> Object -> p -> IO (Either String Text)

instance Model m => RenderPrms [(Text, [Predicate m])] where
  renderPrms c v p = predicatesFromParams c v p

instance Model m => RenderPrms ([(Text, [Predicate m])] :. ()) where
  renderPrms c v (p :. ()) = renderPrms c v p

instance (Model m, RenderPrms ps)
         => RenderPrms ([(Text, [Predicate m])] :. ps) where
  renderPrms c v (p :. ps) = do
    p  <- renderPrms c v p
    ps <- renderPrms c v ps
    case partitionEithers [p, ps] of
      ([], preds) -> return $ Right $ concatPredStrings preds
      (errs, _)   -> return $ Left $ foldl (++) "" errs


mkSearch :: (RenderPrms p, FromRow s)
         => p
         -> (Text -> Int -> Int -> Text -> Query)
         -> (Connection -> [IdentI Role] -> s -> IO t)
         -> SearchHandler b (Either String (SearchResult t))
mkSearch prms mkq parsePatch = do
  roles    <- with auth currentUser >>= userRolesIds . fromJust
  lim      <- getLimit
  offset   <- getOffset
  args     <- getJsonBody
  withPG $ \c -> do
    prms' <- renderPrms c (predicates args) prms
    case prms' of
      Right p -> do
        s  <- query_ c (mkq p lim offset "")
        s' <- mapM (parsePatch c roles) s
        return $ Right $ SearchResult s' lim offset
      Left es -> return $ Left es

parsePgJson :: forall m.Model m => LB.ByteString -> Patch m
parsePgJson bs =
  fromMaybe empty $ decode bs >>= decodeJs >>= fromResult . fromJSON
  where
    fromResult (Error s)   = Nothing
    fromResult (Success r) = Just r
    decodeJs (Object obj) =
      Just $ Object $ HM.foldlWithKey' fixName HM.empty obj
    decodeJs _ = Nothing
    fsMap    = M.modelFieldsMap (M.modelInfo :: M.ModelInfo m)
    namesMap =
      foldl (\a k -> HM.insert (toLower k) k a) HM.empty $ HM.keys fsMap
    fixName h k v = maybe h (\f -> HM.insert f v h) $ HM.lookup k namesMap

parsePatch :: Model m => Maybe LB.ByteString -> Patch m
parsePatch (Just v) = parsePgJson v
parsePatch Nothing  = empty


withPG :: (Connection -> IO a) -> SearchHandler b a
withPG f = gets pg >>= liftIO . (`withResource` f)

getJsonBody :: FromJSON v => SearchHandler b v
getJsonBody = Util.readJSONfromLBS <$> readRequestBody 4096

getLimit :: SearchHandler b Int
getLimit
  = fromMaybe 10 . (>>= fmap fst . B.readInt)
  <$> getParam "limit"

getOffset :: SearchHandler b Int
getOffset
  = fromMaybe 0 . (>>= fmap fst . B.readInt)
  <$> getParam "offset"

writeJSON :: ToJSON v => v -> Handler a b ()
writeJSON v = do
  modifyResponse $ setContentType "application/json"
  writeLBS $ encode v

