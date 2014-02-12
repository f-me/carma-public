{-# LANGUAGE ScopedTypeVariables,
             TypeOperators,
             ExistentialQuantification,
             MultiParamTypeClasses,
             FlexibleInstances
 #-}

module Snaplet.Search.Utils where

import           Control.Applicative ((<$>))
import           Control.Monad.State

import           Data.Maybe
import           Data.Either

import           Data.List (intercalate)
import           Data.Pool
import           Data.Aeson
import           Data.Text (Text, toLower, unpack)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy  as LB
import qualified Data.HashMap.Strict   as HM

import           Database.PostgreSQL.Simple as PG

import           Data.Model       as M
import           Data.Model.Patch (Patch, empty)

import           Snap.Core
import           Snap.Snaplet
import           Snap.Snaplet.Auth hiding (Role)

import           Text.Printf
import           Util
import           Utils.Roles

import           Snaplet.Search.Types
import           Carma.Model.Search
import           Carma.Model.Role


class ParamPred m where
  predFromParam :: Value -> [(Text, [Predicate m])]

class RenderPrms p where
  renderPrms :: Connection -> Object -> p -> IO (Either String Text)

instance Model m => RenderPrms [(Text, [Predicate m])] where
  renderPrms c v p = predicatesFromParams c v p

instance Model m => RenderPrms ([(Text, [Predicate m])] :. ()) where
  renderPrms c v (p :. ()) = renderPrms c v p

instance (Model m, RenderPrms ps)
         => RenderPrms ([(Text, [Predicate m])] :. ps) where
  renderPrms c v (p :. ps) = do
    p'  <- renderPrms c v p
    ps' <- renderPrms c v ps
    case partitionEithers [p', ps'] of
      ([], preds) -> return $ Right $ concatPredStrings preds
      (errs, _)   -> return $ Left $ foldl (++) "" errs


mkSearch :: (RenderPrms p, FromRow s)
         => p
         -> (Text -> Int -> Int -> String -> Query)
         -> (Connection -> [IdentI Role] -> s -> IO t)
         -> SearchHandler b (Either String (SearchResult t))
mkSearch prms mkq patchParser = do
  roles    <- with auth currentUser >>= userRolesIds . fromJust
  lim      <- getLimit
  offset   <- getOffset
  args     <- getJsonBody
  withPG $ \c -> do
    prms' <- renderPrms c (predicates args) prms
    case prms' of
      Right p -> do
        -- retrieving onw more elements than limit, so we know
        -- is there is next page
        s  <- query_ c (mkq p (lim + 1) offset (renderOrder $ sorts args))
        s' <- mapM (patchParser c roles) s
        return $ Right $ buildResult s' lim offset
      Left es -> return $ Left es
  where
    buildResult :: [t] -> Int -> Int -> SearchResult t
    buildResult res lim offset =
      let next = if length res <= lim then Nothing else Just (offset + lim)
          prev = if offset - lim < 0  then Nothing else Just (offset - lim)
          res' = if length res <= lim then res     else init res
      in SearchResult res' next prev

parsePgJson :: forall m.Model m => LB.ByteString -> Patch m
parsePgJson bs =
  fromMaybe empty $ decode bs >>= decodeJs >>= fromResult . fromJSON
  where
    fromResult (Error s) = error $ "Error while parsing patch for: " ++
      (show $ M.modelName (M.modelInfo :: ModelInfo m)) ++
      ", error: " ++ s
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

renderOrder :: Order -> String
renderOrder (Order fs ord) = printf "ORDER BY %s" $
  intercalate (", " :: String) $ map printIdent fs
  where
    printIdent (FieldIdent t n) =
      printf "%s.%s %s" (unpack t) (unpack n) printOrd
    printOrd :: String
    printOrd = case ord of
      Asc  -> "ASC"
      Desc -> "DESC"

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

