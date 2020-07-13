{-# LANGUAGE ScopedTypeVariables,
             TypeOperators,
             ExistentialQuantification,
             MultiParamTypeClasses,
             FlexibleInstances
 #-}

module Snaplet.Search.Utils where

import           Data.Maybe
import           Data.Either

import           Data.List (intercalate)
import           Data.Aeson
import           Data.Text (Text, unpack)
import qualified Data.ByteString.Char8 as B

import           Database.PostgreSQL.Simple as PG
import           Snap.Snaplet.PostgresqlSimple (liftPG')

import           Data.Model       as M
import           Text.Printf

import           Snap
import           Snaplet.Auth.PGUsers (currentUserRoles)
import           Snaplet.Search.Types
import           Carma.Model.Search
import           Util (readJSONfromLBS, withLens)


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
      (errs, _)   -> return $ Left $ concat errs

mkSearch :: (RenderPrms p, FromRow t, MkSelect t)
         => p
         -> (t -> Text -> Int -> Int -> String -> Query)
         -> SearchHandler b (Either String (SearchResult t))
mkSearch prms mkq = do
  lim      <- getLimit
  offset   <- getOffset
  args     <- getJsonBody
  withLens db $ liftPG' $ \c -> do
    prms' <- renderPrms c (predicates args) prms
    case prms' of
      Right p -> do
        -- retrieving one more elements than limit, so we know
        -- if there is next page
        s  <- query_ c (mkq (undefined :: t) p (lim + 1) offset
                        (renderOrder $ sorts args))
        return $ Right $ buildResult s lim offset
      Left es -> return $ Left es
  where
    buildResult :: [t] -> Int -> Int -> SearchResult t
    buildResult res lim offset =
      let next = if length res <= lim then Nothing else Just (offset + lim)
          prev = if offset - lim < 0  then Nothing else Just (offset - lim)
          res' = if length res <= lim then res     else init res
      in SearchResult res' next prev

renderOrder :: Order -> String
renderOrder (Order fs ord) = printf "ORDER BY %s" $
  intercalate (", " :: String) $ map printIdent fs
  where
    printIdent (FieldIdent t n) =
      printf "\"%s\".%s %s" (unpack t) (unpack n) printOrd
    printOrd :: String
    printOrd = case ord of
      Asc  -> "ASC"
      Desc -> "DESC"

getJsonBody :: FromJSON v => SearchHandler b v
getJsonBody = readJSONfromLBS <$> readRequestBody 4096

getLimit :: SearchHandler b Int
getLimit
  = fromMaybe 10 . (>>= fmap fst . B.readInt)
  <$> getParam "limit"

getOffset :: SearchHandler b Int
getOffset
  = fromMaybe 0 . (>>= fmap fst . B.readInt)
  <$> getParam "offset"

stripResults :: StripRead t
             => SearchResult t -> SearchHandler b (SearchResult t)
stripResults s@SearchResult{..} = do
  Just roles <- currentUserRoles
  withLens db $ liftPG' $ \c -> do
    vals <- mapM (stripRead c roles) values
    return $ s{ values = vals }

defaultSearch :: (RenderPrms p, FromRow t, MkSelect t, StripRead t)
              => p
              -> (t -> Text -> Int -> Int -> String -> Query)
              -> SearchHandler b (Either String (SearchResult t))
defaultSearch searchParams mkq = do
  r <- mkSearch searchParams mkq
  case r of
    Left  e -> return $ Left e
    Right v -> Right <$> stripResults v
