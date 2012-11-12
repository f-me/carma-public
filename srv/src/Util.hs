
{-# LANGUAGE DeriveDataTypeable, TemplateHaskell #-}
module Util
  (readJSON
  ,readJSONfromLBS
  ,UsersDict(..)
  ,selectParse
  ,mbreadInt
  ,mbreadDouble
  ,readDouble
  ,lookupNE
  ,selectPrice
  ,printBPrice
  ,getCostField
  ) where

import qualified Data.Map as Map
import Data.Map (Map)
import qualified Data.Vector as V
import Data.Maybe

import Control.Exception
import Control.Applicative
import Data.Typeable
import qualified Data.ByteString.Lazy  as L
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lex.Double as B
import Data.Aeson as Aeson
import Data.Aeson.TH
import Data.Aeson.Types (Parser)
import Data.Attoparsec.ByteString.Lazy (Result(..))
import qualified Data.Attoparsec.ByteString.Lazy as Atto

import qualified Database.Redis       as Redis
import qualified Snap.Snaplet.RedisDB as Redis

import Data.Attoparsec.Combinator (many1, choice)
import qualified Data.Attoparsec.ByteString.Char8 as A

import Text.Printf (printf)

data JSONParseException
  = AttoparsecError FilePath String
  | FromJSONError FilePath String
  deriving (Show, Typeable)

instance Exception JSONParseException


readJSON :: FromJSON v => FilePath -> IO v
readJSON f = readJSONfromLBS' f `fmap` L.readFile f

readJSONfromLBS :: FromJSON v => L.ByteString -> v
readJSONfromLBS = readJSONfromLBS' "LBS"

readJSONfromLBS' :: FromJSON v => String -> L.ByteString -> v
readJSONfromLBS' src s
  = case Atto.parse Aeson.json' s of
    Done _ jsn -> case Aeson.fromJSON jsn of
      Success t -> t
      Error err -> throw $ FromJSONError src err
    err -> throw $ AttoparsecError src (show err)

--------------------------------------------------------------------------------

data UsersDict = UsersDict [Map.Map B.ByteString B.ByteString]
                 deriving (Show)

instance FromJSON UsersDict where
  parseJSON (Object v) = do
    Array c <- v .: "uidCache"
    r <- V.mapM (parseUser) c
    return $ UsersDict $ V.toList r
      where
        parseRoles r = V.mapM (parseJSON) r >>=
                       return . (B.intercalate ",") . V.toList
        parseUser a = do
          Array u <- parseJSON a
          Object u' <- parseJSON $ u V.! 1
          value <- u' .: "login"
          Array roles <- u' .: "roles"
          roles' <- parseRoles roles
          meta  <- u' .: "meta"
          label <- meta .:? "realName" .!= ""
          return $ Map.fromList [ ("value", value)
                                , ("label", label)
                                , ("roles", roles')
                                ]

  parseJSON _ = fail "bad arg"

$(deriveToJSON id ''UsersDict)

--------------------------------------------------------------------------------
-- param parser for select
sParse prm =
  let A.Done _ r = A.feed (A.parse (c) prm) B.empty
  in r
    where
      n = return . B.pack =<< (trim $ many1 $ A.satisfy $ A.notInClass "<>=")
      p = trim $ choice $ map (A.string) ["<=", "<", ">=", ">", "=="]
      c = do
        v    <- n
        pred <- p
        l    <- n
        return [v, pred, l]
      trim p = A.skipSpace *> p <* A.skipSpace

s2p "<=" = (<=)
s2p "<"  = (<)
s2p ">"  = (>)
s2p ">=" = (>=)
s2p "==" = (==)

selectParse obj prm =
  let [l,p,r] = sParse prm
      p' = s2p p
  in case Map.lookup l obj of
    Nothing -> False
    Just v  -> p' v r

mbreadInt :: B.ByteString -> Maybe Int
mbreadInt s = B.readInt s >>= r
  where  r (i, "") = Just i
         r _       = Nothing

mbreadDouble :: B.ByteString -> Maybe Double
mbreadDouble s =  B.readDouble s >>= r
  where r (i, "") = Just i
        r _       = Nothing

readDouble :: B.ByteString -> Double
readDouble = fromMaybe 0 . mbreadDouble

-- | Like Map.lookup but treat Just "" as Nothing
lookupNE :: Ord k => k -> Map k B.ByteString -> Maybe B.ByteString
lookupNE key obj = Map.lookup key obj >>= lp
  where lp "" = Nothing
        lp v  = return v

calcCost srv opt = getCost srv opt >>= calcCost'
  where calcCost' cost = do
          count <- lookupNE "count" opt >>= mbreadDouble
          cost' <- mbreadDouble cost
          return $ printBPrice $ cost' * count

getCost opt srv = getCostField srv >>= flip lookupNE opt

getCostField srv = lookupNE "payType" srv >>= selectPrice

selectPrice v
          | v == "ruamc"                             = Just "price2"
          | any (== v) ["client", "mixed", "refund"] = Just "price1"
          | otherwise                               = Nothing

printPrice p = printf "%.2f" p
printBPrice p = B.pack $ printPrice p