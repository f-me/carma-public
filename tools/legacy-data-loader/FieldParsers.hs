{-# LANGUAGE OverloadedStrings, FlexibleInstances #-}

module FieldParsers where

import           Control.Applicative
import           Control.Monad.Instances () -- Just for Functor (Either a) instance

import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as B
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import qualified Data.Map as M

import           Data.Maybe
import           Data.Time.Format (parseTime)
import           Data.Time.Clock  (UTCTime)
import           Data.Time.Calendar (Day)
import           Data.Time.LocalTime 
import           System.Locale (defaultTimeLocale)



class Err e where
  err :: ByteString -> e -> Either ByteString a

instance Err ByteString where
  err a b = Left $ B.concat [a, ": ", b]  

instance Err [Char] where
  err a = err a . B.fromString



getKey m k
  = maybe (err "Unknown key" k) Right
  $ M.lookup (B.fromString k) m


str' :: [String] -> M.Map ByteString ByteString -> Either ByteString T.Text
str' keys m
  = T.unwords . concatMap (T.words . T.decodeUtf8)
  <$> sequence (map (getKey m) keys)


str  ks m = T.encodeUtf8 <$> str' ks m
strU ks m = T.encodeUtf8 . T.toUpper <$> str' ks m
fixed = const . Right


notEmpty f m = case f m of
  Right "" -> err "empty fields" (show m)
  Right rs -> Right rs
  e        -> e


oneOf fs m = foldr
  (\f res -> either (const res) Right $ f m)
  (err "`oneOf` failed" (show m))
  fs


date keys m = case T.unpack <$> str' keys m of
  Left err -> Left err
  Right "" -> Right ""
  Right dateStr ->
    let tryParse :: String -> Maybe Day
        tryParse format = parseTime defaultTimeLocale format dateStr
    in case catMaybes $ map tryParse dateFormats of
        res:_ -> Right . B.fromString $ show res
        _     -> err "Unknown date format" (show dateStr)

dateFormats =
  ["%m/%d/%Y", "%d.%m.%Y", "%e %b %Y" ,"%b %Y"]


carModel keys m = do
  mod <- str keys m
  case B.words mod of
    []  -> err "empty car model" (show m)
    model:_ -> maybe
      (err "unknown model" model)
      Right
      (M.lookup model carModels)


carModels = M.fromList [(k,v) | (v,ks) <- modelsMap, k <- ks] 
modelsMap
  =  map ok -- VW truck
    ["Caddy", "Caddy", "Amarok"
    ,"Crafter", "T5"]
  ++ map ok
    ["Tiguan", "Polo", "Touareg"
    ,"Passat", "Jetta", "Golf", "Touran"
    ,"Phaeton", "Eos", "Scirocco"]
  ++ map ok -- Opel
    ["Astra","Zafira","Corsa","Insignia"
    ,"Combo","Meriva","Antara","Vectra"]
  where
    ok s = (s,[s])

