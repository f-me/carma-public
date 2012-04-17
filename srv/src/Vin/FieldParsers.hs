{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Vin.FieldParsers where

import           Control.Applicative
import           Control.Monad.Instances () -- Just for Functor (Either a) instance
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as B
import qualified Data.Map as M
import           Data.Maybe
import           Data.Time

import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Data.Text.Encoding as T
import           System.Locale (defaultTimeLocale)

import Vin.Utils

class Err e where
  err :: ByteString -> e -> Either ByteString a
  error' :: ByteString -> e -> a
  error' a b = error $ B.toString msg
    where
      Left msg = err a b


instance Err ByteString where
  err a b = Left $ B.concat [a, ": ", b]

instance Err [Char] where
  err a = err a . B.fromString

instance Err (M.Map ByteString ByteString) where
  err a = err a . showMap



getKey :: M.Map ByteString a -> String -> Either a1 a
getKey m k
  = maybe (error' "Unknown key" k) Right
  $ M.lookup (B.fromString k) m


str' :: [String] -> M.Map ByteString ByteString -> Either ByteString T.Text
str' keys m
  = T.unwords . concatMap (T.words . T.decodeUtf8)
  <$> sequence (map (getKey m) keys)


str  ks m = T.encodeUtf8 <$> str' ks m
strU ks m = T.encodeUtf8 . T.toUpper <$> str' ks m
fixed = const . Right


notEmpty f m = case f m of
  Right "" -> Left "empty field"
  Right rs -> Right rs
  e        -> e


oneOf fs m = foldr
  (\f res -> either (const res) Right $ f m)
  (err "`oneOf` failed" m)
  fs


date keys m = case str' keys m of
    Left e     -> Left e
    Right ""   -> Right ""
    Right dateStr ->
      let tryParse :: (String, String -> String) -> Maybe Day
          tryParse (fmt,tr) = parseTime defaultTimeLocale fmt $ tr (T.unpack dateStr)
      in case catMaybes $ map tryParse dateFormats of
          res:_ -> Right . B.fromString $ show res
          _     -> either
                     (const $ Left "Unknown date format")
                     mkDate
                   $ T.rational dateStr
  where
    mkDate :: (Double, b) -> Either a ByteString
    mkDate = Right . B.fromString . show . utctDay . getTime . fst


maybeRead :: Read a => String -> Maybe a
maybeRead = fmap fst . listToMaybe . filter (null . snd) . reads


dateFormats =
  [("%m/%d/%Y", id)
  ,("%m/%e/%Y", head.words)        -- "10/16/2011 0:00"
  ,("%m/%e/%Y", head.words.('0':)) -- "1/16/2011 0:00"
  ,("%d.%m.%Y", id)
  ,("%e %b %Y", id)
  ,("%e-%b-%Y", id)
  ,("%d-%b-%y", id)
  ,("%m/%d/%y", id)
  ,("%b %Y",    id)]


-- Get date and time in the 1900 date base system.
getTime :: Double -> UTCTime
getTime val = UTCTime day diff
  where
    days = truncate val
    day  = addDays days $ fromGregorian 1899 12 30

    time = val - (fromIntegral days)
    sec  = round $ time * 86400
    diff = secondsToDiffTime sec


carModel keys m = do
  mod <- str keys m
  case B.words mod of
    []  -> Left "empty car model"
    model:_ -> maybe
      (err "unknown model" model)
      Right
      (M.lookup model carModels)


v <=<= ks = map (\k -> (B.fromString k, B.fromString v)) $ v:ks
carModels = M.fromList $ concat
  -- VW truck
  ["Caddy"      <=<= ["Кэдди","кедди","Кедди"]
  ,"Amarok"     <=<= []
  ,"Crafter"    <=<= ["Крафтер"]
  ,"Transporter"<=<= ["T5", "Т5", "Транспортер"]
  -- VW motor
  ,"Tiguan"     <=<= ["Тигуан","тигуан"]
  ,"Polo"       <=<= ["Поло"]
  ,"Touareg"    <=<= ["Туарег","Тouareg"]
  ,"Passat"     <=<= ["Пассат","пассат","Passft"]
  ,"Jetta"      <=<= ["Джетта"]
  ,"Golf"       <=<= ["Гольф","гольф","Гольф+"]
  ,"Touran"     <=<= ["Туран"]
  ,"Phaeton"    <=<= ["Фаэтон","фаэтон"]
  ,"Eos"        <=<= ["Эос"]
  ,"Scirocco"   <=<= ["Сирокко"]
  ,"Caravelle"  <=<= ["Каравелла"]
  ,"Multivan"   <=<= ["Мультивен"]
  ,"Sharan"     <=<= ["Шаран"]
  -- Opel
  ,"Astra"      <=<= []
  ,"Zafira"     <=<= []
  ,"Corsa"      <=<= []
  ,"Insignia"   <=<= []
  ,"Combo"      <=<= []
  ,"Meriva"     <=<= []
  ,"Antara"     <=<= []
  ,"Vectra"     <=<= []
  -- Hummer
  ,"FOCUS"      <=<= ["Фокус", "Focus"]
  ,"ESCAPE"     <=<= []
  ,"MONDEO"     <=<= ["Мондео","Mondeo"]
  ,"FIESTA"     <=<= ["Fiesta"]
  ,"FUSION"     <=<= ["Fusion"]
  ,"COUGAR"     <=<= []
  ,"KUGA"       <=<= []
  ,"GALAXY"     <=<= []
  ,"EXPLORER"   <=<= []
  ,"MAVERICK"   <=<= []
  ,"TRANSIT"    <=<= ["Transit"]
  ]
