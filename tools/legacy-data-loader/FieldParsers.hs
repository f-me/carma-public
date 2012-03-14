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
    let tryParse :: (String, String -> String) -> Maybe Day
        tryParse (fmt,tr) = parseTime defaultTimeLocale fmt $ tr dateStr
    in case catMaybes $ map tryParse dateFormats of
        res:_ -> Right . B.fromString $ show res
        _     -> err "Unknown date format" (show dateStr)

dateFormats =
  [("%m/%d/%Y", id)
  ,("%m/%e/%Y", head.words)        -- "10/16/2011 0:00"
  ,("%m/%e/%Y", head.words.('0':)) -- "1/16/2011 0:00"
  ,("%d.%m.%Y", id)
  ,("%e %b %Y", id)
  ,("%b %Y",    id)]


carModel keys m = do
  mod <- str keys m
  case B.words mod of
    []  -> err "empty car model" (show m)
    model:_ -> maybe
      (err "unknown model" model)
      Right
      (M.lookup model carModels)


v <=<= ks = map (\k -> (B.fromString k, B.fromString v)) $ v:ks
carModels = M.fromList $ concat
  -- VW truck
  ["Caddy"     <=<= ["Кэдди","кедди","Кедди"]
  ,"Amarok"    <=<= []
  ,"Crafter"   <=<= ["Крафтер"]
  ,"T5"        <=<= ["Т5"]
  -- VW motor
  ,"Tiguan"    <=<= ["Тигуан","тигуан"]
  ,"Polo"      <=<= ["Поло"]
  ,"Touareg"   <=<= ["Туарег","Тouareg"]
  ,"Passat"    <=<= ["Пассат","пассат","Passft"]
  ,"Jetta"     <=<= ["Джетта"]
  ,"Golf"      <=<= ["Гольф","гольф","Гольф+"]
  ,"Touran"    <=<= ["Туран"]
  ,"Phaeton"   <=<= ["Фаэтон","фаэтон"]
  ,"Eos"       <=<= ["Эос"]
  ,"Scirocco"  <=<= ["Сирокко"]
  ,"Caravelle" <=<= ["Каравелла"]
  ,"Multivan"  <=<= ["Мультивен"]
  ,"Tx"        <=<= ["Транспортер", "Transporter"]
  ,"Sharan"    <=<= ["Шаран"]
  -- Opel
  ,"Astra"     <=<= []
  ,"Zafira"    <=<= []
  ,"Corsa"     <=<= []
  ,"Insignia"  <=<= []
  ,"Combo"     <=<= []
  ,"Meriva"    <=<= []
  ,"Antara"    <=<= []
  ,"Vectra"    <=<= []
  ]

