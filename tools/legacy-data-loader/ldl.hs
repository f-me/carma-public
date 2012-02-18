
{-# LANGUAGE OverloadedStrings, NoMonomorphismRestriction #-}
{-# LANGUAGE DeriveDataTypeable #-}
import Prelude hiding (catch)
import Control.Applicative
import Control.Monad (join, when)
import Control.Exception
import Data.Typeable

import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Text as T
import Data.Maybe

import Data.Time.Format (parseTime)
import Data.Time.Clock  (UTCTime)
import Data.Time.Calendar (Day)
import Data.Time.LocalTime 
import System.Locale (defaultTimeLocale)
import System.Environment (getArgs)

import Data.Enumerator as E hiding (mapM, map)
import qualified Data.Enumerator.Binary as E
import Data.CSV.Enumerator as CSV
import Data.Aeson
import Data.Aeson.Types


-- Time to upgrade to GHC 7.4?
-- 7.0.* still does not work properly with non-ascii filenames
import qualified Codec.Binary.UTF8.String as UTF8


fixName = UTF8.encodeString 



main = do
  dir:_ <- getArgs
  let f = fixName . (dir++)
  fromCSV (f "/Журнал_звонков.csv") $ toRedis case_info


fromCSV fname f = do
  E.run_ $ E.enumFile fname $$ iterCSV defCSVSettings act True
  where
    act x CSV.EOF = yield x E.EOF
    act skip  (ParsedRow (Just row))
        = when (not skip) (tryIO $ f row)
        >> yield False (Chunks [])

toRedis f row = do
  jsn <- (return $! f row) 
    `catch`
      \(Ex msg o) -> print msg >> L.putStrLn (encode $ Object o) >> return Nothing
  L.putStrLn $ encode jsn

mkObj :: [T.Text] -> [B.ByteString] -> Value
mkObj = (object.) . zipWith (.=)

case_info row = do
  return $! object
    ["timestamp" .= fromMaybe (err "timestamp") (mkTimestamp o) -- 0  Дата звонка -- 1  Время звонка
    ,"callTaker" .= pStr "2"  -- Сотрудник (Обязательное поле)
    ,"program"   .= pStr "3"  -- Клиент (Обязательное поле) FIXME: на самом деле, это программа
    ,"service"   .= pStr "4"  -- Услуга  (Обязательное поле)
    ,"client"    .= pStrs ["5","6"] -- 5  Фамилия звонящего -- 6  Имя отчество звонящего
    ,"carOwner"  .= pStrs ["7","8"] -- 7  Фамилия владельца -- 8  Имя отчество владельца
    ,"phones"    .= pStrs ["9","10"] -- 9  Мобильный телефон -- 10 Дополнительный телефон
    ,"carModel"  .= pStrs ["11","12","13"]  -- 11 Марка автомобиля -- 12 Модель автомобиля -- 13 Другая марка / модель авто
    ,"plateNum"  .= pStr "14" -- Регистрационный номер автомобиля
    ,"color"     .= pStr "15" -- Цвет
    ,"vin"       .= pStr "16" -- VIN автомобиля
    ,"vinCheck"  .= pStr "17" -- 17 VIN Проверен
      -- 18 Дата покупки автомобиля
      -- 19 Дата  прохождения ТО FORD
      -- 20 Пробег автомобиля (км)
      -- 21 Пробег автомобиля на момент прохождения ТО (только для FORD)
      -- 22 Дилер продавший авто / прохождение ТО FORD
      -- 23 Описание неисправности со слов клиента
      -- 24 Система в которой произошла неисправность
      -- 25 Неисправная деталь
      -- 26 Адрес места поломки
      -- 27 Введите дату в формате + или - число -- FIXME: ерунда какая-то
      -- 28 Город дилера куда эвакуируют автомобиль
      -- 29 Название дилера куда эвакуируют автомобиль
      -- 30 Адрес куда эвакуируют автомобиль
      -- 31 Номер происшествия в Arc Time
      -- 32 Название партнёра
      -- 33 Реальное время прибытия на место поломки
      -- 34 Реальное время окончания услуги
      -- 35 Стоимость услуги у партнёра
      -- 36 Расшифровка стоимости
      -- 37 Перепробег (информация от партнера)
      -- 38 Номер заказ-наряда Saga (только для VW)
      -- 39 Статус случая от дилера (Только для VW)
      -- 40 Дата окончания ремонта автомобиля у дилера (только VW)
      -- 41 Описание причины неисправности со слов дилера (только VW)
      -- 42 Комментарий
      -- 43 Статус звонка (Обязательное поле)
    ]
  where
    pStrs = T.strip . T.intercalate " " . map pStr
    pStr :: T.Text -> T.Text
    pStr  = fromJust . parseMaybe (o .:)
    err msg = throw $ Ex msg o
    Object o = mkObj [T.pack $ show i | i <- [0..]] row
    key = "call"

data Ex = Ex String Object deriving (Show, Typeable)
instance Exception Ex

mkTimestamp :: Object -> Maybe UTCTime
mkTimestamp = join . parseMaybe getCallTime
getCallTime o = do
  let pTm = parseTime defaultTimeLocale 
  d <- pTm "%m/%d/%Y" <$> o .: "0"
  t <- pTm "%k:%M"    <$> o .: "1"
  let t' = maybe (Just midday) Just t
  return $ localTimeToUTC (read "+0400") <$> (LocalTime <$> d <*> t')

