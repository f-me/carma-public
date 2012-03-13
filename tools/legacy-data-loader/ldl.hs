{-# LANGUAGE OverloadedStrings #-}
import           Control.Exception (finally)
import           Control.Applicative
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Instances -- Just for Functor (Either a) instance
import           System.Environment (getArgs)
import           System.FilePath ((</>))
import           System.IO (stderr, hPrint)

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

import qualified Data.Enumerator as E (tryIO, Iteratee())
import           Data.CSV.Enumerator as CSV
import           Database.Redis as Redis


main = do
  dir:_ <- getArgs
  rConn <- Redis.connect Redis.defaultConnectInfo
  loadFiles dir rConn `finally` runRedis rConn quit


loadFiles dir rConn = do
  let  loadVinFile = loadXFile $ redisSetVin rConn
  let  loadCSVFile key = loadXFile $ redisSet rConn key
  loadVinFile (dir </> "vin/VIN VW легковые.csv") vwMotor_map
  loadVinFile (dir </> "vin/VIN VW коммерческие.csv") vwTruck_map
  loadVinFile (dir </> "vin/VIN OPEL.csv") opel_map
  loadCSVFile "partner" (dir </> "Партнеры.csv") partner_map
  loadCSVFile "dealer"  (dir </> "Дилеры.csv") dealer_map


-- FIXME: use `create` form snaplet-redson 
redisSet c keyPrefix val = runRedis c $ do
  Right keyInt <- incr $ B.concat ["global:", keyPrefix, ":id"]
  let keyStr = B.concat [keyPrefix,":",B.pack $ show keyInt]
  redisSetWithKey' keyStr val

redisSetVin c val
  = runRedis c $ redisSetWithKey' key val
  where
    key = B.concat ["vin:", val M.! "vin"]

redisSetWithKey' key val = do
  res <- hmset key $ M.toList val
  case res of
    Left err -> liftIO $ print err
    _ -> return ()


loadXFile store fName keyMap
  = foldCSVFile fName defCSVSettings run ()
  >>= hPrint stderr -- Just to force evaluation
  where
    run :: () -> ParsedRow MapRow -> E.Iteratee B.ByteString IO ()
    run _ (ParsedRow (Just r)) = E.tryIO
      $ either (hPrint stderr) store
      $ remap keyMap r
    run _ _ = return ()


-- | convert MapRow according to transformations described in `keyMap`.
remap keyMap m = M.fromList <$> foldl f (Right []) keyMap
  where
    f err@(Left _) _ = err
    f (Right res) (key',f)
      = f m >>= \val -> return
        (if B.null val then res else ((key',val):res))



getKey m k
  = maybe (Left $ "Unknown key: " ++ show k) Right
  $ M.lookup (B.fromString k) m

str' :: [String] -> M.Map B.ByteString B.ByteString -> Either String T.Text
str' keys m
  = T.unwords . concatMap (T.words . T.decodeUtf8)
  <$> sequence (map (getKey m) keys)

str  ks m = T.encodeUtf8 <$> str' ks m
strU ks m = T.encodeUtf8 . T.toUpper <$> str' ks m
fixed = const . Right

notEmpty f m = case f m of
  Right "" -> Left $ "empty fields: " ++ show m
  Right rs -> Right rs
  err      -> err


date keys m = case T.unpack <$> str' keys m of
  Left err -> Left err
  Right "" -> Right ""
  Right dateStr ->
    let tryParse :: String -> Maybe Day
        tryParse format = parseTime defaultTimeLocale format dateStr
    in case catMaybes $ map tryParse dateFormats of
        res:_ -> Right . B.fromString $ show res
        _     -> Left $ "Unknown date format: " ++ show dateStr

dateFormats =
  ["%m/%d/%Y", "%d.%m.%Y", "%e %b %Y" ,"%b %Y"]


carModel keys m = do
  m <- str keys m
  case B.words m of
    []  -> Left $ "empty car model in " ++ show m
    model:_ -> maybe
      (Left $ "unknown model: " ++ show model)
      Right
      (M.lookup model carModels)


carModels = M.fromList [(k,v) | (v,ks) <- modelsMap, k <- ks] 
modelsMap
  =  map ok -- VW
    ["Caddy", "Caddy", "Amarok"
    ,"Crafter", "T5"]
  ++ map ok -- Opel
    ["Astra","Zafira","Corsa","Insignia"
    ,"Combo","Meriva","Antara","Vectra"]
  where
    ok s = (s,[s])

oneOf fs m = foldr
  (\f res -> either (const res) Right $ f m)
  (Left $ "`oneOf` failed: " ++ show m)
  fs

vwMotor_map =
  [("make",         fixed "VW")
  ,("model",        carModel ["Модель"])
  ,("modelFull",    str ["Модель"])
  ,("vin",          notEmpty $ strU ["VIN"])
  ,("buyDate",      date ["Дата передачи АМ Клиенту"])
  ,("color",        str  ["Цвет авт"])
  ,("modelYear",    str  ["Модельный год"])
  ,("dealerCode",   str  ["Код дилера получателя"])
  ,("dealerName",   str  ["Дилер получатель"])
  ,("contractNo",   str  ["No Дог продажи Клиенту"])
  ,("contractDate", date ["Дата договора продажи"])
--  ,("Отч неделя", -- skip
  ,("ownerCompany", str  ["Компания покупатель"])
  ,("ownerContact", str  ["Контактное лицо покупателя"])
  ,("ownerName",    str  ["Фактический получатель ам"])
--  ,("Поле30", -- skip
  ]


vwTruck_map = 
  [("make",         fixed "VW")
  ,("model",        carModel ["модель"])
  ,("modelFull",    str ["Модель"])
  ,("vin",          notEmpty $ strU ["VIN"])
  ,("buyDate",      date ["Дата продажи"])
  ,("plateNum",     strU ["госномер"])
  ,("validUntil",   date ["Дата окончания карты"])
  ,("dealerName",   str  ["Продавец"])
  ,("cardNumber",   str  ["№ карты"])
  ,("modelYear",    str  ["модельный год"])
  ,("ownerName",    str  ["фамилия", "имя", "отчество"])
  ,("ownerAddress", str  ["индекс"
                         ,"город"
                         ,"адрес частного лица или организации"])
  ,("ownerPhone",   str  ["тел1","тел2"])
  ,("ownerCompany", str  ["название организации"])
  ]

opel_map =
  [("make",         fixed "OPEL")
  ,("model",        carModel ["Model"])
  ,("vin",          oneOf $ map (notEmpty . strU) 
                        [["VIN"], ["Previous VIN (SKD)"]])
  ,("buyDate",      date  ["Retail Date"])
--  ,("Brand",
  ,("dealerCode",   strU  ["Retail Dealer"])
  ]

partner_map =
  [("code",         str ["Code"])
  ,("cityRu",       str ["Город"])
  ,("cityEn",       str ["City"])
  ,("priority1",    str ["Приоритет за городом"])
  ,("priority2",    str ["Приоритет город"])
  ,("serviceRu",    str ["Услуга"])
  ,("serviceEn",    str ["Service"])
  ,("phones",       str ["Телефон Диспетчерской 1"
                        ,"Телефон Диспетчерской 2"
                        ,"Телефон Диспетчерской 3"
                        ,"Телефон Диспетчерской 4"])
  ,("companyName",  str ["Название компании"])
  ,("addrDeJure",   str ["Юридический адрес"])
  ,("addrDeFacto",  str ["Фактический адрес индекс","Фактический адрес улица"])
  ,("contactPerson",str ["Ответственное лицо"])
  ,("contactPhone", str ["Телефон Ответственного за Сотрудничество"])
  ,("eMail",        str ["Электронная почта"])
  ,("fax",          str ["Факс"])
  ,("price",        str ["Тариф"])
  ,("comment",      str ["Комментарий"])
  ,("status",       str ["Статус"])
  ]

dealer_map = 
  [("city",         str ["Город", "Округ"])
  ,("type",         str ["Дилер"])
  ,("name",         str ["Название дилера"])
  ,("salesAddr",    str ["Адрес отдела продаж"])
  ,("salesPhone",   str ["Телефон отдела продаж"])
  ,("salesHours",   str ["Время работы"])
  ,("techAddr",     str ["Адрес сервисного отдела"])
  ,("techPhone",    str ["Телефон сервисного отдела"])
  ,("techHours",    str ["Время работы сервисного отдела"])
  ,("program",      str ["Код_Программы_Клиенты"])
  ,("servicePhone", str ["тел для заказа услуги"])
  ,("carModels",    str ["Автомобили"])
  ,("service",      str ["предоставляемая услуга"])
  ,("workingHours", str ["время работы по предоставлению услуги"])
  ]
