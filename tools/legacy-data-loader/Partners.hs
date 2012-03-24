
{-# LANGUAGE OverloadedStrings #-}

module Partners where

import           System.FilePath ((</>))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import           Data.List
import           Control.Monad (forM_)
import           Database.Redis
import qualified Data.Aeson as Aeson

import Debug.Trace
import Utils
import FieldParsers

loadFiles dir rConn = do
--  loadCSVFile "dealer"  (dir </> "Дилеры.csv") dealer
  res <- loadXFile' M.empty processPartners (dir </> "Партнеры.csv") partner
  case res of
    Left err -> print err
    Right ps -> forM_ (M.elems ps) 
      $ redisSet rConn "partner"
      . addJsonArray "services" 

addJsonArray name (item,array)
  = M.insert name (lbs2sbs $ Aeson.encode array) item
    
lbs2sbs = B.concat . LB.toChunks


processPartners partnerIx partnerMap = do
  let pName = M.findWithDefault "---" "name" partnerMap
  let partnerAndService = mapSnd (:[]) $ extract partnerMap
        ["serviceRu", "serviceEn"
        ,"code","price"
        ,"priority1", "priority2"
        ,"comment", "status"]
  return $! M.insertWith' joinPartners pName
      partnerAndService partnerIx


joinPartners (m1,svc1) (m2,svc2)
  = (M.fromList $ foldl' f [] allProps, svc1 ++ svc2)
  where
    allProps = nub $ M.keys m1 ++ M.keys m2
    f res p = case (M.lookup p m1, M.lookup p m2) of
      (Just v1, Just v2)
        | v1 == v2  -> (p,v1) : res
        | otherwise -> trace (show v1 ++ " =/= " ++ show v2) (p,v1) : res
      (Just v1, _)  -> (p,v1) : res
      (_, Just v2)  -> (p,v2) : res
      _ -> error "impossible happened"



extract m = foldl' f (m, M.empty)
  where
    f (m,m') prop = case M.lookup prop m of
      Just val -> (M.delete prop m, M.insert prop val m')
      Nothing  -> (m,m')


mapSnd f (a,b) = (a, f b)


partner =
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
  ,("name",         str ["Название компании"])
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

dealer = 
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
  ,("serviceRu",    str ["предоставляемая услуга"])
  ,("workingHours", str ["время работы по предоставлению услуги"])
  ]
