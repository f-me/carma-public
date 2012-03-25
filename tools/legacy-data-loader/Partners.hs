
{-# LANGUAGE OverloadedStrings #-}

module Partners where

import           System.FilePath ((</>))
import           Control.Monad (when)
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B
import qualified Data.ByteString.UTF8 as BU
import qualified Data.ByteString.Lazy as LB
import qualified Data.Map as M
import           Data.List
import           Control.Monad (forM_)
import           Database.Redis
import qualified Data.Aeson as Aeson

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


processPartners partnerIx partnerMap
  = case M.lookup "name" partnerMap of
    Nothing -> do
      B.putStrLn $ B.concat ["!!! Empty name in:\n\t", showMap partnerMap]
      return partnerIx

    Just pName -> do
      let partnerAndService = mapSnd (:[]) $ extract partnerMap
            ["serviceRu", "serviceEn"
            ,"code","price"
            ,"priority1", "priority2"
            ,"comment", "status"]
      case M.lookup pName partnerIx of
        Nothing  -> return $! M.insert pName partnerAndService partnerIx
        Just pns -> do
          let res = joinPartners partnerAndService pns
          when (not . null $ jWarnings res)
            $ B.putStrLn $ B.unlines 
              $ (B.concat ["!!! When joining services for ", pName])
              : jWarnings res
          return $! M.insert pName
            (jPartner res, jServices res)
             partnerIx


data JoinResult = JoinResult
  {jPartner  :: M.Map ByteString ByteString
  ,jServices :: [M.Map ByteString ByteString]
  ,jWarnings :: [ByteString]
  }

joinPartners (m1,svc1) (m2,svc2) = JoinResult 
  {jPartner  = M.fromList res
  ,jServices = svc1 ++ svc2
  ,jWarnings = errs
  }
  where
    (errs, res) = foldl' f ([], []) allProps
    allProps = nub $ M.keys m1 ++ M.keys m2
    f (err,res) p = case (M.lookup p m1, M.lookup p m2) of
      (Just v1, Just v2)
        | v1 == v2  -> (err, (p,v1):res)
        | otherwise -> 
           let msg = B.concat ["\t", v1, " =/= ", v2]
           in  (msg:err, (p,v1):res)
      (Just v1, _)  -> (err, (p,v1):res)
      (_, Just v2)  -> (err, (p,v2):res)
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
