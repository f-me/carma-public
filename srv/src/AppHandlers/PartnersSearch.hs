{-# LANGUAGE QuasiQuotes #-}
module AppHandlers.PartnersSearch (partnersSearchH) where

import Data.String (fromString)

import Database.PostgreSQL.Simple.SqlQQ
import Database.PostgreSQL.Simple

import Application
import AppHandlers.Util
import Utils.HttpErrors

q :: String
q = [sql|
     SELECT p.id::text
          , p.name
          , p.city
          , p.comment
          , p.addrDeFacto
          , p.phone1
          , p.workingTime
          , (p.isDealer::int)::text
          , (p.isMobile::int)::text
          , s.priority2
          , s.priority3
          , s.servicename
     FROM partnertbl p
     INNER JOIN partner_servicetbl s
     ON p.id = cast(split_part(s.parentid, ':', 2) as integer)
     AND s.parentid is not null
     AND s.parentid != ''
     AND s.parentid != 'partner:null'
     WHERE true
|]

partnersSearchH :: AppHandler ()
partnersSearchH = do
  rows <- withPG pg_search $ \c -> query_ c $ fromString q
  let fields = ["id","name","city","comment" ,"addrDeFacto"
               ,"phone1","workingTime","isDealer","isMobile"
               ,"priority2", "priority3", "serviceName"
               ]
  writeJSON $ mkMap fields rows
