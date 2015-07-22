DROP VIEW IF EXISTS "Услуги с приоритетами";
CREATE VIEW "Услуги с приоритетами" AS
--ТАРИФНЫЕ ОПЦИИ ИЗ УСЛУГ ПАРТНЕРОВ
WITH tfs AS
(
--УСЛУГИ, ОКАЗЫВАЕМЫЕ ПАРТНЕРАМИ
WITH
ps AS
(SELECT partnertbl.id,
partnertbl.name,
partnertbl.city,
SPLIT_PART(regexp_split_to_table(partnertbl.services, ','), 'PartnerService:', 2) AS service
from partnertbl
)

SELECT
ps.id as partnerid,
ps.name,
ps.city,
ps.service,
"PartnerService".id as partnerserviceid,
"PartnerService".parentid,
"PartnerService".priority1,
"PartnerService".priority2,
"PartnerService".priority3,
"PartnerService".falsecallpercent,
"PartnerService".servicename,
"ServiceType".label AS servicelabel,
SPLIT_PART(regexp_split_to_table("PartnerService".tarifoptions, ','), 'tarifOption:', 2) AS tarifoption
-- partner_servicetbl.tarifoptions
FROM ps
LEFT JOIN "PartnerService" ON ps.service = "PartnerService".id::TEXT
LEFT JOIN "ServiceType" ON "PartnerService".servicename = "ServiceType".id
)

SELECT
tfs.partnerid AS "Номер партнера",
tfs.name AS "Название партнера",
"City".label AS "Город",
tfs.priority1 AS "Приоритет за нал",
tfs.priority2 AS "Приоритет по безналу город",
tfs.priority3 AS "Приоритет по безналу за город",
tfs.falsecallpercent AS "Процент за ложный вызов",
tfs.servicelabel AS "Услуга",
tarifoptiontbl.optionname AS "Тарифная опция",
price1 AS "Стоимость за единицу за нал",
price2 AS "Стоимость за единицу по безналу",
tfs.partnerserviceid AS "partner_servicetbl.id",
tfs.tarifoption AS "tarifoption.id"
FROM
tfs
LEFT JOIN tarifoptiontbl ON  tfs.tarifoption::TEXT = tarifoptiontbl.id::TEXT
LEFT JOIN "City" ON tfs.city = "City".id
ORDER BY tfs.name;

GRANT SELECT ON "Услуги с приоритетами" TO reportgen;
