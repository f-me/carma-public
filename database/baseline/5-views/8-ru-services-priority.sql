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
SPLIT_PART(regexp_split_to_table(partnertbl.services, ','), 'partner_service:', 2) AS service
from partnertbl
)

SELECT
ps.id as partnerid,
ps.name,
ps.city,
ps.service,
partner_servicetbl.id as partnerserviceid,
partner_servicetbl.parentid,
partner_servicetbl.priority1,
partner_servicetbl.priority2,
partner_servicetbl.priority3,
partner_servicetbl.falsecallpercent,
partner_servicetbl.servicename,
"ServiceType".label AS servicelabel,
SPLIT_PART(regexp_split_to_table(partner_servicetbl.tarifoptions, ','), 'tarifOption:', 2) AS tarifoption
--partner_servicetbl.tarifoptions
FROM ps
LEFT JOIN partner_servicetbl ON ps.service = partner_servicetbl.id::TEXT
LEFT JOIN "ServiceType" ON partner_servicetbl.servicename = "ServiceType".id
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
tfs.tarifoption "tarifoption.id"
FROM
tfs
LEFT JOIN tarifoptiontbl ON  tfs.tarifoption::TEXT = tarifoptiontbl.id::TEXT
LEFT JOIN "City" ON tfs.city::TEXT = "City".value
ORDER BY tfs.name;

GRANT SELECT ON "Услуги с приоритетами" TO reportgen;
GRANT ALL ON "Услуги с приоритетами" TO analyst;
