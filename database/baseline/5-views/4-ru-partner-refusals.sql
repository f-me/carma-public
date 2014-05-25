CREATE VIEW "Отказы партнёров" AS
--ОБЪЕДИНЕНИЕ ГОРОДОВ ИЗ "Region" В ОДНУ СТРОКУ ДЛЯ ИЗБЕЖАНИЯ ЗАДВОЕНИЯ СТРОК В ОТЧЕТЕ
WITH cities_regions as (WITH cit AS (SELECT label, unnest(cities) as cities FROM "Region"  )
SELECT array_to_string(array_agg(label), ',') as regionlist, cities FROM cit GROUP BY cities ORDER BY cities)


SELECT
partnercanceltbl.id,
--ФУНКЦИИ TIME (ВРЕМЯ ПО МОСКВЕ) НЕ РЕАЛИЗУЕТС ВО VIEW, БУДЕТ РЕАЛИЗОВАНА В JASPERSOFT STUDIO (формат DD.MM.YYYY HH24:MI)
        ctime AT TIME ZONE 'Europe/Moscow' as "Время и дата",
casetbl.id AS "Номер кейса",
partnertbl.name as "Партнёр",
--СПРАВОЧНИКА dictionaries/PartnerCancelReason.json --РЕАЛИЗОВАН, КАК UNBOUNDED
        partnercanceltbl.partnercancelreason  AS "Причина отказа",
--ЗАЧЕМ ДЖОЙН С РЕГИОНОМ?
        --"Region" AS "Регион".label,
partnercanceltbl.comment as "Комментарий",
"City".label as "Город",
"ServiceNames".label AS "Услуга",
partnercanceltbl.owner AS "Оператор",
regionlist as "Регион"
from partnercanceltbl
LEFT JOIN casetbl ON split_part(partnercanceltbl.caseid, 'case:', 2)::Integer = casetbl.id
LEFT JOIN partnertbl ON partnertbl.id = "substring"(partnercanceltbl.partnerid, ':(.*)')::Integer
LEFT JOIN "City" ON casetbl.city = "City".value
--РЕАЛИЗАЦИЯ УНИКАЛЬНОЙ СВЯЗКИ В servicetbl(id, type):
        LEFT JOIN servicetbl ON
                ((split_part(partnercanceltbl.serviceid, ':', 2)::Integer = servicetbl.id)
                AND
                (split_part(partnercanceltbl.serviceid, ':', 1)::text = servicetbl.type))
LEFT JOIN "ServiceNames" ON servicetbl.type = "ServiceNames".value
--ДЖОЙНИМ РЕЗУЛЬТАТ ПОДЗАПРОСА cities_regions ДЛЯ ВЫБОРА РЕГИОНОВ
        LEFT JOIN cities_regions ON "City".id = cities_regions.cities::Integer
ORDER BY partnercanceltbl.ctime ASC, casetbl.id ASC;

GRANT SELECT ON "Отказы партнёров" TO reportgen;
GRANT ALL ON "Отказы партнёров" TO analyst;
