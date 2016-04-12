DROP VIEW IF EXISTS "Партнеры";

CREATE VIEW "Партнеры" AS
--ВЫБИРАЕМ НОРМАЛЬНЫЕ НАЗВАНИЯ УСЛУГ У ПАРТНЕРА
WITH servicelabel AS
(
  SELECT p.id AS id, string_agg(t.label, ', ') AS label
  FROM (SELECT DISTINCT id, json_array_elements(services)->>'type' AS svcid FROM partnertbl) p
    JOIN "ServiceType" t ON (p.svcid::int = t.id)
  GROUP BY p.id
),
makelabels AS
(
 SELECT MK.id, string_agg("CarMake".label, ', ') as makes
 FROM (SELECT id, unnest(makes) as mk FROM partnertbl) MK
 LEFT JOIN "CarMake" ON "CarMake".id = MK.mk
 GROUP BY MK.id
),
addrs AS
(
 SELECT DISTINCT id, addrs_value as text FROM
 (SELECT id,
         json_array_elements(addrs)->>'value' as addrs_value,
         json_array_elements(addrs)->>'key' as addrs_key
         FROM partnertbl) s WHERE addrs_key = 'fact'
),
phones AS
(
 SELECT id, string_agg(phones_value, ', ') as text FROM
 (SELECT id, json_array_elements(phones)->>'value' as phones_value
  FROM partnertbl) s GROUP BY id
),
emails AS
(
 SELECT id, string_agg(emails_value, ', ') as text FROM
 (SELECT id, json_array_elements(emails)->>'value' as emails_value
  FROM partnertbl) s GROUP BY id
)
SELECT
        (CASE WHEN partnertbl.isActive THEN '+' ELSE '-' END) AS "Партнер активен",
        (CASE WHEN partnertbl.isDealer THEN '+' ELSE '-' END) AS "Дилер",
        (CASE WHEN partnertbl.isMobile THEN '+' ELSE '-' END) AS "Мобильный партнёр",
        partnertbl.name AS "Название партнёра",
        partnertbl.code AS "Код",
        "City".label AS "Город",
        makelabels.makes AS "Обслуживаемые марки",
        partnertbl.personInCharge AS "Ответственное лицо",
        "TaxScheme".label AS "Форма налогообложения",
        (CASE WHEN partnertbl.isPayBackConfirmed THEN '+' ELSE '-' END) AS "Соглашение о вознаграждении",
        partnertbl.comment AS "Комментарий",
        --Далее идут поля, которых нету в шаблоне отчета по партнерам:
        servicelabel.label AS "Услуги",
        --ЧТО ЭТО?
                --partnertbl.garbage,
        --ЧТО ЭТО? Чем отличается от makes?
                --makers, --СТАРОЕ ПОЛЕ
        --ЧТО ЭТО?
                --mtime, -- время последнего обновления данных о партнёре через партнёрское приложение
        addrs.text AS "Адрес",
        phones.text AS "Телефоны",
        emails.text AS "Электронная почта",
        --ЧТО ЭТО?
                --isfree, -- свободен ли партнёр (используется для мобильных партнёров, у них кнопочка в приложении есть)
        foreignident AS "Интеграционный код",
        array_to_string(synonyms, ', ') AS "Синонимы"
FROM partnertbl
LEFT JOIN "City" ON partnertbl.city = "City".id
LEFT JOIN "TaxScheme" ON partnertbl.taxScheme::Integer = "TaxScheme".id
LEFT JOIN servicelabel ON partnertbl.id = servicelabel.id
LEFT JOIN makelabels ON partnertbl.id = makelabels.id
LEFT JOIN addrs ON partnertbl.id = addrs.id
LEFT JOIN phones ON partnertbl.id = phones.id
LEFT JOIN emails ON partnertbl.id = emails.id;

GRANT SELECT ON "Партнеры" TO reportgen;
