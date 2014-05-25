CREATE VIEW "Партнёры" AS
Select
--ФУНКЦИЯ YESNO
        (CASE
                WHEN
                        partnertbl.isActive = true
                THEN
                        '+'
                ELSE
                        '-'
                END)
        AS "Патрнер активен",
--ФУНКЦИЯ YESNO
        (CASE
                WHEN
                        partnertbl.isDealer = true
                THEN
                        '+'
                ELSE
                        '-'
                END)
        AS "Дилер",
--ФУНКЦИЯ YESNO
        (CASE
                WHEN
                        partnertbl.isMobile = true
                THEN
                        '+'
                ELSE
                        '-'
                END)
        AS "Мобильный партнёр",
partnertbl.name AS "Название партнёра",
partnertbl.code AS "Код",
--LOOKUP(DealerCities, partnertbl.city) вместо dictionaries/DealerCities.json
        "City".label AS "Город",
makes AS "Обслуживаемые марки",
partnertbl.personInCharge AS "Ответственное лицо",
--partnertbl.taxScheme AS "Форма налогообложения",
        "TaxScheme".label AS "Форма налогообложения",
--ФУНКЦИЯ YESNO
        (CASE
                WHEN
                        partnertbl.isPayBackConfirmed = true
                THEN
                        '+'
                ELSE
                        '-'
                END)
        AS "Соглашение о вознаграждении",
partnertbl.comment AS "Комментарий",
--Далее идут поля, которых нету в шаблоне отчета по партнерам:
partnertbl.services AS "Услуги",
--ЧТО ЭТО?
        --partnertbl.garbage,
coords AS "Координаты",
--ЧТО ЭТО? Чем отличается от makes?
        --makers, --СТАРОЕ ПОЛЕ
--ЧТО ЭТО?
        --mtime, -- время последнего обновления данных о партнёре через партнёрское приложение
--JSON
addrs AS "Адреса",
--JSON
phones AS "Телефоны",
emails AS "Электронная почта",
--ЧТО ЭТО?
        --isfree, -- свободен ли партнёр (используется для мобильных партнёров, у них кнопочка в приложении есть)
foreignident AS "Интеграционный код",
synonyms AS "Синонимы"
FROM partnertbl
LEFT JOIN "City" ON partnertbl.city = "City".value
LEFT JOIN "TaxScheme" ON partnertbl.taxScheme::Integer = "TaxScheme".id;

GRANT SELECT ON "Партнёры" TO reportgen;
GRANT ALL ON "Партнёры" TO analyst;
