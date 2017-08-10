DROP VIEW IF EXISTS "Контракты";
CREATE VIEW "Контракты" AS
SELECT "Contract".id AS "Номер контракта",
--ФУНКЦИИ TIME (ВРЕМЯ ПО МОСКВЕ) НЕ РЕАЛИЗУЕТС ВО VIEW, БУДЕТ РЕАЛИЗОВАНА В JASPERSOFT STUDIO (формат  'DD/MM/YYYY HH24:MI')
    "Contract".ctime AT TIME ZONE 'Europe/Moscow' AS "Время создания контракта",
        CASE "Contract".isactive
            WHEN true THEN '+'::text
            ELSE '-'::text
        END AS "Активен",
    "Contract".name AS "ФИО клиента",
    "Contract".email AS "E-mail клиента",
    "Contract".vin AS "VIN",
    "Contract".cardnumber AS "Номер карты",
    "Contract".codeword AS "Кодовое слово",
    "Contract".phone AS "Номер телефона",
    "Contract".platenum AS "Госномер",
    "Contract".validsince AS "Дата регистрации в программе",
    "Contract".validuntil AS "Программа действует до (Дата)",
    "Contract".startmileage AS "Пробег при регистрации в программ",
    "CarMake".label AS "Марка",
    "CarModel".label AS "Модель",
    "Contract".makeyear::Integer AS "Год производства автомобиля",
    "CarClass".label AS "Класс автомобиля",
    "Contract".color AS "Цвет",
    "Transmission".label AS "Коробка передач",
    "Contract".enginevolume AS "Объём двигателя",
    "Engine".label AS "Тип двигателя",
    "Contract".buydate AS "Дата покупки",
    "Contract".firstSaleDate AS "Дата первой продажи",
    p2.name AS "Дилер, продавший автомобиль",
    p1.name AS "Дилер, у которого проходило послед",
    "Contract".checkperiod AS "Межсервисный интервал",
    "CheckType".label AS "Вид ТО",
    "Contract".ordernumber AS "Номер заказ-наряда",
    "Contract".managername AS "ФИО менеджера",
    "Contract".comment AS "Комментарий",
    "ProgramType".label AS "Тип программы",
    "Program".label AS "Программа",
    "SubProgram".label AS "Подпрограмма",
    "LegalForm".label AS "Физическое/юридическое лицо",
    usermetatbl.realname AS "Пользователь, внёсший данные",
        CASE "Contract".dixi
            WHEN true THEN '+'::text
            ELSE '-'::text
        END AS "Заполнен"
   FROM "Contract"
   LEFT JOIN "CarClass" ON "Contract".carclass = "CarClass".id
   LEFT JOIN "CheckType" ON "Contract".checktype = "CheckType".id
   LEFT JOIN usermetatbl ON "Contract".committer = usermetatbl.id
   LEFT JOIN "Engine" ON "Contract".enginetype = "Engine".id
   LEFT JOIN partnertbl p1 ON "Contract".lastcheckdealer = p1.id
   LEFT JOIN "LegalForm" ON "Contract".legalform = "LegalForm".id
   LEFT JOIN "CarMake" ON "Contract".make = "CarMake".id
   LEFT JOIN "CarModel" ON "Contract".model = "CarModel".id
   LEFT JOIN partnertbl p2 ON "Contract".seller = p2.id
   LEFT JOIN "SubProgram" ON "Contract".subprogram = "SubProgram".id
   LEFT JOIN "Program" ON "SubProgram".parent = "Program".id
   LEFT JOIN "ProgramType" ON "Program".ptype = "ProgramType".id
   LEFT JOIN "Transmission" ON "Contract".transmission = "Transmission".id

   ORDER BY ctime;

GRANT SELECT ON "Контракты" TO reportgen;
