CREATE VIEW "Звонки" AS
--ОПРЕДЕЛЕНИЕ УНИКАЛЬНЫХ ЗАПИСЕЙ В "CallType"
        WITH calltypes AS
        (SELECT value, label FROM "CallType" GROUP BY value, label)

SELECT
calltbl.id,
timezone('Europe/Moscow'::text, calldate) AS "Дата и время начала звонка" ,
timezone('Europe/Moscow'::text, enddate) AS "Дата и время окончаня звонка" ,
usermetatbl.login AS "Оператор, принимающий звонок",
usermetatbl.realname as "Имя оператора,принимающего звонок",
"Wazzup".label AS "Что случилось",
calltbl.customerComment AS "Комментарий",
callername_name AS "Звонящий",
concat_ws(', '::text, calltbl.callername_phone1, calltbl.callername_phone2, calltbl.callername_phone3, calltbl.callername_phone4) AS "Контактные телефоны звонящего",
callername_email  AS "Email звонящего",
CASE
        WHEN
                callername_contactowner = true
        THEN
                'Y'
        ELSE
                'N'
        END	AS "Звонящий владелец?",-- boolean,
callername_ownername  AS "Владелец",
concat_ws(', '::text, callername_ownerphone1, callername_ownerphone2, callername_ownerphone3, callername_ownerphone4)  AS "Контактные телефоны владельца",
callername_owneremail  AS "Email владельца",
        --callertype "Кто звонит",
"CallerType".label as "Кто звонит",
        --city,
"City".label as "Город",
--calltype AS "Тип звонка",
        calltypes.label AS "Тип звонка",
"CarMake".label AS "Марка",
"CarModel".label AS "Модель",
coords AS "Координаты",
address AS "Адрес",
--programm,
"Program".label AS "Программа",
--subprogram,
"SubProgram".label AS "Подпрограмма"
FROM
calltbl
LEFT JOIN "Wazzup" ON calltbl.wazzup = "Wazzup".id
LEFT JOIN "CallerType" ON calltbl.callertype = "CallerType".value
LEFT JOIN "City" ON calltbl.city = "City".value
--LEFT JOIN "CallType" ON calltbl.calltype = "CallType".value AND calltbl.callertype = "CallerType".value --НЕ У ВСЕХ ЗВОНКОВ УСТАНОВЛЕН CALLERTYPE
LEFT JOIN calltypes ON calltbl.calltype = calltypes.value
LEFT JOIN "Program" ON calltbl.program = "Program".id
LEFT JOIN "SubProgram" ON calltbl.subprogram = "SubProgram".id
LEFT JOIN usermetatbl ON calltbl.calltaker = usermetatbl.id
LEFT JOIN "CarMake" ON calltbl.carmake = "CarMake".id
LEFT JOIN "CarModel" ON calltbl.carmodel = "CarModel".id
ORDER BY calldate ASC;

GRANT SELECT ON "Звонки" TO reportgen;
GRANT ALL ON "Звонки" TO analyst;
