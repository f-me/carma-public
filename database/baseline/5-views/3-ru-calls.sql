CREATE VIEW "Звонки" AS

SELECT
calltbl.id,
timezone('Europe/Moscow'::text, calldate) AS "Дата и время начала звонка" ,
timezone('Europe/Moscow'::text, enddate) AS "Дата и время окончаня звонка" ,
usermetatbl.login AS "Оператор, принимающий звонок",
usermetatbl.realname as "Имя оператора,принимающего звонок",
calltbl.customerComment AS "Комментарий",
callerName AS "Звонящий",
calltbl.callerPhone AS "Контактный телефон звонящего",
"CallerType".label as "Кто звонит",
"CallType".label AS "Тип обращение",
calltbl.coords AS "Координаты",
address AS "Адрес",
--programm,
"Program".label AS "Программа",
--subprogram,
calltbl.caseid AS "Номер кейса"
FROM
calltbl
LEFT JOIN "CallerType" ON calltbl.callertype = "CallerType".id
LEFT JOIN "CallType" ON calltbl.calltype = "CallType".id
LEFT JOIN "Program" ON calltbl.program = "Program".id
LEFT JOIN usermetatbl ON calltbl.calltaker = usermetatbl.id
ORDER BY calldate ASC;

GRANT SELECT ON "Звонки" TO reportgen;
GRANT ALL ON "Звонки" TO analyst;
