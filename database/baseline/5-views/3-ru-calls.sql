DROP VIEW "Звонки";

CREATE VIEW "Звонки" AS

SELECT
calltbl.id,
timezone('Europe/Moscow'::text, calldate) AS "Дата и время начала звонка" ,
timezone('Europe/Moscow'::text, enddate)  AS "Дата и время окончания звонка" ,
usermetatbl.login       AS "Оператор, принимающий звонок",
usermetatbl.realname    as "Имя оператора,принимающего звонок",
calltbl.customerComment AS "Комментарий",
callerName              AS "Звонящий",
calltbl.callerPhone     AS "Контактный телефон звонящего",
"CallerType".label      as "Кто звонит",
"CallType".label        AS "Тип обращения",
"CallReason".label      AS "Причина обращения",
calltbl.coords          AS "Координаты",
address                 AS "Адрес",
"Program".label         AS "Программа",
calltbl.caseid          AS "Номер кейса",
"AbuseTarget".label     AS "На кого поступила жалоба",
partnertbl.name         AS "Дилер"

FROM
calltbl
LEFT JOIN "CallerType"  ON calltbl.callertype = "CallerType".id
LEFT JOIN "CallType"    ON calltbl.calltype = "CallType".id
LEFT JOIN "CallReason"  ON calltbl.callreason = "CallReason".id
LEFT JOIN "Program"     ON calltbl.program = "Program".id
LEFT JOIN usermetatbl   ON calltbl.calltaker = usermetatbl.id
LEFT JOIN partnertbl    ON calltbl.partner = partnertbl.id
LEFT JOIN "AbuseTarget" ON calltbl.abuseTarget = "AbuseTarget".id
ORDER BY calldate ASC;

GRANT SELECT ON TABLE "Звонки" TO reportgen;
