BEGIN;

DROP VIEW IF EXISTS "SMS";

CREATE VIEW "SMS" AS
  SELECT s.ctime::DATE                 AS "Дата Отправки"
       , s.ctime::TIME                 AS "Время Отправки"
       , GetSmsStatusLabel(s.status)   AS "Статус"
       , s.phone                       AS "Номер телефона"
       , s.sender                      AS "Отправитель"
       , t.label                       AS "Шаблон"
       , s.msgtext                     AS "Текст"
       , COALESCE(s.caseref::TEXT, '') AS "Номер Кейса"
       , u.login                       AS "Пользователь, отправивший СМС"

  FROM "Sms" s
    LEFT JOIN "SmsTemplate" t ON s.template = t.id

    LEFT JOIN "Event" e
           ON s.id = e.modelid
          AND e.modelname = 'Sms'
          AND e.type = 'Create'

    LEFT JOIN usermetatbl u ON e.userid = u.id;

GRANT SELECT ON TABLE "SMS" TO reportgen;

COMMIT;
