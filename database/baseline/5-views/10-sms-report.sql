BEGIN;

DROP VIEW IF EXISTS "SMS";

CREATE VIEW "SMS" AS
  SELECT
    s.ctime::DATE AS "Дата Отправки",
    s.ctime::TIME AS "Время Отправки",

    CASE
      WHEN s.status = 'sent'::TEXT        THEN 'Отправлено'::TEXT
      WHEN s.status = 'please-send'::TEXT THEN 'В очереди на отправку'::TEXT
      WHEN s.status = 'draft'::TEXT       THEN 'Черновик'::TEXT
      WHEN s.status = 'error'::TEXT       THEN 'Ошибка'::TEXT
      WHEN s.status = 'processing'::TEXT  THEN 'Отправляется'::TEXT
    END AS "Статус",

    s.phone                       AS "Номер телефона",
    s.sender                      AS "Отправитель",
    t.label                       AS "Шаблон",
    s.msgtext                     AS "Текст",
    COALESCE(s.caseref::TEXT, '') AS "Номер Кейса",
    u.login                       AS "Пользователь, отправивший СМС"

  FROM "Sms" s
    LEFT JOIN "SmsTemplate" t ON s.template = t.id

    LEFT JOIN "Event" e
           ON s.id = e.modelid
          AND e.modelname = 'Sms'
          AND e.type = 'Create'

    LEFT JOIN usermetatbl u ON e.userid = u.id;

GRANT SELECT ON TABLE "SMS" TO reportgen;

COMMIT;
