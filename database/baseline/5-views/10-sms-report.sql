CREATE VIEW "SMS" AS
        SELECT
                s.ctime::date as "Дата Отправки",
                s.ctime::time as "Время Отправки",
                CASE
                        WHEN s.status = 'sent'::text THEN 'Отправлено'::text
                        WHEN s.status = 'please-send'::text THEN 'В очереди на отправку'::text
                        WHEN s.status = 'draft'::text THEN 'Черновик'::text
                        WHEN s.status = 'error'::text THEN 'Ошибка'::text
                        WHEN s.status = 'processing'::text THEN 'Отправляется'::text
                END AS "Статус",
                s.phone as "Номер телефона",
                s.sender as "Отправитель",
                t.label as "Шаблон",
                s.msgtext as "Текст",
                s.caseref as "Номер Кейса",
                u.login as "Пользователь, отправивший СМС"
        FROM "Sms" s
                LEFT JOIN "SmsTemplate" t ON s.template = t.id
                LEFT JOIN "Event" e ON s.id = e.modelid AND e.modelname = 'Sms' AND e.type = 'Create'
                LEFT JOIN usermetatbl u on e.userid = u.id;

GRANT SELECT ON TABLE "SMS" TO reportgen;
