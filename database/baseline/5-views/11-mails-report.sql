CREATE VIEW "Письма" AS
        SELECT
                e.ctime::date as "Дата Создания",
                e.ctime::time as "Время Создания",
                e.mtime::date as "Дата Отправки/Последнего изменения",
                e.mtime::time as "Время Отправки/Последнего изменения",
                CASE
                        WHEN e.status = 'done'::text THEN 'Отправлено'::text
                        WHEN e.status = 'please-send'::text THEN 'В очереди на отправку'::text
                        WHEN e.status = 'error'::text THEN 'Ошибка'::text
                        WHEN e.status = 'processing'::text THEN 'Отправляется'::text
                END AS "Статус",
                concat (e.to, ', ', e.cc) as "Получатели",
                e.from as "Отправитель",
                e.reply as "Обратный адрес",
                e.body as "Текст письма",
                CASE
                        WHEN e.why ->> 'foo' = 'psa'::text THEN 'Письмо в PSA'::text
                        WHEN e.why ->> 'foo' = 'dealer'::text THEN 'Письмо дилеру PSA'::text
                        WHEN e.why ->> 'foo' = 'genser'::text THEN 'Письмо в Genser'::text
                END AS "Тип письма",
                replace (e.why ->> 'svc', 'Ident Service ','') AS "Номер услуги",
                s.parentid as "Номер кейса",
                st.label as "Тип услуги"
        FROM "Email" e
                LEFT JOIN servicetbl s ON replace (e.why ->> 'svc', 'Ident Service ','') = s.id::text
                LEFT JOIN "ServiceType" st ON s.type = st.id;


ALTER TABLE "Письма"
  OWNER TO carma;
GRANT ALL ON TABLE "Письма" TO carma;
GRANT ALL ON TABLE "Письма" TO fmuser;
GRANT SELECT ON TABLE "Письма" TO reportgen;
GRANT ALL ON TABLE "Письма" TO analyst;
GRANT ALL ON TABLE "Письма" TO carma_db_sync;
