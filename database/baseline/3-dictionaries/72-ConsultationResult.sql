CREATE TABLE "ConsultationResult"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "ConsultationResult" (id, label) FROM stdin;
1	Рекомендована Эвакуация
2	Рекомендована Техпомощь
3	Рекомендовано Самостоятельно двигаться в ДЦ
4	Консультация помогла без заказа услуги
5	Техническая консультация
6	Консультация не потребовалась
\.

GRANT ALL ON "ConsultationResult" TO carma_db_sync;
GRANT ALL ON "ConsultationResult_id_seq" TO carma_db_sync;

SELECT setval(pg_get_serial_sequence('"ConsultationResult"', 'id'), max(id)) from "ConsultationResult";
