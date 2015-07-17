CREATE TABLE "CaseStatus"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "CaseStatus" (id, label) FROM stdin;
1	Front Office
2	Требуется дополнительная информация
3	Back Office
4	Закрыт
5	Отмена
6	Заказ услуги через мобильное приложение
7	ДТП из мобильного приложения
\.

GRANT ALL ON "CaseStatus" TO carma_db_sync;
GRANT ALL ON "CaseStatus_id_seq" TO carma_db_sync;

SELECT setval(pg_get_serial_sequence('"CaseStatus"', 'id'), max(id)) from "CaseStatus";
