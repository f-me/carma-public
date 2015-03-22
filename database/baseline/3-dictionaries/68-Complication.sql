CREATE TABLE "Complication"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "Complication" (id, label) FROM stdin;
1	Сложность идентификации
2	Конференция с ДЦ
3	Определение местоположения
4	Долгий поиск партнёра (сложность погрузки)
5	Долгий поиск партнёра (обращение в региональный отдел)
6	Сложность с клиентом
7	Долгое согласование заявки (> 3 минут)
8	Консультация механика (> 5 минут)
9	Вскрытие автомобиля
10	Другое (указать в комментарии)
\.

GRANT ALL ON "Complication" TO carma_db_sync;
GRANT ALL ON "Complication" TO carma_search;
GRANT ALL ON "Complication_id_seq" TO carma_db_sync;
GRANT ALL ON "Complication_id_seq" TO carma_search;

SELECT setval(pg_get_serial_sequence('"Complication"', 'id'), max(id)) from "Complication";
