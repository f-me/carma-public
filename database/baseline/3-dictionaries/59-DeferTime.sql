CREATE TABLE "DeferTime"
  ( id    SERIAL PRIMARY KEY
  , "time" text UNIQUE NOT NULL
  , label text UNIQUE NOT NULL
  );

GRANT ALL ON "DeferTime" TO carma_db_sync;
GRANT ALL ON "DeferTime" TO carma_search;
GRANT ALL ON "DeferTime_id_seq" TO carma_db_sync;
GRANT ALL ON "DeferTime_id_seq" TO carma_search;

COPY "DeferTime" (time, label) FROM stdin;
00:10	10 минут
00:30	30 минут
1:00	1 час
2:00	2 часа
3:00	3 часа
6:00	6 часов
24:00	1 день
48:00	2 дня
120:00	5 дней
168:00	Неделя
\.

SELECT setval(pg_get_serial_sequence('"DeferTime"', 'id'), max(id)) from "DeferTime";
