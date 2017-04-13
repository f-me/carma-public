CREATE TABLE "TowSort"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "TowSort" (id, label) FROM stdin;
1	Эвакуатор
2	Манипулятор
3	Эвакуатор с подкатными тележками
4	Грузовой эвакуатор
5	Эвакуатор с длинной платформой
6	Эвакуатор с крытым тентом
7	Эвакуатор с буксировочными крюками
\.

GRANT ALL ON "TowSort" TO carma_db_sync;
GRANT ALL ON "TowSort_id_seq" TO carma_db_sync;

SELECT setval(pg_get_serial_sequence('"TowSort"', 'id'), max(id)) from "TowSort";
