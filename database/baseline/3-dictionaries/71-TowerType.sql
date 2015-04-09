CREATE TABLE "TowerType"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "TowerType" (id, label) FROM stdin;
1	Эвакуатор
2	Манипулятор
3	Подкатные тележки
4	Грузовой эвакуатор
5	Длиннобазный автомобиль
\.

GRANT ALL ON "TowerType" TO carma_db_sync;
GRANT ALL ON "TowerType" TO carma_search;
GRANT ALL ON "TowerType_id_seq" TO carma_db_sync;
GRANT ALL ON "TowerType_id_seq" TO carma_search;

SELECT setval(pg_get_serial_sequence('"TowerType"', 'id'), max(id)) from "TowerType";
