CREATE TABLE "ContractCheckStatus"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "ContractCheckStatus" (id, label) FROM stdin;
1	VIN проверен по базе
2	VIN проверен у дилера
3	VIN проверен в FDDS
4	VIN проверен по сервисной книжке
5	По согласованию заказчика
6	VIN не найден
7	VIN просрочен
8	Карта участника найдена
9	Карта участника не найдена
\.

GRANT ALL ON "ContractCheckStatus" TO carma_db_sync;
GRANT ALL ON "ContractCheckStatus" TO carma_search;
GRANT ALL ON "ContractCheckStatus_id_seq" TO carma_db_sync;
GRANT ALL ON "ContractCheckStatus_id_seq" TO carma_search;

SELECT setval(pg_get_serial_sequence('"ContractCheckStatus"', 'id'), max(id)) from "ContractCheckStatus";
