CREATE TABLE "TechType"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "TechType" (id, label) FROM stdin;
1	Доставка топлива
2	Доставка масла
3	Замена колеса
4	Подзарядка АКБ
5	Ремонт стартера
6	Ремонт кондиционера
7	Отключение сигнализации
8	Слив топлива
9	Вскрытие автомобиля
10	Снятие с паркинга
11	Возвращение на дорогу
12	Замена дворников
13	Замена лампочек
14	Ремонт электрооборудования
15	Разморозка
\.

GRANT ALL ON "TechType" TO carma_db_sync;
GRANT ALL ON "TechType_id_seq" TO carma_db_sync;

SELECT setval(pg_get_serial_sequence('"TechType"', 'id'), max(id)) from "TechType";
