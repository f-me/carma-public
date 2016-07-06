CREATE TABLE "TechType"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  , isActive bool not null default 't'
  );

COPY "TechType" (id, label, isActive) FROM stdin;
1	Доставка топлива	t
2	Доставка масла	t
3	Замена колеса	t
4	Подзарядка АКБ	t
5	Ремонт стартера	t
6	Ремонт кондиционера	t
7	Отключение сигнализации	t
8	Слив топлива	t
9	Вскрытие автомобиля	t
10	Снятие с паркинга	t
11	Возвращение на дорогу	t
12	Замена дворников	t
13	Замена лампочек	t
14	Ремонт электрооборудования	t
15	Разморозка	t
\.

GRANT ALL ON "TechType" TO carma_db_sync;
GRANT ALL ON "TechType_id_seq" TO carma_db_sync;

SELECT setval(pg_get_serial_sequence('"TechType"', 'id'), max(id)) from "TechType";
