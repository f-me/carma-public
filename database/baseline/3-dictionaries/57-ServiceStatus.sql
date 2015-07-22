CREATE TABLE "ServiceStatus"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (label <> '')
  );

COPY "ServiceStatus" (id, label) FROM stdin;
1	В Back Office
2	Создание
3	Время выезда
4	Отказ от услуги
5	Требуется конференция с механиком
6	Требуется конференция с дилером
7	Клиент просит уточнить время оказания услуги
8	Проверка условий
9	Ошибка оператора
10	Согласование с дилером
11	Согласование с производителем
12	Заказ услуги
13	Требуется партнёр
15	Услуга заказана
16	Оказание услуги задерживается
17	Услуга оказывается
18	Ложный вызов
19	Услуга оказана
20	Услуга закрыта
\.

GRANT ALL ON "ServiceStatus" TO carma_db_sync;
GRANT ALL ON "ServiceStatus_id_seq" TO carma_db_sync;

SELECT setval(pg_get_serial_sequence('"ServiceStatus"', 'id'), max(id)) from "ServiceStatus";
