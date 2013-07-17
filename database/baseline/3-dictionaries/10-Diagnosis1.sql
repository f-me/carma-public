
CREATE TABLE "Diagnosis1" (
    id SERIAL PRIMARY KEY,
    value text UNIQUE NOT NULL,
    label text UNIQUE NOT NULL
);

COPY "Diagnosis1" (id, value, label) FROM stdin;
1	diag1.1	Безопасности
2	diag1.2	Впускная
3	diag1.3	Выпуска отработавших газов
4	diag1.4	Двигатель
5	diag1.5	Кузов
6	diag1.6	Осветительные приборы
7	diag1.7	Охлаждение / подогрева воздуха салона
8	diag1.8	Охлаждения двигателя
9	diag1.9	Подвеска
10	diag1.10	Привод вспомогательных агрегатов
11	diag1.11	Рулевого управления
12	diag1.12	Система блокировки и сигнализация
13	diag1.13	Система зажигания
14	diag1.14	Топливная
15	diag1.15	Тормозная
16	diag1.16	Трансмиссия
17	diag1.17	Электрооборудование
\.

GRANT SELECT ON TABLE "Diagnosis1" TO carma_search;


