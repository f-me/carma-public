CREATE TABLE "Program"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL
  , client text
  , clientAddress text
  , clientCode text
  );

COPY "Program" (id, label) FROM stdin;
1	PSA
2	ВТБ24 /B2C
3	VW Гарантия Мобильности /B2B
4	GM Assistance /B2B
5	Chevrolet Korea
6	Ford - помощь на дорогах /B2B
7	Мапфре /B2B
8	KIA /B2B
9	Европлан /B2B
10	Ковидиен /B2B
11	Атлант-М /B2B
12	Вектор Лизинг /B2B
13	Нота Банк /B2C
14	Друг Компании /B2C
15	B2C карты
16	РУС-ЛАН /B2B
17	Независимость BMW /B2B
18	АВИЛОН VW /B2B
19	VW Центр Внуково /B2B
20	Ирито
21	Aston Martin
22	Bentley
23	Ночной Сервис Европкар/B2B
24	AIG Надежный патруль /B2B
25	Самара-Ассистанс /B2B
26	Лада Центр Белгород /B2B
27	RTR Hyundai /B2B
28	DAF NTS /B2B
29	ИП Трубкин /B2B
30	ARC B2B (Заявки от европейских клубов)
31	Интач B2B
32	Дженсер
33	Autoclub Europlan
34	Цезарь Сателлит (заявки от сотрудников) /B2B
35	3S-Telematica (заявки от сотрудников)/B2B
36	Авто-Цель (заявки от сотрудников) /B2B
37	АРВАЛ (заявки от сотрудников) /B2B
38	Автоимпорт (заявки от сотрудников) /B2B
39	ТДВ-Авто (заявки от сотрудников) /B2B
40	Блок Центр (заявки от сотрудников) /B2B
41	Автопрестус (заявки от сотрудников) /B2B
42	Дженсер Ясенево (заявки от сотрудников) /B2B
43	Интач-помощь на дорогах /B2C
44	ВТБ24 Автокарта/B2C
45	Петрокоммерц-Лукойл-Мастеркард /B2C
46	Адвокард Драйвер Голд /B2C
47	Юникредит/B2C
48	АК Барс Банк /B2C
49	Кузьмиха Помощь на дорогах /B2C
50	Castrol Помощь на дорогах /B2C
51	ПАРИ /B2C
52	Росавтобанк /B2C
53	Цюрих /B2C
54	Петрол /B2C
55	БАТ /B2C
56	МотоПипл /B2C
57	Эрго Русь /B2C
58	АГ ассистанс /B2C
59	Шелл /B2C
60	Уралсиб /B2C
61	BP /B2B
62	Сотрудник РАМК
63	Заказ билетов
64	РАМК
65	Тестовая программа
\.

GRANT ALL ON "Program" TO carma_db_sync;
GRANT ALL ON "Program" TO carma_search;
GRANT ALL ON "Program_id_seq" TO carma_db_sync;
GRANT ALL ON "Program_id_seq" TO carma_search;
