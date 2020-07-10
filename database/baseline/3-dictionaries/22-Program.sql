CREATE TABLE "Program"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL CHECK (trim(label) <> '')
  , shortLabel text CHECK (trim(shortLabel) <> '')
  , logo text CHECK (trim(logo) <> '')
  , client text
  , clientAddress text
  , clientCode text
  , fdds text
  , managers int4[] -- ELEMENT REFERENCES Usermeta
  , pType int4 REFERENCES "ProgramType"
  , help text
  );

COPY "Program" (id, label, pType) FROM stdin;
1	Peugeot	\N
2	ВТБ24 /B2C	2
3	VW Гарантия Мобильности /B2B	1
4	GM Assistance /B2B	1
5	Аларм Ассистанс	\N
6	Ford - помощь на дорогах /B2B	1
7	Мапфре /B2B	1
8	KIA /B2B	1
9	Европлан /B2B	1
10	Ковидиен /B2B	1
11	Атлант-М /B2B	1
12	Вектор Лизинг /B2B	1
13	Нота Банк /B2C	2
14	Друг Компании /B2C	2
15	B2C карты	2
16	РУС-ЛАН /B2B	1
17	Независимость BMW /B2B	1
18	АВИЛОН VW /B2B	1
19	VW Центр Внуково /B2B	1
20	Ирито	\N
21	Aston Martin	\N
22	Bentley	\N
23	Ночной Сервис Европкар/B2B	1
24	AIG Надежный патруль /B2B	1
25	Самара-Ассистанс /B2B	1
26	Лада Центр Белгород /B2B	1
27	RTR Hyundai /B2B	1
28	DAF NTS /B2B	1
29	ИП Трубкин /B2B	1
30	ARC B2B (Заявки от европейских клубов)	1
31	Интач B2B	1
32	Дженсер	\N
33	Autoclub Europlan	\N
34	Цезарь Сателлит (заявки от сотрудников) /B2B	1
35	3S-Telematica (заявки от сотрудников)/B2B	1
36	Авто-Цель (заявки от сотрудников) /B2B	1
37	АРВАЛ (заявки от сотрудников) /B2B	1
38	Автоимпорт (заявки от сотрудников) /B2B	1
39	ТДВ-Авто (заявки от сотрудников) /B2B	1
40	Блок Центр (заявки от сотрудников) /B2B	1
41	Автопрестус (заявки от сотрудников) /B2B	1
42	Дженсер Ясенево (заявки от сотрудников) /B2B	1
43	Интач-помощь на дорогах /B2C	2
44	ВТБ24 Автокарта/B2C	2
45	Петрокоммерц-Лукойл-Мастеркард /B2C	2
46	Адвокард Драйвер Голд /B2C	2
47	Юникредит/B2C	2
48	АК Барс Банк /B2C	2
49	Кузьмиха Помощь на дорогах /B2C	2
50	Castrol Помощь на дорогах /B2C	2
51	ПАРИ /B2C	2
52	Росавтобанк /B2C	2
53	Цюрих /B2C	2
54	Петрол /B2C	2
55	БАТ /B2C	2
56	МотоПипл /B2C	2
57	Эрго Русь /B2C	2
58	АГ ассистанс /B2C	2
59	Шелл /B2C	2
60	Уралсиб /B2C	2
61	BP /B2B	1
62	Сотрудник РАМК	\N
63	Заказ билетов	\N
64	РАМК	\N
65	Тестовая программа	\N
66	Аренда-Авто Ассистанс /B2B	1
67	Citroen	\N
68	МариАвто Ассистанс	\N
\.

GRANT ALL ON "Program" TO carma_db_sync;
GRANT ALL ON "Program_id_seq" TO carma_db_sync;

GRANT SELECT ON "Program" TO reportgen;

SELECT setval(pg_get_serial_sequence('"Program"', 'id'), max(id)) from "Program";
