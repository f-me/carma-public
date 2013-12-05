CREATE TABLE "SubProgram"
  ( id    SERIAL PRIMARY KEY
  , parent int4 REFERENCES "Program" ON DELETE SET NULL
  , label text NOT NULL
  , active bool NOT NULL DEFAULT true
  , value text UNIQUE NOT NULL
  , mailAddr text
  , mailPass text
  , contacts text
  , services int4[] DEFAULT array[]::int4[]
  , checkPeriod integer
  , validFor integer
  , contract text
  , logo text
  , help text
  , dealerHelp text
  );

CREATE TEMPORARY TABLE "SubProgram_tmp"
  ( id    SERIAL PRIMARY KEY
  , program text
  , label text
  , value text
  );

COPY "SubProgram_tmp" (id, program, label, value) FROM stdin;
1	VW Гарантия Мобильности /B2B	Легковые	vwMotor
2	VW Гарантия Мобильности /B2B	Коммерческие	vwcargo
3	PSA	Peugeot	peugeot
4	PSA	Citroen	citroen
5	VW Гарантия Мобильности /B2B	Продлённая гарантия (коммерческие)	vwCarePoint
6	VW Гарантия Мобильности /B2B	ДТП	vwdtp
7	GM Assistance /B2B	Opel (c 01.04.2011)	opel
8	GM Assistance /B2B	Cadillac (до 01.01.2012)	cadido2012
9	GM Assistance /B2B	Cadillac (с 01.01.2012)	cad2012
10	GM Assistance /B2B	Chevrolet NAV (Tahoe, Trail Blazer, Camaro)	chevyna
11	GM Assistance /B2B	Cлужебные	gmofficial
12	GM Assistance /B2B	Пресса	gmpress
13	GM Assistance /B2B	Chevrolet Korea	chevyko
14	Ford - помощь на дорогах /B2B	Ford	ford
15	Ford - помощь на дорогах /B2B	Ford new	fordnew
16	Мапфре /B2B	Основная	map
17	Мапфре /B2B	Citroen	mapC
18	KIA /B2B	Kia-помощь на дороге	KIA
19	KIA /B2B	Мондиаль	kiamondial
20	Европлан /B2B	Базовая	euro
21	Европлан /B2B	Стандарт	euroVW
22	Европлан /B2B	Премиум	euroFord
23	Ковидиен /B2B	Стандарт	covidienVW
24	Ковидиен /B2B	Премиум	covidienзprem
25	Атлант-М /B2B	Годовая	atlant
26	Атлант-М /B2B	От ТО до ТО	atlantot
27	Вектор Лизинг /B2B	Легковые Базовая	vecbaz
28	Вектор Лизинг /B2B	Легковые Стандарт	vecstan
29	Вектор Лизинг /B2B	Легковые VIP	euroGM
30	Вектор Лизинг /B2B	Коммерческие Базовая	corpse
31	Вектор Лизинг /B2B	Коммерческие Стандарт	covidienFord
32	Вектор Лизинг /B2B	Коммерческие VIP	covidienGM
33	Нота Банк /B2C	Базовая	notabaz
34	Нота Банк /B2C	Расширенная	notaras
35	Нота Банк /B2C	Премиум-1	notaprem1
36	Нота Банк /B2C	Премиум-2	notaprem2
37	Нота Банк /B2C	Приоритет-1	notaprior1
38	Нота Банк /B2C	Приоритет-2	notaprior2
39	Друг Компании /B2C	Базовый	drugbaz
40	Друг Компании /B2C	Стандарт	drugstan
41	Друг Компании /B2C	Престиж	drugprest
42	Друг Компании /B2C	Платинум	drugplat
43	B2C карты	Старт	b2cstart
44	B2C карты	Лайт	b2cL
45	B2C карты	Стандарт	b2cSt
46	B2C карты	Премиум	b2cPr
47	B2C карты	Платинум	b2cplat
48	B2C карты	Мото Стандарт	b2cmSt
49	B2C карты	Мото Премиум	b2cmPr
50	B2C карты	Мото Платинум	b2cmplat
51	B2C карты	РАМК	b2cramc
52	РУС-ЛАН /B2B	Основная	ruslan
53	Независимость BMW /B2B	Основная	nz
54	АВИЛОН VW /B2B	Основная	avilon
55	VW Центр Внуково /B2B	Основная	vnukovo
56	Ирито	Базовая	irito
57	Ирито	Продвинутая	iritoadv
58	Aston Martin	Основная	aston
59	Bentley	Основная	bentley
60	Ночной Сервис Европкар/B2B	Основная	night
61	AIG Надежный патруль /B2B	Основная	chartis
62	Самара-Ассистанс /B2B	Основная	samaraAssis
63	Лада Центр Белгород /B2B	Основная	lada
64	RTR Hyundai /B2B	Основная	hyundai
65	DAF NTS /B2B	Основная	daf
66	ИП Трубкин /B2B	Основная	trub
67	ARC B2B (Заявки от европейских клубов)	Основная	arc
68	Интач B2B	Основная	intb2b
69	Дженсер	Основная	gensernov
70	Autoclub Europlan	Основная	autoeuroplan
71	Цезарь Сателлит (заявки от сотрудников) /B2B	Основная	chezer
72	3S-Telematica (заявки от сотрудников)/B2B	Основная	tele
73	Авто-Цель (заявки от сотрудников) /B2B	Основная	avtoC
74	АРВАЛ (заявки от сотрудников) /B2B	Основная	arval
75	Автоимпорт (заявки от сотрудников) /B2B	Основная	auimp
76	ТДВ-Авто (заявки от сотрудников) /B2B	Основная	tdv
77	Блок Центр (заявки от сотрудников) /B2B	Основная	bm
78	Автопрестус (заявки от сотрудников) /B2B	Основная	autopres
79	Дженсер Ясенево (заявки от сотрудников) /B2B	Основная	jenser
80	Интач-помощь на дорогах /B2C	Основная	int
81	ВТБ24 /B2C	Основная	vtb24
82	ВТБ24 Автокарта/B2C	Основная	avtoVTB24
83	Петрокоммерц-Лукойл-Мастеркард /B2C	Основная	lyckoil
84	Адвокард Драйвер Голд /B2C	Основная	advocard
85	Юникредит/B2C	Основная	unicredit
86	АК Барс Банк /B2C	Основная	akbars
87	Кузьмиха Помощь на дорогах /B2C	Основная	kuz
88	Castrol Помощь на дорогах /B2C	Основная	castrol
89	ПАРИ /B2C	Основная	pari
90	Росавтобанк /B2C	Основная	rosavto
91	Цюрих /B2C	Основная	curih
92	Петрол /B2C	Основная	fleetcor
93	БАТ /B2C	Основная	bat
94	МотоПипл /B2C	Основная	motop
95	Эрго Русь /B2C	Основная	ergo
96	АГ ассистанс /B2C	Основная	agass
97	Шелл /B2C	Основная	shell
98	Уралсиб /B2C	Основная	uralsib
99	BP /B2B	Основная	bp
100	Сотрудник РАМК	Основная	ramcsotr
101	Заказ билетов	Основная	tickets
102	РАМК	Основная	ra
103	Тестовая программа	Тест 1	test1
104	Тестовая программа	Тест 2	test2
\.

INSERT INTO "SubProgram" (id, label, parent, value)
SELECT s.id, s.label, p.id, s.value FROM "SubProgram_tmp" s, "Program" p
WHERE p.label = s.program;

GRANT ALL ON "SubProgram" TO carma_db_sync;
GRANT ALL ON "SubProgram" TO carma_search;
GRANT ALL ON "SubProgram_id_seq" TO carma_db_sync;
GRANT ALL ON "SubProgram_id_seq" TO carma_search;
