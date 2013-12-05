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
7	GM Assistance /B2B	Opel (до 01.04.2011)	opeldo
8	GM Assistance /B2B	Opel (c 01.04.2011)	opel
9	GM Assistance /B2B	Cadillac (до 01.01.2012)	cadido2012
10	GM Assistance /B2B	Cadillac (с 01.01.2012)	cad2012
11	GM Assistance /B2B	Chevrolet NAV (Tahoe, Trail Blazer, Camaro)	chevyna
12	GM Assistance /B2B	Cлужебные	gmofficial
13	GM Assistance /B2B	Пресса	gmpress
14	GM Assistance /B2B	Chevrolet Korea	chevyko
15	Ford - помощь на дорогах /B2B	Ford	ford
16	Ford - помощь на дорогах /B2B	Ford new	fordnew
17	Мапфре /B2B	Основная	map
18	Мапфре /B2B	Citroen	mapC
19	KIA /B2B	Kia-помощь на дороге	KIA
20	KIA /B2B	Мондиаль	kiamondial
21	Европлан /B2B	Базовая	euro
22	Европлан /B2B	Стандарт	euroVW
23	Европлан /B2B	Премиум	euroFord
24	Ковидиен /B2B	Стандарт	covidienVW
25	Ковидиен /B2B	Премиум	covidienзprem
26	Атлант-М /B2B	Годовая	atlant
27	Атлант-М /B2B	От ТО до ТО	atlantot
28	Вектор Лизинг /B2B	Легковые Базовая	vecbaz
29	Вектор Лизинг /B2B	Легковые Стандарт	vecstan
30	Вектор Лизинг /B2B	Легковые VIP	euroGM
31	Вектор Лизинг /B2B	Коммерческие Базовая	corpse
32	Вектор Лизинг /B2B	Коммерческие Стандарт	covidienFord
33	Вектор Лизинг /B2B	Коммерческие VIP	covidienGM
34	Нота Банк /B2C	Базовая	notabaz
35	Нота Банк /B2C	Расширенная	notaras
36	Нота Банк /B2C	Премиум-1	notaprem1
37	Нота Банк /B2C	Премиум-2	notaprem2
38	Нота Банк /B2C	Приоритет-1	notaprior1
39	Нота Банк /B2C	Приоритет-2	notaprior2
40	Друг Компании /B2C	Базовый	drugbaz
41	Друг Компании /B2C	Стандарт	drugstan
42	Друг Компании /B2C	Престиж	drugprest
43	Друг Компании /B2C	Платинум	drugplat
44	B2C карты	Старт	b2cstart
45	B2C карты	Лайт	b2cL
46	B2C карты	Стандарт	b2cSt
47	B2C карты	Премиум	b2cPr
48	B2C карты	Платинум	b2cplat
49	B2C карты	Мото Стандарт	b2cmSt
50	B2C карты	Мото Премиум	b2cmPr
51	B2C карты	Мото Платинум	b2cmplat
52	B2C карты	РАМК	b2cramc
53	РУС-ЛАН /B2B	Основная	ruslan
54	Независимость BMW /B2B	Основная	nz
55	АВИЛОН VW /B2B	Основная	avilon
56	VW Центр Внуково /B2B	Основная	vnukovo
57	Ирито	Базовая	irito
58	Ирито	Продвинутая	iritoadv
59	Aston Martin	Основная	aston
60	Bentley	Основная	bentley
61	Ночной Сервис Европкар/B2B	Основная	night
62	AIG Надежный патруль /B2B	Основная	chartis
63	Самара-Ассистанс /B2B	Основная	samaraAssis
64	Лада Центр Белгород /B2B	Основная	lada
65	RTR Hyundai /B2B	Основная	hyundai
66	DAF NTS /B2B	Основная	daf
67	ИП Трубкин /B2B	Основная	trub
68	ARC B2B (Заявки от европейских клубов)	Основная	arc
69	Интач B2B	Основная	intb2b
70	Дженсер	Основная	gensernov
71	Autoclub Europlan	Основная	autoeuroplan
72	Цезарь Сателлит (заявки от сотрудников) /B2B	Основная	chezer
73	3S-Telematica (заявки от сотрудников)/B2B	Основная	tele
74	Авто-Цель (заявки от сотрудников) /B2B	Основная	avtoC
75	АРВАЛ (заявки от сотрудников) /B2B	Основная	arval
76	Автоимпорт (заявки от сотрудников) /B2B	Основная	auimp
77	ТДВ-Авто (заявки от сотрудников) /B2B	Основная	tdv
78	Блок Центр (заявки от сотрудников) /B2B	Основная	bm
79	Автопрестус (заявки от сотрудников) /B2B	Основная	autopres
80	Дженсер Ясенево (заявки от сотрудников) /B2B	Основная	jenser
81	Интач-помощь на дорогах /B2C	Основная	int
82	ВТБ24 /B2C	Основная	vtb24
83	ВТБ24 Автокарта/B2C	Основная	avtoVTB24
84	Петрокоммерц-Лукойл-Мастеркард /B2C	Основная	lyckoil
85	Адвокард Драйвер Голд /B2C	Основная	advocard
86	Юникредит/B2C	Основная	unicredit
87	АК Барс Банк /B2C	Основная	akbars
88	Кузьмиха Помощь на дорогах /B2C	Основная	kuz
89	Castrol Помощь на дорогах /B2C	Основная	castrol
90	ПАРИ /B2C	Основная	pari
91	Росавтобанк /B2C	Основная	rosavto
92	Цюрих /B2C	Основная	curih
93	Петрол /B2C	Основная	fleetcor
94	БАТ /B2C	Основная	bat
95	МотоПипл /B2C	Основная	motop
96	Эрго Русь /B2C	Основная	ergo
97	АГ ассистанс /B2C	Основная	agass
98	Шелл /B2C	Основная	shell
99	Уралсиб /B2C	Основная	uralsib
100	BP /B2B	Основная	bp
101	Сотрудник РАМК	Основная	ramcsotr
102	Заказ билетов	Основная	tickets
103	РАМК	Основная	ra
104	Тестовая программа	Тест 1	test1
105	Тестовая программа	Тест 2	test2
\.

INSERT INTO "SubProgram" (id, label, parent, value)
SELECT s.id, s.label, p.id, s.value FROM "SubProgram_tmp" s, "Program" p
WHERE p.label = s.program;

UPDATE "SubProgram" n
SET contract = p.contracts,
    checkPeriod = p.carCheckPeriodDefault,
    validFor = p.duedateDefault,
    logo = p.logo,
    help = p.help
FROM programtbl p
WHERE n.value = p.value;

GRANT ALL ON "SubProgram" TO carma_db_sync;
GRANT ALL ON "SubProgram" TO carma_search;
GRANT ALL ON "SubProgram_id_seq" TO carma_db_sync;
GRANT ALL ON "SubProgram_id_seq" TO carma_search;
