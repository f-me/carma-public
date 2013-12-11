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
  ( program text
  , label text
  , value text
  );

COPY "SubProgram_tmp" (program, label, value) FROM stdin;
VW Гарантия Мобильности /B2B	Легковые	vwMotor
VW Гарантия Мобильности /B2B	Коммерческие	vwcargo
VW Гарантия Мобильности /B2B	Продлённая гарантия (коммерческие)	vwCarePoint
VW ДТП	Основная	vwdtp
GM Assistance /B2B	Opel (до 01.04.2011)	opeldo
GM Assistance /B2B	Opel (c 01.04.2011)	opel
GM Assistance /B2B	Cadillac (до 01.01.2012)	cadido2012
GM Assistance /B2B	Cadillac (с 01.01.2012)	cad2012
GM Assistance /B2B	Chevrolet NAV (Tahoe, Trail Blazer, Camaro)	chevyna
GM Assistance /B2B	Cлужебные	gmofficial
GM Assistance /B2B	Пресса	gmpress
Chevrolet Korea	Основная	chevyko
Ford - помощь на дорогах /B2B	Ford	ford
Ford - помощь на дорогах /B2B	Ford new	fordnew
Peugeot	Основная	peugeot
Citroen	Основная	citroen
Мапфре /B2B	Основная	map
Мапфре Citroen /B2B	Основная	mapC
KIA /B2B	Kia-помощь на дороге	KIA
KIA /B2B	Мондиаль	kiamondial
Европлан /B2B	Базовая	euro
Европлан /B2B	Стандарт	euroVW
Европлан /B2B	Премиум	euroFord
Ковидиен /B2B	Стандарт	covidienVW
Ковидиен /B2B	Премиум	covidienзprem
Атлант-М /B2B	Годовая	atlant
Атлант-М /B2B	От ТО до ТО	atlantot
Вектор Лизинг /B2B	Легковые Базовая	vecbaz
Вектор Лизинг /B2B	Легковые Стандарт	vecstan
Вектор Лизинг /B2B	Легковые VIP	euroGM
Вектор Лизинг /B2B	Коммерческие Базовая	corpse
Вектор Лизинг /B2B	Коммерческие Стандарт	covidienFord
Вектор Лизинг /B2B	Коммерческие VIP	covidienGM
Нота Банк /B2C	Базовая	notabaz
Нота Банк /B2C	Расширенная	notaras
Нота Банк /B2C	Премиум-1	notaprem1
Нота Банк /B2C	Премиум-2	notaprem2
Нота Банк /B2C	Приоритет-1	notaprior1
Нота Банк /B2C	Приоритет-2	notaprior2
Друг Компании /B2C	Базовый	drugbaz
Друг Компании /B2C	Стандарт	drugstan
Друг Компании /B2C	Престиж	drugprest
Друг Компании /B2C	Платинум	drugplat
B2C карты	Старт	b2cstart
B2C карты	Лайт	b2cL
B2C карты	Стандарт	b2cSt
B2C карты	Премиум	b2cPr
B2C карты	Платинум	b2cplat
B2C карты	Мото Стандарт	b2cmSt
B2C карты	Мото Премиум	b2cmPr
B2C карты	Мото Платинум	b2cmplat
B2C карты	РАМК 	b2cramc
РУС-ЛАН /B2B	Основная	ruslan
Независимость BMW /B2B	Основная	nz
АВИЛОН VW /B2B	Основная	avilon
VW Центр Внуково /B2B	Основная	vnukovo
Ирито	Базовая	irito
Ирито	Продвинутая	iritoadv
Aston Martin	Основная	aston
Bentley	Основная	bentley
Ночной Сервис Европкар/B2B	Основная	night
AIG Надежный патруль /B2B	Основная	chartis
Самара-Ассистанс /B2B	Основная	samaraAssis
Лада Центр Белгород /B2B	Основная	lada
RTR Hyundai /B2B	Основная	hyundai
DAF NTS /B2B	Основная	daf
ИП Трубкин /B2B	Основная	trub
ARC B2B (Заявки от европейских клубов)	Основная	arc
Интач B2B	Основная	intb2b
Дженсер	Основная	gensernov
Autoclub Europlan	Основная	autoeuroplan
Цезарь Сателлит (заявки от сотрудников) /B2B	Основная	chezer
3S-Telematica (заявки от сотрудников)/B2B	Основная	tele
Авто-Цель (заявки от сотрудников) /B2B	Основная	avtoC
АРВАЛ (заявки от сотрудников) /B2B	Основная	arval
Автоимпорт (заявки от сотрудников) /B2B	Основная	auimp
ТДВ-Авто (заявки от сотрудников) /B2B	Основная	tdv
Блок Центр (заявки от сотрудников) /B2B	Основная	bm
Автопрестус (заявки от сотрудников) /B2B	Основная	autopres
Дженсер Ясенево (заявки от сотрудников) /B2B	Основная	jenser
Интач-помощь на дорогах /B2C	Основная	int
ВТБ24 /B2C	Основная	vtb24
ВТБ24 Автокарта/B2C	Основная	avtoVTB24
Петрокоммерц-Лукойл-Мастеркард /B2C	Основная	lyckoil
Адвокард Драйвер Голд /B2C	Основная	advocard
Юникредит/B2C	Основная	unicredit
АК Барс Банк /B2C	Основная	akbars
Кузьмиха Помощь на дорогах /B2C	Основная	kuz
Castrol Помощь на дорогах /B2C	Основная	castrol
ПАРИ /B2C	Основная	pari
Росавтобанк /B2C	Основная	rosavto
Цюрих /B2C	Основная	curih
Петрол /B2C	Основная	fleetcor
БАТ /B2C	Основная	bat
МотоПипл /B2C	Основная	motop
Эрго Русь /B2C	Основная	ergo
АГ ассистанс /B2C	Основная	agass
Шелл /B2C	Основная	shell
Уралсиб /B2C	Основная	uralsib
Сотрудник РАМК	Основная	ramcsotr
Заказ билетов	Основная	tickets
РАМК	Основная	ra
Тестовая программа	Тест 1	test1
Тестовая программа	Тест 2	test2
\.

INSERT INTO "SubProgram" (label, parent, value)
SELECT s.label, p.id, s.value FROM "SubProgram_tmp" s, "Program" p
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
