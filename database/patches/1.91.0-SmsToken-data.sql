COPY "SmsTokenName" (id, var_name, label) FROM stdin;
1	program_contact_info	Контактная информация
2	program_from_name	Название программы для подстановки вместо адресата в СМС
3	program_info	Название программы
\.

CREATE TEMPORARY TABLE "SmsTokenValue_tmp" (tok int, pValue text, value text);
-- program_contact_info
COPY "SmsTokenValue_tmp" (tok, pValue, value) FROM stdin;
1	aston	+74956463477
1	bentley	+74956463477
1	nz	+74956409001
1	chezer	+74956406690
1	tele	+74956406690
1	arc	+74956463477
1	hyundai	+74956406690
1	avilon	+74956405457
1	avtoC	+74956406690
1	arval	+74956406690
1	jenser	+74956406690
1	auimp	+74956406690
1	tvd	+74956406690
1	euro	+78002508082
1	euroVW	+78002508082
1	euroFord	+78002508082
1	euroGM	+78002508082
1	corpse	+74956406690
1	map	+78002508192
1	mapC	+78002508192
1	micro	+74956406690
1	covidien	+78002507262
1	covidienVW	+78002507262
1	covidienFord	+78002507262
1	covidienGM	+78002507262
1	rnbase  RN	+78002501070
1	rnstandard	+78002501070
1	lyckoil	+78002507262
1	vnukovo	+74956408680
1	bm	+74956406690
1	lada	+74956401297
1	autopres	+74956406690
1	b2cPl	+78002507262
1	b2cPr	+78002507262
1	b2cSt	+78002507262
1	b2cL	+78002507262
1	unicredit	+78002507300
1	vtb24	+78002500024
1	akbars	+78002507262
1	bp	+74956406690
1	chartis	+78007005170
1	ford	+78002503673
1	fordPlus	+78002503673
1	cadold	+78002501218
1	cad2012	+78002501218
1	chevyko	+78002502438
1	chevyna	+78002501218
1	hum	+78002501218
1	opel	+78002501218
1	gmofficial	+78002501218
1	vmMotor	+78007007011
1	vwcargo	+78007007011
1	vwCarePoint	+78007007011
1	atlant	+74656405605
1	Cash	+74956406690
1	tickets	+74956406690
1	ramc1	+74956406690
1	ramc2	+74956406690
1	castrol	+74956406690
1	ruslan	+78002504357
1	citroen	+78001001223
1	peugeot	+78005551810
1	advocard	+74956406690
1	samaraAssis	+78002500161
\.

-- program_from_name
COPY "SmsTokenValue_tmp" (tok, pValue, value) FROM stdin;
2	aston	AstonMartin
2	bentley	Bentley
2	nz	NAssistance
2	tele	Telematica
2	chezer	Caesar
2	arc	RAMC
2	hyundai	RAMC
2	avilon	Avilon
2	avtoC	AvtoT
2	arval	ARVAL
2	jenser	GenserYA
2	auimp	Autoimport
2	tvd	TDV
2	euro	Europlan
2	euroVW	Europlan
2	euroFord	Europlan
2	euroGM	Europlan
2	corpse	RAMC
2	map	Mapfe
2	mapC	MapfeCitroen
2	micro	Assistance
2	castrol	Castrol
2	covidien	Covidien
2	covidienVW	Covidien
2	covidienFord	Covidien
2	covidienGM	Covidien
2	rnbase	RNcard
2	rnstandard	RNcard
2	lyckoil	LukoilP
2	vnukovo	VWVnykovo
2	bm	BlockMotors
2	autopres	Autoprestus
2	b2cPl	RAMC
2	b2cPr	RAMC
2	b2cSt	RAMC
2	b2cL	RAMC
2	unicredit	Unicreditbank
2	vtb24	VTB24Assist
2	akbars	AKBARS
2	bp	BPAssistance
2	chartis	Chartis
2	ford	Ford
2	fordPlus	Ford
2	lada	Lada
2	cadold	GM
2	cad2012	GM
2	chevyko	GM
2	chevyna	GM
2	hum	GM
2	opel	GM
2	gmofficial	GM
2	vmMotor	VW
2	vwcargo	VW
2	vwCarePoint	VW
2	atlant	AtlantM
2	Cash	RAMC
2	tickets	RAMC
2	ramc1	RAMC
2	ramc2	RAMC
2	ruslan	RUSLAN
2	citroen	Citroen
2	peugeot	Peugeot
2	advocard	Advocard
2	samaraAssis	SamaraAssistance
\.

-- program_info
COPY "SmsTokenValue_tmp" (tok, pValue, value) FROM stdin;
3	aston	ACTA / Aston Martin
3	bentley	ACTA / Bentley
3	nz	Независимость
3	tele	3S Телематика
3	chezer	Цезарь Сателлит
3	arc	Arc B2B
3	hyundai	RTR Hyundai
3	avilon	Авилон
3	avtoC	Авто-Цель
3	arval	АРВАЛ
3	jenser	Дженсер Ясенево
3	auimp	ДЦ Автоимпорт
3	castrol	B2C/ Кастрол
3	tvd	ДЦ ТВД-Авто
3	euro	Европлан
3	euroVW	Европлан
3	euroFord	Европлан
3	euroGM	Европлан
3	corpse	ИП Трубкин
3	map	Мапфре
3	mapC	Мапфре Citroen
3	micro	РАМК
3	covidien	Ковидиен
3	covidienVW	Ковидиен
3	covidienFord	Ковидиен
3	covidienGM	Ковидиен
3	rnbase	РН-карт-Москва
3	rnstandard	РН-карт-Москва
3	lyckoil	ЛУКОЙЛ-Петрокоммерц
3	vnukovo	VW Внуково
3	bm	Блок Моторс
3	autopres	Автопрестус
3	b2cPl	РАМК
3	b2cPr	РАМК
3	b2cL	РАМК
3	b2cSt	РАМК
3	unicredit	ЮниКредитбанк
3	vtb24	ВТБ 24
3	akbars	АК БАРС
3	bp	BP
3	chartis	Chartis Assistance
3	ford	Ford Помощь на дорогах
3	fordPlus	Ford Помощь на дорогах
3	cadold	GM Assistance
3	cad2012	GM Assistance
3	chevyko	GM Assistance
3	chevyna	GM Assistance
3	hum	GM Assistance
3	opel	GM Assistance
3	gmofficial	GM Assistance
3	vwMotor	VW Гарантия мобильности
3	vwcargo	VW Гарантия мобильности
3	vwCarePoint	VW Гарантия мобильности
3	atlant	Атлант М
3	lada	ВАЗ (Lada)
3	Cash	РАМК
3	tickets	РАМК
3	ramc1	РАМК
3	ramc2	РАМК
3	ruslan	Рус Лан
3	citroen	Citroen Assistance
3	peugeot	Peugeot Assistance
3	advocard	Адвокард
3	samaraAssis	Самара-Ассистан
\.

INSERT INTO "SmsTokenValue" (token, program, sub_program, value)
SELECT t.tok, p.id, s.id, t.value
FROM "SmsTokenValue_tmp" t, "Program" p , "SubProgram" s
WHERE s.value = t.pValue AND p.id = s.parent;
