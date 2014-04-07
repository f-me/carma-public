CREATE TABLE "ProgramInfo"
  ( id       SERIAL PRIMARY KEY
  , program  int4 REFERENCES "Program" NOT NULL
  , info     text NOT NULL DEFAULT ''
  );

GRANT ALL ON "ProgramInfo" TO carma_db_sync;
GRANT ALL ON "ProgramInfo" TO carma_search;
GRANT ALL ON "ProgramInfo_id_seq" TO carma_db_sync;
GRANT ALL ON "ProgramInfo_id_seq" TO carma_search;

COPY "ProgramInfo" (program, info) FROM stdin;
2	ФИО и Год рождения в списке участников
3	Дата продажи &gt; 01.06.2010<br>Дата продажи &lt; 2 года назад<br>Модели: Amarok, Caddy, Caravelle, Crafter, Multivan, Transporter
4	Дата продажи &gt; 15.12.2010<br>Дата продажи &lt; 1 год назад
6	VIN в списке участников программы<br>Дата последнего ТО &lt; 1 год назад<br>Пробег - Пробег на последнем ТО &lt; 15000
7	
9	
11	VIN в базе участников программы<br>Срок действия программы &lt; сегодня<br>Пробег &lt; ограничение по пробегу (если указано)
12	
15	Номер карты участника в списке карт
16	VIN в базе участников программы<br>Пробег - Пробег на последнем ТО &lt; 15000 (20000 для Crafter)<br>Дата последнего ТО &lt; 1 год назад
21	Обращение сотрудника ACTA<br>Предоставление GOP
22	Обращение сотрудника ACTA<br>Предоставление GOP
24	VIN в списке участников программы
26	
27	
30	
34	
35	Контактное лицо из списка:
36	
37	
38	
42	
45	Сегодня &lt; 1 год со дня выдачи Сертификата «Пакет автомобилиста»
47	Номер карты участника в списке<br>Программа действует до &gt; текущая дата
50	
61	Обращение сотрудника BP
63	
\.
