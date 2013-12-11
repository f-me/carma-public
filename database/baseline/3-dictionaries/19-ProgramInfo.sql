
CREATE TABLE "ProgramInfo"
  (
    id       SERIAL PRIMARY KEY
  , program  int4 REFERENCES "programtbl" ON DELETE SET NULL
  , info     text NOT NULL DEFAULT ''
  );

GRANT ALL ON "ProgramInfo" TO carma_db_sync;
GRANT ALL ON "ProgramInfo" TO carma_search;

INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Обращение сотрудника ACTA<br>Предоставление GOP'
  FROM programtbl WHERE value = 'aston';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Обращение сотрудника ACTA<br>Предоставление GOP'
  FROM programtbl WHERE value = 'bentley';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'VIN в базе<br>Сегодня &lt; Программа действует до<br>Пробег &lt; Действует до пробега'
  FROM programtbl WHERE value = 'kraft';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Контактное лицо из списка:'
  FROM programtbl WHERE value = 'tele';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    ''
  FROM programtbl WHERE value = 'arc';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    ''
  FROM programtbl WHERE value = 'hyundai';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    ''
  FROM programtbl WHERE value = 'arval';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'ФИО и Год рождения в списке участников'
  FROM programtbl WHERE value = 'vtb24';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    ''
  FROM programtbl WHERE value = 'jenser';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    ''
  FROM programtbl WHERE value = 'avtoC';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    ''
  FROM programtbl WHERE value = 'chezer';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    ''
  FROM programtbl WHERE value = 'castrol';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    ''
  FROM programtbl WHERE value = 'euro';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    ''
  FROM programtbl WHERE value = 'euroVW';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    ''
  FROM programtbl WHERE value = 'euroFord';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    ''
  FROM programtbl WHERE value = 'euroGM';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    ''
  FROM programtbl WHERE value = 'auimp';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    ''
  FROM programtbl WHERE value = 'tvd';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    ''
  FROM programtbl WHERE value = 'corpse';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    ''
  FROM programtbl WHERE value = 'map';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    ''
  FROM programtbl WHERE value = 'mapC';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Гос. номер в списке участников программы<br>Программа действует до &gt; текущая дата'
  FROM programtbl WHERE value = 'rnbase';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Гос. номер в списке участников программы<br>Программа действует до &gt; текущая дата'
  FROM programtbl WHERE value = 'rnstandard';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Номер карты участника в списке<br>Программа действует до &gt; текущая дата'
  FROM programtbl WHERE value = 'unicredit';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Сегодня &lt; 1 год со дня выдачи Сертификата «Пакет автомобилиста»'
  FROM programtbl WHERE value = 'lyckoil';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Номер карты участника в списке карт'
  FROM programtbl WHERE value = 'b2cPl';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Номер карты участника в списке карт'
  FROM programtbl WHERE value = 'b2cPr';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Номер карты участника в списке карт'
  FROM programtbl WHERE value = 'b2cSt';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Номер карты участника в списке карт'
  FROM programtbl WHERE value = 'b2cL';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Обращение сотрудника BP'
  FROM programtbl WHERE value = 'bp';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Обращение сотрудника GM'
  FROM programtbl WHERE value = 'gmofficial';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'VIN в списке участников программы'
  FROM programtbl WHERE value = 'chartis';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'VIN в списке участников программы<br>Дата последнего ТО &lt; 1 год назад<br>Пробег - Пробег на последнем ТО &lt; 15000'
  FROM programtbl WHERE value = 'ford';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    ''
  FROM programtbl WHERE value = 'fordPlus';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    ''
  FROM programtbl WHERE value = 'lada';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Дата продажи &gt; 01.02.2010<br>Дата продажи &lt; 1 год назад<br>Модели<br>- SRX<br>- CTS<br>- STS<br>- Escalade<br>- BLS'
  FROM programtbl WHERE value = 'cadold';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Дата продажи &gt; 01.01.2012<br>Дата продажи &lt; 3 год назад<br>Модели<br>- SRX<br>- CTS<br>- STS<br>- Escalade<br>- BLS'
  FROM programtbl WHERE value = 'cad2012';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Дата продажи &gt; 15.12.2010<br>Дата продажи &lt; 1 год назад'
  FROM programtbl WHERE value = 'chevyko';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Дата продажи &gt; 01.02.2010<br>Дата продажи &lt; 1 год назад<br>Модели<br>- TrailBlazer<br>- Tahoe'
  FROM programtbl WHERE value = 'chevyna';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Дата продажи &gt; 01.02.2010<br>Дата продажи &lt; 1 год назад'
  FROM programtbl WHERE value = 'hum';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Дата продажи &gt; 01.04.2011<br>Дата продажи &lt; 3 год назад<br>Модели<br>- Antara<br>- Astra<br>- Corsa<br>- Combo Tour<br>- Combo<br>- Insignia<br>- Meriva<br>- Zafira'
  FROM programtbl WHERE value = 'opel';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Дата продажи &gt; 15.01.2010<br>Дата продажи &lt; 2 года назад'
  FROM programtbl WHERE value = 'vwMotor';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Дата продажи &gt; 01.06.2010<br>Дата продажи &lt; 2 года назад<br>Модели: Amarok, Caddy, Caravelle, Crafter, Multivan, Transporter'
  FROM programtbl WHERE value = 'vwcargo';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'Дата продажи &gt; 01.06.2010<br>Дата продажи &lt; 2 года назад<br>Модели: Amarok, Caddy, Caravelle, Crafter, Multivan, Transporter'
  FROM programtbl WHERE value = 'vwCarePoint';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'VIN в базе участников программы<br>Срок действия программы &lt; сегодня<br>Пробег &lt; ограничение по пробегу (если указано)'
  FROM programtbl WHERE value = 'atlant';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    ''
  FROM programtbl WHERE value = 'Cash';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    ''
  FROM programtbl WHERE value = 'tickets';
INSERT INTO "ProgramInfo" (program, info)
  SELECT id,
    'VIN в базе участников программы<br>Пробег - Пробег на последнем ТО &lt; 15000 (20000 для Crafter)<br>Дата последнего ТО &lt; 1 год назад'
  FROM programtbl WHERE value = 'ruslan';

