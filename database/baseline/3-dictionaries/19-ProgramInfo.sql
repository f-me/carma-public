
CREATE TABLE "ProgramInfo"
  (
    id       SERIAL PRIMARY KEY
  , label    text NOT NULL DEFAULT ''
  , value    text UNIQUE NOT NULL
  );

GRANT ALL ON "ProgramInfo" TO carma_db_sync;
GRANT ALL ON "ProgramInfo" TO carma_search;

INSERT INTO "ProgramInfo" (value, label) VALUES
    ('aston', 'Обращение сотрудника ACTA<br>Предоставление GOP')
  , ('bentley', 'Обращение сотрудника ACTA<br>Предоставление GOP')
  , ('kraft', 'VIN в базе<br>Сегодня &lt; Программа действует до<br>Пробег &lt; Действует до пробега')
  , ('tele', 'Контактное лицо из списка:')
  , ('arc', '')
  , ('hyundai', '')
  , ('arval', '')
  , ('vtb24', 'ФИО и Год рождения в списке участников')
  , ('jenser', '')
  , ('avtoC', '')
  , ('chezer', '')
  , ('castrol', '')
  , ('euro', '')
  , ('euroVW', '')
  , ('euroFord', '')
  , ('euroGM', '')
  , ('auimp', '')
  , ('tvd', '')
  , ('corpse', '')
  , ('map', '')
  , ('mapC', '')
  , ('rnbase', 'Гос. номер в списке участников программы<br>Программа действует до &gt; текущая дата')
  , ('rnstandard', 'Гос. номер в списке участников программы<br>Программа действует до &gt; текущая дата')
  , ('unicredit', 'Номер карты участника в списке<br>Программа действует до &gt; текущая дата')
  , ('lyckoil', 'Сегодня &lt; 1 год со дня выдачи Сертификата «Пакет автомобилиста»')
  , ('b2cPl', 'Номер карты участника в списке карт')
  , ('b2cPr', 'Номер карты участника в списке карт')
  , ('b2cSt', 'Номер карты участника в списке карт')
  , ('b2cL', 'Номер карты участника в списке карт')
  , ('bp', 'Обращение сотрудника BP')
  , ('gmofficial', 'Обращение сотрудника GM')
  , ('chartis', 'VIN в списке участников программы')
  , ('ford', 'VIN в списке участников программы<br>Дата последнего ТО &lt; 1 год назад<br>Пробег - Пробег на последнем ТО &lt; 15000')
  , ('fordPlus', '')
  , ('lada', '')
  , ('cadold', 'Дата продажи &gt; 01.02.2010<br>Дата продажи &lt; 1 год назад<br>Модели<br>- SRX<br>- CTS<br>- STS<br>- Escalade<br>- BLS')
  , ('cad2012', 'Дата продажи &gt; 01.01.2012<br>Дата продажи &lt; 3 год назад<br>Модели<br>- SRX<br>- CTS<br>- STS<br>- Escalade<br>- BLS')
  , ('chevyko', 'Дата продажи &gt; 15.12.2010<br>Дата продажи &lt; 1 год назад')
  , ('chevyna', 'Дата продажи &gt; 01.02.2010<br>Дата продажи &lt; 1 год назад<br>Модели<br>- TrailBlazer<br>- Tahoe')
  , ('hum', 'Дата продажи &gt; 01.02.2010<br>Дата продажи &lt; 1 год назад')
  , ('opel', 'Дата продажи &gt; 01.04.2011<br>Дата продажи &lt; 3 год назад<br>Модели<br>- Antara<br>- Astra<br>- Corsa<br>- Combo Tour<br>- Combo<br>- Insignia<br>- Meriva<br>- Zafira')
  , ('vwMotor', 'Дата продажи &gt; 15.01.2010<br>Дата продажи &lt; 2 года назад')
  , ('vwcargo', 'Дата продажи &gt; 01.06.2010<br>Дата продажи &lt; 2 года назад<br>Модели: Amarok, Caddy, Caravelle, Crafter, Multivan, Transporter')
  , ('vwCarePoint', 'Дата продажи &gt; 01.06.2010<br>Дата продажи &lt; 2 года назад<br>Модели: Amarok, Caddy, Caravelle, Crafter, Multivan, Transporter')
  , ('atlant', 'VIN в базе участников программы<br>Срок действия программы &lt; сегодня<br>Пробег &lt; ограничение по пробегу (если указано)')
  , ('Cash', '')
  , ('tickets', '')
  , ('ruslan', 'VIN в базе участников программы<br>Пробег - Пробег на последнем ТО &lt; 15000 (20000 для Crafter)<br>Дата последнего ТО &lt; 1 год назад')
