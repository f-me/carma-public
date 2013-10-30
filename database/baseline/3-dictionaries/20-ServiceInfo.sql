CREATE TABLE "ServiceInfo"
  (
    id      SERIAL PRIMARY KEY
  , program int4 REFERENCES "programtbl" ON DELETE SET NULL
  , service text NOT NULL
  , info    text NOT NULL DEFAULT ''
  );

GRANT ALL ON "ServiceInfo" TO carma_db_sync;
GRANT ALL ON "ServiceInfo" TO carma_search;

INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Правила оказания:</b><br>Если неполадка не может быть устранена в течении 45 минут заказывается эвакуация в ДЦ'
  FROM programtbl WHERE value = 'aston';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    ''
  FROM programtbl WHERE value = 'aston';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    '<b>Условия:</b><br>Неисправность не может быть исправлена в день обращения<br><b>Правила оказания:</b><br>Максимум 2 дня<br>До 14 дней заграницей<br>Класс G'
  FROM programtbl WHERE value = 'aston';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    '<b>Условия:</b><br>Неисправоность не может быть исправлена в день обращения<br>Неисправность возникла в 80 км от места проживания клиента<br><b>Правила оказания:</b><br>Максимум 2 дня<br>Завтрак должен быть включён'
  FROM programtbl WHERE value = 'aston';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличные'
  FROM programtbl WHERE value = 'aston';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'aston';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    '<b>Условия:</b><br>Неисправоность не может быть исправлена в день обращения<br>Неисправность возникла в 80 км от места проживания клиента'
  FROM programtbl WHERE value = 'aston';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    '<b>Условия:</b><br>Неисправность не может быть исправлена в течении 8 дней<br><b>Правила оказания:</b><br>Если клиент за границей'
  FROM programtbl WHERE value = 'aston';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'aston';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'aston';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'aston';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'aston';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'aston';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'aston';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    'За наличные'
  FROM programtbl WHERE value = 'bentley';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Правила оказания:</b><br>До ближайшего ДЦ'
  FROM programtbl WHERE value = 'bentley';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    '<b>Правила оказания:</b><br>На 3 рабочих дня<br>BMW 5 series, Mercedes E class, Audi A6 или другие автомобили того же класса<br>По требованию компании BML срок пользования подменным автомобилем может выть продлен с дальнейшим выставлением компании счета за услуги в случае, если инцидент случился за пределами страны, в которой автомобиль был куплен'
  FROM programtbl WHERE value = 'bentley';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    '<b>Условия:</b><br>Неисправность не может быть исправлена в день обращения<br>Неисправность возникла в 80 км от места проживания клиента<br><b>Правила оказания:</b><br>Для водителя и пассажиров<br>Максимум 2 дня'
  FROM programtbl WHERE value = 'bentley';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    '<b>Правила оказания:</b><br>До ближайшего воказал или аэропорта<br>Доставка к отремонтированному ТС'
  FROM programtbl WHERE value = 'bentley';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'bentley';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    '<b>Правила оказания:</b><br>Билеты 1ого класса на поезд, если ехать меньше 6 часов<br>Билеты 1ого класса на самолёт, если поездом ехать больше 6 часов'
  FROM programtbl WHERE value = 'bentley';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    ''
  FROM programtbl WHERE value = 'bentley';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'bentley';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'bentley';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'bentley';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'bentley';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'bentley';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'bentley';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Условия:</b><br>&lt; 100 км от Москвы<br><b>Правила оказания:</b><br>Включая подзарядку АКБ, замена колёс, не включая вскрытие автомобиля и подвоз топлива'
  FROM programtbl WHERE value = 'kraft';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Условия:</b><br>&lt; 100 км от Москвы<br><b>Правила оказания:</b><br>Только в Автокрафт'
  FROM programtbl WHERE value = 'kraft';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличные'
  FROM programtbl WHERE value = 'kraft';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    'За наличные'
  FROM programtbl WHERE value = 'kraft';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличные'
  FROM programtbl WHERE value = 'kraft';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'kraft';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличные'
  FROM programtbl WHERE value = 'kraft';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'kraft';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'kraft';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'kraft';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'kraft';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'kraft';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'kraft';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'kraft';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Правила оказания:</b><br>До 1850 рублей в черте города'
  FROM programtbl WHERE value = 'tele';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Правила оказания:</b><br>Ограничение по стоимости'
  FROM programtbl WHERE value = 'tele';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличные'
  FROM programtbl WHERE value = 'tele';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    '<b>Правила оказания:</b><br>Ограничение по стоимости'
  FROM programtbl WHERE value = 'tele';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличные'
  FROM programtbl WHERE value = 'tele';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'tele';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличные'
  FROM programtbl WHERE value = 'tele';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'tele';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'tele';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'tele';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'tele';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'tele';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'tele';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'tele';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Правила оказания:</b><br>До 1850 рублей в черте города'
  FROM programtbl WHERE value = 'arc';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Правила оказания:</b><br>Ограничение по стоимости'
  FROM programtbl WHERE value = 'arc';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличные'
  FROM programtbl WHERE value = 'arc';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    '<b>Правила оказания:</b><br>Ограничение по стоимости'
  FROM programtbl WHERE value = 'arc';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличные'
  FROM programtbl WHERE value = 'arc';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'arc';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличные'
  FROM programtbl WHERE value = 'arc';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'arc';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'arc';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'arc';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'arc';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'arc';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'arc';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'arc';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Правила оказания:</b><br>До 1850 рублей в черте города'
  FROM programtbl WHERE value = 'hyundai';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Правила оказания:</b><br>Ограничение по стоимости'
  FROM programtbl WHERE value = 'hyundai';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличные'
  FROM programtbl WHERE value = 'hyundai';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    '<b>Правила оказания:</b><br>Ограничение по стоимости'
  FROM programtbl WHERE value = 'hyundai';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличные'
  FROM programtbl WHERE value = 'hyundai';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'hyundai';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличные'
  FROM programtbl WHERE value = 'hyundai';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'hyundai';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'hyundai';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'hyundai';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'hyundai';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'hyundai';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'hyundai';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'hyundai';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Правила оказания:</b><br>До 1850 рублей в черте города'
  FROM programtbl WHERE value = 'arval';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Правила оказания:</b><br>Ограничение по стоимости'
  FROM programtbl WHERE value = 'arval';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличные'
  FROM programtbl WHERE value = 'arval';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    '<b>Правила оказания:</b><br>Ограничение по стоимости'
  FROM programtbl WHERE value = 'arval';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличные'
  FROM programtbl WHERE value = 'arval';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'arval';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличные'
  FROM programtbl WHERE value = 'arval';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'arval';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'arval';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'arval';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'arval';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'arval';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'arval';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'arval';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Предоставляемые услуги техпомощи:</b><br>Подвоз топлива<br>Замена колеса<br>Запуск двигателя<br><b>Условия:</b><br>&lt;100 км до клиента<br>Не ДТП<br><b>Правила оказания:</b><br>Оплата топлива по чеку АЗС, только если топливо закончилось (Подвоз топлива)'
  FROM programtbl WHERE value = 'vtb24';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Условия:</b><br>&lt;100 км до клиента<br>Не ДТП'
  FROM programtbl WHERE value = 'vtb24';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличные'
  FROM programtbl WHERE value = 'vtb24';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    'За наличные'
  FROM programtbl WHERE value = 'vtb24';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличные'
  FROM programtbl WHERE value = 'vtb24';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'vtb24';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличные'
  FROM programtbl WHERE value = 'vtb24';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'vtb24';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'vtb24';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    ''
  FROM programtbl WHERE value = 'vtb24';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'vtb24';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    ''
  FROM programtbl WHERE value = 'vtb24';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'vtb24';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'vtb24';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Правила оказания:</b><br>До 1850 рублей в черте города'
  FROM programtbl WHERE value = 'jenser';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Правила оказания:</b><br>Ограничение по стоимости'
  FROM programtbl WHERE value = 'jenser';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличные'
  FROM programtbl WHERE value = 'jenser';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    '<b>Правила оказания:</b><br>Ограничение по стоимости'
  FROM programtbl WHERE value = 'jenser';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличные'
  FROM programtbl WHERE value = 'jenser';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'jenser';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличные'
  FROM programtbl WHERE value = 'jenser';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'jenser';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'jenser';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'jenser';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'jenser';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'jenser';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'jenser';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'jenser';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Правила оказания:</b><br>До 1850 рублей в черте города'
  FROM programtbl WHERE value = 'auimp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Правила оказания:</b><br>Ограничение по стоимости'
  FROM programtbl WHERE value = 'auimp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличные'
  FROM programtbl WHERE value = 'auimp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    '<b>Правила оказания:</b><br>Ограничение по стоимости'
  FROM programtbl WHERE value = 'auimp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличные'
  FROM programtbl WHERE value = 'auimp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'auimp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличные'
  FROM programtbl WHERE value = 'auimp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'auimp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'auimp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'auimp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'auimp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'auimp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'auimp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'auimp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Правила оказания:</b><br>До 1850 рублей в черте города'
  FROM programtbl WHERE value = 'tvd';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Правила оказания:</b><br>Ограничение по стоимости'
  FROM programtbl WHERE value = 'tvd';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличные'
  FROM programtbl WHERE value = 'tvd';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    '<b>Правила оказания:</b><br>Ограничение по стоимости'
  FROM programtbl WHERE value = 'tvd';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличные'
  FROM programtbl WHERE value = 'tvd';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'tvd';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличные'
  FROM programtbl WHERE value = 'tvd';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'tvd';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'tvd';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'tvd';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'tvd';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'tvd';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'tvd';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'tvd';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Правила оказания:</b><br>До 1850 рублей в черте города'
  FROM programtbl WHERE value = 'corpse';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Правила оказания:</b><br>Ограничение по стоимости'
  FROM programtbl WHERE value = 'corpse';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличные'
  FROM programtbl WHERE value = 'corpse';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    '<b>Правила оказания:</b><br>Ограничение по стоимости'
  FROM programtbl WHERE value = 'corpse';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличные'
  FROM programtbl WHERE value = 'corpse';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'corpse';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличные'
  FROM programtbl WHERE value = 'corpse';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'corpse';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'corpse';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'corpse';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'corpse';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'corpse';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'corpse';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'corpse';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Предоставляемые услуги техпомощи:</b><br>Подвоз топлива<br>Замена колеса<br>Запуск двигателя<br><b>Условия:</b><br>&lt;100 км до клиента<br>Не ДТП<br><b>Правила оказания:</b><br>Оплата топлива по чеку АЗС, только если топливо закончилось (Подвоз топлива)'
  FROM programtbl WHERE value = 'rnbase';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Условия:</b><br>&lt;100 км до клиента<br>Не ДТП'
  FROM programtbl WHERE value = 'rnbase';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличные'
  FROM programtbl WHERE value = 'rnbase';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    'За наличные'
  FROM programtbl WHERE value = 'rnbase';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличные'
  FROM programtbl WHERE value = 'rnbase';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'rnbase';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличные'
  FROM programtbl WHERE value = 'rnbase';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'rnbase';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'rnbase';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    ''
  FROM programtbl WHERE value = 'rnbase';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'rnbase';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'rnbase';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'rnbase';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'rnbase';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Предоставляемые услуги техпомощи:</b><br>Подвоз топлива<br>Замена колеса<br>Запуск двигателя<br><b>Условия:</b><br>&lt;100 км до клиента<br>Не ДТП<br><b>Правила оказания:</b><br>Оплата топлива по чеку АЗС, только если топливо закончилось (Подвоз топлива)'
  FROM programtbl WHERE value = 'rnstandard';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Условия:</b><br>&lt;100 км до клиента<br>Не ДТП'
  FROM programtbl WHERE value = 'rnstandard';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличные'
  FROM programtbl WHERE value = 'rnstandard';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    'За наличные'
  FROM programtbl WHERE value = 'rnstandard';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличные'
  FROM programtbl WHERE value = 'rnstandard';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'rnstandard';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличные'
  FROM programtbl WHERE value = 'rnstandard';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'rnstandard';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'rnstandard';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    ''
  FROM programtbl WHERE value = 'rnstandard';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'rnstandard';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'rnstandard';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'rnstandard';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'rnstandard';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Предоставляемые услуги техпомощи:</b><br>Подвоз топлива<br>Вскрытие автомобиля<br><b>Условия:</b><br>Не более одного раза в год (Вскрытие автомобиля)<br><b>Правила оказания:</b><br>Оплата топлива по чеку АЗС, только если топливо закончилось'
  FROM programtbl WHERE value = 'unicredit';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Условия:</b><br>&lt;100 км до клиента<br>ДТП<br>Не более одного раза в год'
  FROM programtbl WHERE value = 'unicredit';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличные'
  FROM programtbl WHERE value = 'unicredit';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    'За наличные'
  FROM programtbl WHERE value = 'unicredit';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    '<b>Условия:</b><br>Эвакуация РАМК<br>ДТП<br><b>Правила оказания:</b><br>Максимум 1000 руб. с НДС'
  FROM programtbl WHERE value = 'unicredit';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'unicredit';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличные'
  FROM programtbl WHERE value = 'unicredit';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'unicredit';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'unicredit';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    ''
  FROM programtbl WHERE value = 'unicredit';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'unicredit';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'unicredit';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'unicredit';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'unicredit';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Предоставляемые услуги техпомощи:</b><br>Подвоз топлива<br><b>Правила оказания:</b><br>Оплата топлива по чеку АЗС, только если топливо закончилось<br>До 20 литров'
  FROM programtbl WHERE value = 'lyckoil';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Условия:</b><br>&lt;100 км от города<br>ДТП, офрмлено в ГИБДД<br>Не более одного раза в год'
  FROM programtbl WHERE value = 'lyckoil';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличные'
  FROM programtbl WHERE value = 'lyckoil';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    'За наличные'
  FROM programtbl WHERE value = 'lyckoil';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    '<b>Условия:</b><br>Эвакуация РАМК<br>ДТП<br><b>Правила оказания:</b><br>Максимум 1000 руб. с НДС'
  FROM programtbl WHERE value = 'lyckoil';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'lyckoil';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличные'
  FROM programtbl WHERE value = 'lyckoil';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'lyckoil';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'lyckoil';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    '<b>Условия:</b><br>По телефону'
  FROM programtbl WHERE value = 'lyckoil';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'lyckoil';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'lyckoil';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'lyckoil';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'lyckoil';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Предоставляемые услуги техпомощи:</b><br>Подвоз топлива<br>Замена колеса<br>Запуск двигателя<br>Вскрытие автомобиля<br><b>Условия:</b><br>Не более двух раз за срок действия программы<br>Не более одного раза за срок действия программы (Вскрытие автомобиля)<br>&lt; 50 км от МКАД или &lt;30 км от города<br><b>Правила оказания:</b><br>Топливо оплачивает клиент(Подвоз топлива)'
  FROM programtbl WHERE value = 'b2c';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Условия:</b><br>Не более двух раз за срок действия программы<br>&lt; 50 км от МКАД или &lt;30 км от города'
  FROM programtbl WHERE value = 'b2c';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличные'
  FROM programtbl WHERE value = 'b2c';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    'За наличные'
  FROM programtbl WHERE value = 'b2c';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличные'
  FROM programtbl WHERE value = 'b2c';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'b2c';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличные'
  FROM programtbl WHERE value = 'b2c';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'b2c';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'b2c';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'b2c';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    '<b>Условия:</b><br>Не более одного раза в год'
  FROM programtbl WHERE value = 'b2c';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'b2c';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'b2c';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'b2c';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    'За наличные'
  FROM programtbl WHERE value = 'b2cplat';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    'За наличные'
  FROM programtbl WHERE value = 'b2cplat';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличные'
  FROM programtbl WHERE value = 'b2cplat';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    'За наличные'
  FROM programtbl WHERE value = 'b2cplat';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличные'
  FROM programtbl WHERE value = 'b2cplat';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'b2cplat';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличные'
  FROM programtbl WHERE value = 'b2cplat';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'b2cplat';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'b2cplat';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    ''
  FROM programtbl WHERE value = 'b2cplat';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'b2cplat';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'b2cplat';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'b2cplat';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'b2cplat';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Предоставляемые услуги техпомощи:</b><br>Слив топлива<br><b>Правила оказания:</b><br>Лимит стоимости 1490 рублей'
  FROM programtbl WHERE value = 'bp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Правила оказания:</b><br>Лимит стоимости 1490 рублей'
  FROM programtbl WHERE value = 'bp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличные'
  FROM programtbl WHERE value = 'bp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    'За наличные'
  FROM programtbl WHERE value = 'bp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличные'
  FROM programtbl WHERE value = 'bp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'bp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличные'
  FROM programtbl WHERE value = 'bp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'bp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'bp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'bp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'bp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'bp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'bp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'bp';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Предоставляемые услуги техпомощи:</b><br>Зарядка АКБ<br>Замена колеса<br>Вскрытие автомобиля<br>Подвоз топлива<br><b>Условия:</b><br>&lt; 100 км от Москвы или Спб'
  FROM programtbl WHERE value = 'chartis';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Условия:</b><br>&lt; 100 км от Москвы или Спб<br>Не ДТП<br>Не вандализм<br><b>Правила оказания:</b><br>Если на гарантии к ближайшему дилеру<br>Если не на гарантии, то в любой сервис'
  FROM programtbl WHERE value = 'chartis';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    '<b>Условия:</b><br>Неисправность не может быть исправлена в день обращения<br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>До 3 суток'
  FROM programtbl WHERE value = 'chartis';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    'За наличные'
  FROM programtbl WHERE value = 'chartis';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    '<b>Условия:</b><br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>До 50 км'
  FROM programtbl WHERE value = 'chartis';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'chartis';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    '<b>Условия:</b><br>Неисправность не может быть исправлена в день обращения<br>Клиент воспользовался эвакуацией РАМК<br>СТО в 100 км от места проживания клиента<br>Клиент документально подтверждает, что путешествие было запланировано<br><b>Правила оказания:</b><br>Железнодорожний билеты первого класса<br>Авиабилеты эконом-класса (если до точки назначения &gt; 600 км)<br>Лимит стоимости 50000 рублей'
  FROM programtbl WHERE value = 'chartis';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'chartis';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'chartis';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    ''
  FROM programtbl WHERE value = 'chartis';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'chartis';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'chartis';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'chartis';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'chartis';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Условия:</b><br>&lt; 200 км до клиента<br><b>Правила оказания:</b><br>Бесплатно 200 км, остальное за наличные (в том числе возвращение на дорожное полотно)'
  FROM programtbl WHERE value = 'ford';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Условия:</b><br>&lt; 200 км до ДЦ<br><b>Правила оказания:</b><br>Бесплатно 200 км, остальное за наличные'
  FROM programtbl WHERE value = 'ford';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    '<b>Условия:</b><br>Неисправность не может быть исправлена в день обращения<br>Клиент воспользовался эвакуацией РАМК<br>Требуется согласование производителя<br><b>Правила оказания:</b><br>На срок не более чем 2 рабочих днях'
  FROM programtbl WHERE value = 'ford';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    'За наличные'
  FROM programtbl WHERE value = 'ford';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    '<b>Условия:</b><br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Лимит стоимости 1000 рублей с НДС (820 рублей без НДС)<br>От места поломки или от ДЦ до дома клиента'
  FROM programtbl WHERE value = 'ford';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'ford';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличные'
  FROM programtbl WHERE value = 'ford';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'ford';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'ford';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'ford';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'ford';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    '<b>Условия:</b><br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>До трёх лиц'
  FROM programtbl WHERE value = 'ford';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'ford';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'ford';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Условия:</b><br>&lt; 130 км до клиента<br><b>Правила оказания:</b><br>Устранение поломки на месте будет предприниматься во всех случаях, за исключением тех, где могут быть гарантийные обязательства'
  FROM programtbl WHERE value = 'cadold';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Условия:</b><br>&lt; 130 км от клиента до ДЦ<br>Клиент договорился с ДЦ<br><b>Правила оказания:</b><br>Автомобиль с прицепом или без<br>Эвакуация до любого места, если до ближайшего авторизированного дилера &gt; 130 км'
  FROM programtbl WHERE value = 'cadold';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    '<b>Условия:</b><br>Требуется согласование производителя<br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Максимум на 4 дня'
  FROM programtbl WHERE value = 'cadold';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    '<b>Условия:</b><br>Неисправность возникла в 130 км от места проживания клиента<br>Неисправность не может быть исправлена в день обращения<br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Максимум 4 дня<br>3 звезды или эквивалент'
  FROM programtbl WHERE value = 'cadold';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    '<b>Условия:</b><br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Лимит стоимости 1854 рубля<br>Оплата проезда от места неисправности или от мастерской до пункта посадки на транспорт (или обратно)'
  FROM programtbl WHERE value = 'cadold';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    '<b>Условия:</b><br>Не более трёх раз за срок действия программы<br>&lt; 130 км<br><b>Правила оказания:</b><br>Если ОСАГО на неограниченное число лиц - вызывается трезвый водитель.<br>Если ОСАГО ограничено вызывается эвакуатор (при этом услуга засчитывается как Трезвый водитель)'
  FROM programtbl WHERE value = 'cadold';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    '<b>Условия:</b><br>Неисправность не может быть исправлена в день обращения<br>Клиент воспользовался эвакуацией РАМК<br>Клиент документально подтверждает, что путешествие было запланировано<br><b>Правила оказания:</b><br>Железнодорожний билеты первого класса или авиабилет экономкласса (если время в пути поездом превышает 8 часов) <br>Лимит стоимости 27585 рублей на клиента'
  FROM programtbl WHERE value = 'cadold';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'cadold';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    '<b>Условия:</b><br>Клиент воспользовался эвакуацией РАМК<br>В стране нет возможности получить деталь через авторизованного дилера GM<br><b>Правила оказания:</b><br>Оплачивается организация доставки, таможенную очистку (за исключением стоимости таможенных сборов) и расходы по транспортировке запасной части. <br>В случае гарантийной поломки обязанность по оплате стоимости запасных частей ложится на GM , во всех остальных случаях на Клиента.'
  FROM programtbl WHERE value = 'cadold';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'cadold';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'cadold';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'cadold';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'cadold';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'cadold';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Условия:</b><br>&lt; 130 км до клиента<br><b>Правила оказания:</b><br>Устранение поломки на месте будет предприниматься во всех случаях, за исключением тех, где могут быть гарантийные обязательства<br>Доставка топлива: <b>Условия:</b><br>&lt; 130 км<br><b>Правила оказания:</b><br>Клиент оплачивает по квитанции само топливо, доставка топлива бесплатна'
  FROM programtbl WHERE value = 'cad2012';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Условия:</b><br>&lt; 130 км от клиента до ДЦ<br>Клиент договорился с ДЦ<br><b>Правила оказания:</b><br>Автомобиль с прицепом или без<br>Эвакуация до любого места, если до ближайшего авторизированного дилера &gt; 130 км'
  FROM programtbl WHERE value = 'cad2012';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    '<b>Условия:</b><br>Требуется согласование производителя<br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Максимум на 4 дня'
  FROM programtbl WHERE value = 'cad2012';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    '<b>Условия:</b><br>Неисправность возникла в 130 км от места проживания клиента<br>Неисправность не может быть исправлена в день обращения<br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Максимум 4 дня<br>3 звезды или эквивалент'
  FROM programtbl WHERE value = 'cad2012';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    '<b>Условия:</b><br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Лимит стоимости 1854 рубля<br>Оплата проезда от места неисправности или от мастерской до пункта посадки на транспорт (или обратно)'
  FROM programtbl WHERE value = 'cad2012';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'cad2012';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    '<b>Условия:</b><br>Неисправность не может быть исправлена в день обращения<br>Клиент воспользовался эвакуацией РАМК<br>Клиент документально подтверждает, что путешествие было запланировано<br><b>Правила оказания:</b><br>Железнодорожний билеты первого класса или авиабилет экономкласса (если время в пути поездом превышает 8 часов) <br>Лимит стоимости 27585 рублей на клиента'
  FROM programtbl WHERE value = 'cad2012';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'cad2012';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    '<b>Условия:</b><br>Клиент воспользовался эвакуацией РАМК<br>В стране нет возможности получить деталь через авторизованного дилера GM<br><b>Правила оказания:</b><br>Оплачивается организация доставки, таможенную очистку (за исключением стоимости таможенных сборов) и расходы по транспортировке запасной части. <br>В случае гарантийной поломки обязанность по оплате стоимости запасных частей ложится на GM , во всех остальных случаях на Клиента.'
  FROM programtbl WHERE value = 'cad2012';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'cad2012';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'cad2012';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'cad2012';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'cad2012';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'cad2012';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Условия:</b><br>&lt; 130 км до клиента<br><b>Правила оказания:</b><br>Устранение поломки на месте будет предприниматься во всех случаях, за исключением тех, где могут быть гарантийные обязательства'
  FROM programtbl WHERE value = 'chevyko';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Условия:</b><br>&lt; 130 км от клиента до ДЦ<br>Клиент договорился с ДЦ<br><b>Правила оказания:</b><br>Автомобиль с прицепом или без<br>Эвакуация до любого места, если до ближайшего авторизированного дилера &gt; 130 км'
  FROM programtbl WHERE value = 'chevyko';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    '<b>Условия:</b><br>Требуется согласование производителя<br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Максимум на 4 дня'
  FROM programtbl WHERE value = 'chevyko';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    '<b>Условия:</b><br>Неисправность возникла в 130 км от места проживания клиента<br>Неисправность не может быть исправлена в день обращения<br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Максимум 4 дня<br>3 звезды или эквивалент'
  FROM programtbl WHERE value = 'chevyko';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    '<b>Условия:</b><br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Лимит стоимости 1854 рубля<br>Оплата проезда от места неисправности или от мастерской до пункта посадки на транспорт (или обратно)'
  FROM programtbl WHERE value = 'chevyko';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'chevyko';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    '<b>Условия:</b><br>Неисправность не может быть исправлена в день обращения<br>Клиент воспользовался эвакуацией РАМК<br>Клиент документально подтверждает, что путешествие было запланировано<br><b>Правила оказания:</b><br>Железнодорожний билеты первого класса или авиабилет экономкласса (если время в пути поездом превышает 8 часов) <br>Лимит стоимости 27585 рублей на клиента'
  FROM programtbl WHERE value = 'chevyko';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'chevyko';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    '<b>Условия:</b><br>Клиент воспользовался эвакуацией РАМК<br>В стране нет возможности получить деталь через авторизованного дилера GM<br><b>Правила оказания:</b><br>Оплачивается организация доставки, таможенную очистку (за исключением стоимости таможенных сборов) и расходы по транспортировке запасной части. <br>В случае гарантийной поломки обязанность по оплате стоимости запасных частей ложится на GM , во всех остальных случаях на Клиента.'
  FROM programtbl WHERE value = 'chevyko';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'chevyko';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'chevyko';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'chevyko';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'chevyko';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'chevyko';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Условия:</b><br>&lt; 130 км до клиента<br><b>Правила оказания:</b><br>Устранение поломки на месте будет предприниматься во всех случаях, за исключением тех, где могут быть гарантийные обязательства'
  FROM programtbl WHERE value = 'chevyna';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Условия:</b><br>&lt; 130 км от клиента до ДЦ<br>Клиент договорился с ДЦ<br><b>Правила оказания:</b><br>Автомобиль с прицепом или без<br>Эвакуация до любого места, если до ближайшего авторизированного дилера &gt; 130 км'
  FROM programtbl WHERE value = 'chevyna';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    '<b>Условия:</b><br>Требуется согласование производителя<br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Максимум на 4 дня'
  FROM programtbl WHERE value = 'chevyna';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    '<b>Условия:</b><br>Неисправность возникла в 130 км от места проживания клиента<br>Неисправность не может быть исправлена в день обращения<br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Максимум 4 дня<br>3 звезды или эквивалент'
  FROM programtbl WHERE value = 'chevyna';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    '<b>Условия:</b><br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Лимит стоимости 1854 рубля<br>Оплата проезда от места неисправности или от мастерской до пункта посадки на транспорт (или обратно)'
  FROM programtbl WHERE value = 'chevyna';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'chevyna';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    '<b>Условия:</b><br>Неисправность не может быть исправлена в день обращения<br>Клиент воспользовался эвакуацией РАМК<br>Клиент документально подтверждает, что путешествие было запланировано<br><b>Правила оказания:</b><br>Железнодорожний билеты первого класса или авиабилет экономкласса (если время в пути поездом превышает 8 часов) <br>Лимит стоимости 27585 рублей на клиента'
  FROM programtbl WHERE value = 'chevyna';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'chevyna';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    '<b>Условия:</b><br>Клиент воспользовался эвакуацией РАМК<br>В стране нет возможности получить деталь через авторизованного дилера GM<br><b>Правила оказания:</b><br>Оплачивается организация доставки, таможенную очистку (за исключением стоимости таможенных сборов) и расходы по транспортировке запасной части. <br>В случае гарантийной поломки обязанность по оплате стоимости запасных частей ложится на GM , во всех остальных случаях на Клиента.'
  FROM programtbl WHERE value = 'chevyna';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'chevyna';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'chevyna';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'chevyna';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'chevyna';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'chevyna';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Условия:</b><br>&lt; 130 км до клиента<br><b>Правила оказания:</b><br>Устранение поломки на месте будет предприниматься во всех случаях, за исключением тех, где могут быть гарантийные обязательства'
  FROM programtbl WHERE value = 'hum';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Условия:</b><br>&lt; 130 км от клиента до ДЦ<br>Клиент договорился с ДЦ<br><b>Правила оказания:</b><br>Автомобиль с прицепом или без<br>Эвакуация до любого места, если до ближайшего авторизированного дилера &gt; 130 км'
  FROM programtbl WHERE value = 'hum';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    '<b>Условия:</b><br>Требуется согласование производителя<br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Максимум на 4 дня'
  FROM programtbl WHERE value = 'hum';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    '<b>Условия:</b><br>Неисправность возникла в 130 км от места проживания клиента<br>Неисправность не может быть исправлена в день обращения<br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Максимум 4 дня<br>3 звезды или эквивалент'
  FROM programtbl WHERE value = 'hum';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    '<b>Условия:</b><br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Лимит стоимости 1854 рубля<br>Оплата проезда от места неисправности или от мастерской до пункта посадки на транспорт (или обратно)'
  FROM programtbl WHERE value = 'hum';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'hum';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    '<b>Условия:</b><br>Неисправность не может быть исправлена в день обращения<br>Клиент воспользовался эвакуацией РАМК<br>Клиент документально подтверждает, что путешествие было запланировано<br><b>Правила оказания:</b><br>Железнодорожний билеты первого класса или авиабилет экономкласса (если время в пути поездом превышает 8 часов) <br>Лимит стоимости 27585 рублей на клиента'
  FROM programtbl WHERE value = 'hum';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'hum';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    '<b>Условия:</b><br>Клиент воспользовался эвакуацией РАМК<br>В стране нет возможности получить деталь через авторизованного дилера GM<br><b>Правила оказания:</b><br>Оплачивается организация доставки, таможенную очистку (за исключением стоимости таможенных сборов) и расходы по транспортировке запасной части. <br>В случае гарантийной поломки обязанность по оплате стоимости запасных частей ложится на GM , во всех остальных случаях на Клиента.'
  FROM programtbl WHERE value = 'hum';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'hum';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'hum';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'hum';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'hum';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'hum';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Условия:</b><br>&lt; 130 км до клиента<br><b>Правила оказания:</b><br>Устранение поломки на месте будет предприниматься во всех случаях, за исключением тех, где могут быть гарантийные обязательства'
  FROM programtbl WHERE value = 'opel';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Условия:</b><br>&lt; 130 км от клиента до ДЦ<br>Клиент договорился с ДЦ<br><b>Правила оказания:</b><br>Автомобиль с прицепом или без<br>Эвакуация до любого места, если до ближайшего авторизированного дилера &gt; 130 км'
  FROM programtbl WHERE value = 'opel';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    '<b>Условия:</b><br>Требуется согласование производителя<br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Максимум на 4 дня'
  FROM programtbl WHERE value = 'opel';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    '<b>Условия:</b><br>Неисправность возникла в 130 км от места проживания клиента<br>Неисправность не может быть исправлена в день обращения<br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Максимум 4 дня<br>3 звезды или эквивалент'
  FROM programtbl WHERE value = 'opel';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    '<b>Условия:</b><br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Лимит стоимости 1854 рубля<br>Оплата проезда от места неисправности или от мастерской до пункта посадки на транспорт (или обратно)'
  FROM programtbl WHERE value = 'opel';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'opel';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    '<b>Условия:</b><br>Неисправность не может быть исправлена в день обращения<br>Клиент воспользовался эвакуацией РАМК<br>Клиент документально подтверждает, что путешествие было запланировано<br><b>Правила оказания:</b><br>Железнодорожний билеты первого класса или авиабилет экономкласса (если время в пути поездом превышает 8 часов) <br>Лимит стоимости 27585 рублей на клиента'
  FROM programtbl WHERE value = 'opel';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'opel';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    '<b>Условия:</b><br>Клиент воспользовался эвакуацией РАМК<br>В стране нет возможности получить деталь через авторизованного дилера GM<br><b>Правила оказания:</b><br>Оплачивается организация доставки, таможенную очистку (за исключением стоимости таможенных сборов) и расходы по транспортировке запасной части. <br>В случае гарантийной поломки обязанность по оплате стоимости запасных частей ложится на GM , во всех остальных случаях на Клиента.'
  FROM programtbl WHERE value = 'opel';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'opel';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'opel';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'opel';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'opel';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'opel';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Предоставляемые услуги техпомощи:</b><br>Замена колеса<br>Зарядка аккумулятора<br><b>Условия:</b><br>&lt; 125 км до клиента<br>Прокол больше чем 1 колеса<br><b>Правила оказания:</b><br>Бесплатно 125 км, всё остальное за наличные'
  FROM programtbl WHERE value = 'vwMotor';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Условия:</b><br>&lt; 125 км до ДЦ<br>Поломка (не ДТП и не вандализм)<br>Клиент договорился с ДЦ<br><b>Правила оказания</b><br>Бесплатно 125 км, всё остальное за наличные'
  FROM programtbl WHERE value = 'vwMotor';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    '<b>Условия:</b><br>Неисправность не может быть исправлена в день обращения<br>Клиент воспользовался эвакуацией РАМК<br>Если клиент самостоятельно доехал до дилера, то от дилера требуется письмо подтверждением (TODO: шаблон письма)<br>Требуется согласование производителя<br>Клиент не пользовался услугой Гостиница<br><b>Правила оказания:</b><br>До 5 дней для владельцев VW Phaeton<br>До 3 дней для владельцев остальных моделей'
  FROM programtbl WHERE value = 'vwMotor';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    '<b>Условия:</b><br>Неисправность возникла в 80 км от места проживания клиента<br>Неисправность не может быть исправлена в день обращения<br>Клиент не пользовался услугой Подменный автомобиль<br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Не более 5 ночей для владельцев VW Phaeton<br>Не более 1 ночи для владельцев остальных моделей<br>Для водителя и всех пассажиров (но не более чем посадочных мест в автомобиле)<br>Не более 6600 рублей без НДС на человека на день'
  FROM programtbl WHERE value = 'vwMotor';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'vwMotor';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'vwMotor';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'vwMotor';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'vwMotor';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'vwMotor';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'vwMotor';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'vwMotor';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'vwMotor';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'vwMotor';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'vwMotor';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Предоставляемые услуги техпомощи:</b><br>Замена колеса<br>Зарядка аккумулятора<br><b>Условия:</b><br>&lt; 250 км до клиента<br>Прокол больше чем 1 колеса<br><b>Правила оказания:</b><br>Бесплатно 250 км, всё остальное за наличные'
  FROM programtbl WHERE value = 'vwcargo';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Условия:</b><br>&lt; 250 км до ДЦ<br>Поломка (не ДТП и не вандализм)<br><b>Правила оказания:</b><br>Бесплатно 250 км, всё остальное за наличные'
  FROM programtbl WHERE value = 'vwcargo';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    '<b>Условия:</b><br>Неисправность не может быть исправлена в день обращения<br>Клиент воспользовался эвакуацией РАМК<br>Клиент не пользовался услугой Гостиница<br>Если клиент самостоятельно доехал до дилера, то от дилера требуется письмо подтверждением (TODO: шаблон письма)<br><b>Правила оказания:</b><br>До трёх дней'
  FROM programtbl WHERE value = 'vwcargo';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    '<b>Условия:</b><br>Неисправность возникла в 80 км от места проживания клиента<br>Неисправность не может быть исправлена в день обращения<br>Клиент не пользовался услугой Подменный автомобиль<br>Клиент воспользовался эвакуацией РАМК<br><b>Правила оказания:</b><br>Не более 1 ночи для водителя и всех пассажиров (для VW Crafter водитель и не более чем 2 пассажира)<br>Не более 6600 рублей без НДС на человека на день'
  FROM programtbl WHERE value = 'vwcargo';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'vwcargo';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'vwcargo';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'vwcargo';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'vwcargo';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'vwcargo';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'vwcargo';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'vwcargo';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'vwcargo';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'vwcargo';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'vwcargo';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Предоставляемые услуги техпомощи:</b><br>Зарядка АКБ<br>Замена колеса<br><b>Условия:</b><br>&lt;125 км до клиента<br><b>Правила оказания:</b>Не включает в себя возвращение на дорогу, вскрытие автомобиля, подвоз топлива, слив топлива'
  FROM programtbl WHERE value = 'atlant';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Условия:</b><br>&lt; 125 км<br>Не вандализм<br><b>Правила оказания:</b><br>Только в ДЦ Аталант М'
  FROM programtbl WHERE value = 'atlant';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'atlant';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'atlant';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'atlant';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'atlant';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'atlant';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'atlant';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'atlant';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'atlant';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'atlant';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'Правила оказания:</b><br>До трёх лиц'
  FROM programtbl WHERE value = 'atlant';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'atlant';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'atlant';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    'За наличные'
  FROM programtbl WHERE value = 'Cash';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    'За наличные'
  FROM programtbl WHERE value = 'Cash';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличные'
  FROM programtbl WHERE value = 'Cash';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    'За наличные'
  FROM programtbl WHERE value = 'Cash';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличные'
  FROM programtbl WHERE value = 'Cash';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'Cash';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличные'
  FROM programtbl WHERE value = 'Cash';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'Cash';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'Cash';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'Cash';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'Cash';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'Cash';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'Cash';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличные'
  FROM programtbl WHERE value = 'Cash';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    'За наличные'
  FROM programtbl WHERE value = 'tickets';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    'За наличные'
  FROM programtbl WHERE value = 'tickets';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличные'
  FROM programtbl WHERE value = 'tickets';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    'За наличные'
  FROM programtbl WHERE value = 'tickets';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличные'
  FROM programtbl WHERE value = 'tickets';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличные'
  FROM programtbl WHERE value = 'tickets';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличные'
  FROM programtbl WHERE value = 'tickets';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличные'
  FROM programtbl WHERE value = 'tickets';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличные'
  FROM programtbl WHERE value = 'tickets';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличные'
  FROM programtbl WHERE value = 'tickets';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличные'
  FROM programtbl WHERE value = 'tickets';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличные'
  FROM programtbl WHERE value = 'tickets';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличные'
  FROM programtbl WHERE value = 'tickets';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    ''
  FROM programtbl WHERE value = 'tickets';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech',
    '<b>Предоставляемые услуги техпомощи:</b><br>Зарядка АКБ<br>Замена колеса<br><b>Условия:</b><br>&lt;125 км до клиента<br><b>Правила оказания:</b>Не включает в себя возвращение на дорогу, вскрытие автомобиля и подвоз топлива'
  FROM programtbl WHERE value = 'ruslan';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'towage',
    '<b>Условия:</b><br>&lt; 125 км<br><b>Правила оказания:</b><br>Эвакуация только в Рус Лан<br>В нерабочее время эвакуация домой, затем в Рус Лан<br>В том числе в случае ДТП'
  FROM programtbl WHERE value = 'ruslan';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'rent',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'ruslan';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'hotel',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'ruslan';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'taxi',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'ruslan';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'sober',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'ruslan';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'transportation',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'ruslan';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverCar',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'ruslan';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'deliverParts',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'ruslan';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'ken',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'ruslan';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tech1',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'ruslan';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'information',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'ruslan';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'bank',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'ruslan';
INSERT INTO "ServiceInfo" (program, service, info)
  SELECT id,
    'tickets',
    'За наличный рассчёт'
  FROM programtbl WHERE value = 'ruslan';

