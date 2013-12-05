CREATE TABLE "Program"
  ( id    SERIAL PRIMARY KEY
  , label text UNIQUE NOT NULL
  , client text
  , clientAddress text
  , clientCode text
  );

COPY "Program" (label) FROM stdin;
Peugeot
Citroen
ВТБ24 /B2C
VW Гарантия Мобильности /B2B
VW ДТП
GM Assistance /B2B
Chevrolet Korea
Ford - помощь на дорогах /B2B
Мапфре /B2B
Мапфре Citroen /B2B
KIA /B2B
Европлан /B2B
Ковидиен /B2B
Атлант-М /B2B
Вектор Лизинг /B2B
Нота Банк /B2C
Друг Компании /B2C
B2C карты
РУС-ЛАН /B2B
Независимость BMW /B2B
АВИЛОН VW /B2B
VW Центр Внуково /B2B
Ирито
Aston Martin
Bentley
Ночной Сервис Европкар/B2B
AIG Надежный патруль /B2B
Самара-Ассистанс /B2B
Лада Центр Белгород /B2B
RTR Hyundai /B2B
DAF NTS /B2B
ИП Трубкин /B2B
ARC B2B (Заявки от европейских клубов)
Интач B2B
Дженсер
Autoclub Europlan
Цезарь Сателлит (заявки от сотрудников) /B2B
3S-Telematica (заявки от сотрудников)/B2B
Авто-Цель (заявки от сотрудников) /B2B
АРВАЛ (заявки от сотрудников) /B2B
Автоимпорт (заявки от сотрудников) /B2B
ТДВ-Авто (заявки от сотрудников) /B2B
Блок Центр (заявки от сотрудников) /B2B
Автопрестус (заявки от сотрудников) /B2B
Дженсер Ясенево (заявки от сотрудников) /B2B
Интач-помощь на дорогах /B2C
ВТБ24 Автокарта/B2C
Петрокоммерц-Лукойл-Мастеркард /B2C
Адвокард Драйвер Голд /B2C
Юникредит/B2C
АК Барс Банк /B2C
Кузьмиха Помощь на дорогах /B2C
Castrol Помощь на дорогах /B2C
ПАРИ /B2C
Росавтобанк /B2C
Цюрих /B2C
Петрол /B2C
БАТ /B2C
МотоПипл /B2C
Эрго Русь /B2C
АГ ассистанс /B2C
Шелл /B2C
Уралсиб /B2C
Сотрудник РАМК
Заказ билетов
РАМК
Тестовая программа
\.

UPDATE "Program" n
SET client=p.client, 
    clientaddress=p.clientaddress,
    clientcode=p.clientcode
FROM programtbl p
WHERE n.label = p.label;

GRANT ALL ON "Program" TO carma_db_sync;
GRANT ALL ON "Program" TO carma_search;
GRANT ALL ON "Program_id_seq" TO carma_db_sync;
GRANT ALL ON "Program_id_seq" TO carma_search;
