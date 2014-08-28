create table "CtrModel" (
  id       SERIAL PRIMARY KEY,
  value    text NOT NULL,
  label    text NOT NULL
);

insert into "CtrModel" (id, value, label) values
 (1, 'Case', 'Кейс')
,(2, 'AverageCommissioner', 'Аварийный комиссар')
,(3, 'Bank', 'Банковская поддержка')
,(4, 'Consultation', 'Консультация')
,(5, 'Continue', 'Продолжение путешествия')
,(6, 'DeliverCar', 'Доставка ТС')
,(7, 'DeliverParts', 'Доставка запчастей')
,(8, 'Hotel', 'Гостиница')
,(9, 'Information', 'Информирование о происшествии')
,(10, 'LegalAssistance', 'Юридическая помощь')
,(11, 'Rent', 'Подменный автомобиль')
,(12, 'SoberDriver', 'Трезвый водитель')
,(13, 'Taxi', 'Такси')
,(14, 'Tech', 'Техпомощь')
,(15, 'TechInspect', 'ТО')
,(16, 'Tickets', 'Заказ билетов')
,(17, 'Towage', 'Эвакуация')
,(18, 'Transportation', 'Транспортировка')
;

GRANT ALL ON "CtrModel" TO carma_db_sync;
GRANT ALL ON "CtrModel" TO carma_search;
