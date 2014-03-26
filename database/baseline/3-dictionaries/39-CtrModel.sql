
create table "CtrModel" (
  id       SERIAL PRIMARY KEY,
  value    text NOT NULL,
  label    text NOT NULL
);

insert into "CtrModel" (id, value, label) values
 (0, 'Case', 'Кейс')
,(1, 'AverageCommissioner', 'Аварийный комиссар')
,(2, 'Bank', 'Банковская поддержка')
,(3, 'Consultation', 'Консультация')
,(4, 'Continue', 'Продолжение путешествия')
,(5, 'DeliverCar', 'Доставка ТС')
,(6, 'DeliverParts', 'Доставка запчастей')
,(7, 'Hotel', 'Гостиница')
,(8, 'Information', 'Информирование о происшествии')
,(9, 'LegalAssistance', 'Юридическая помощь')
,(10, 'Rent', 'Подменный автомобиль')
,(11, 'SoberDriver', 'Трезвый водитель')
,(12, 'Taxi', 'Такси')
,(13, 'Tech', 'Техпомощь')
,(14, 'TechInspect', 'TO')
,(15, 'Tickets', 'Заказ билетов')
,(16, 'Towage', 'Эвакуация')
,(17, 'Transportation', 'Транспортировка')
;

GRANT ALL ON "CtrModel" TO carma_db_sync;
GRANT ALL ON "CtrModel" TO carma_search;
GRANT ALL ON "CtrModel_id_seq" TO carma_db_sync;
GRANT ALL ON "CtrModel_id_seq" TO carma_search;
