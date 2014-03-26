
create table "ConstructorFieldOption" (
  id       SERIAL PRIMARY KEY,
  model    int4 NOT NULL REFERENCES "CtrModel",
  screen   int4 NOT NULL REFERENCES "CtrScreen",
  program  int4 NOT NULL, -- REFERENCES "Program" ON DELETE CASCADE,
  ord      int4 NOT NULL DEFAULT 0,
  field    text NOT NULL,
  label    text NOT NULL DEFAULT '',
  info     text NOT NULL DEFAULT '',
  required bool NOT NULL DEFAULT false,
  r        bool NOT NULL DEFAULT false,
  w        bool NOT NULL DEFAULT false,
  unique(model,screen,program,field)
);


insert into "ConstructorFieldOption"
  (model, screen, program, ord, field, label, info, required)
  values
 (0,0,0,0,'callDate','Дата звонка','',false)
,(0,0,0,1,'vwcreatedate','Дата звонка','',false)
,(0,0,0,2,'callTaker','Сотрудник РАМК','',false)
,(0,0,0,3,'comment','Что случилось','comment',true)
,(0,0,0,4,'diagnosis1','Система','system',true)
,(0,0,0,5,'diagnosis2','Узел/деталь','detail',true)
,(0,0,0,6,'diagnosis3','Описание причины неисправности','diagnosis3',false)
,(0,0,0,7,'diagnosis4','Рекомендация','recomendation',false)
,(0,0,0,8,'contact_name','Звонящий','',false)
,(0,0,0,9,'contact_phone1','Контактные телефоны','',false)
,(0,0,0,10,'contact_phone2','','',false)
,(0,0,0,11,'contact_phone3','','',false)
,(0,0,0,12,'contact_phone4','','',false)
,(0,0,0,13,'contact_email','Email','',false)
,(0,0,0,14,'contact_contactOwner','Звонящий владелец?','owner',false)
,(0,0,0,15,'contact_ownerName','Владелец','',false)
,(0,0,0,16,'contact_ownerPhone1','Контактные телефоны','',false)
,(0,0,0,17,'contact_ownerPhone2','','',false)
,(0,0,0,18,'contact_ownerPhone3','','',false)
,(0,0,0,19,'contact_ownerPhone4','','',false)
,(0,0,0,20,'contact_ownerEmail','Email','',false)
,(0,0,0,21,'program','Программа','program',true)
,(0,0,0,22,'subprogram','Подпрограмма','',false)
,(0,0,0,23,'contractIdentifier','Идентификатор контракта','',false)
,(0,0,0,24,'contract','Контракт','',false)
,(0,0,0,25,'car_vin','Автомобиль','',true)
,(0,0,0,26,'car_make','Марка','',true)
,(0,0,0,27,'car_model','Модель','',true)
,(0,0,0,28,'car_seller','Дилер, продавший автомобиль','',true)
,(0,0,0,29,'car_plateNum','Госномер','platenum',true)
,(0,0,0,30,'car_makeYear','Год производства автомобиля','',false)
,(0,0,0,31,'car_color','Цвет','',true)
,(0,0,0,32,'car_buyDate','Дата покупки','',true)
,(0,0,0,33,'car_dealerTO','Дилер у которого проходило последнее ТО','',true)
,(0,0,0,34,'car_mileage','Текущий пробег','',true)
,(0,0,0,35,'car_transmission','Коробка передач','',true)
,(0,0,0,36,'car_engine','Тип двигателя','',false)
,(0,0,0,37,'car_liters','Объём двигателя','',false)
,(0,0,0,38,'car_class','Класс автомобиля','',false)
,(0,0,0,39,'vinChecked','Участие в программе','vinChecked',true)
,(0,0,0,40,'city','Город','city',true)
,(0,0,0,41,'caseAddress_address','Адрес места поломки','caseAddress',false)
,(0,0,0,42,'caseAddress_comment','Примечания','',false)
,(0,0,0,43,'caseAddress_notRussia','Не по РФ','',false)
,(0,0,0,44,'caseAddress_coords','Координаты','coords',false)
,(0,0,0,45,'caseAddress_map','','',false)
,(0,0,0,46,'temperature','Температура','temperature',false)
,(0,0,0,47,'repair','Дата починки','',false)
,(0,0,0,48,'accord','Номер согласования','',false)
,(0,0,0,49,'dealerCause','Неисправность со слов дилера/партнёра','dealerCause',false)
,(0,0,0,50,'caseStatus','Статус кейса','',true)
,(0,0,0,51,'psaExportNeeded','Требуется выгрузка в PSA','',false)
,(0,0,0,52,'psaExported','Выгружен в PSA','',false)
,(0,0,0,53,'claim','Претензия / Благодарность','claim',false)
,(0,0,0,54,'services','Услуги','',false)
,(0,0,0,55,'actions','Действия','',false)
,(0,0,0,56,'comments','','',false)
,(0,0,0,57,'files','Прикрепленные файлы','',false)
;


GRANT ALL ON "ConstructorFieldOption" TO carma_db_sync;
GRANT ALL ON "ConstructorFieldOption" TO carma_search;
GRANT ALL ON "ConstructorFieldOption_id_seq" TO carma_db_sync;
GRANT ALL ON "ConstructorFieldOption_id_seq" TO carma_search;
