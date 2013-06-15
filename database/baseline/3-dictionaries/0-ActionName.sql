CREATE TABLE "ActionName"
  (id    SERIAL PRIMARY KEY
  ,value text
  ,label text UNIQUE NOT NULL
  );
INSERT INTO Dictionary (name) VALUES ('ActionName');

INSERT INTO "ActionName" (value, label) VALUES
 ('orderService'
 ,'Заказ услуги');
INSERT INTO "ActionName" (value, label) VALUES
 ('callMeMaybe'
 ,'Заказ услуги через мобильное приложение');
INSERT INTO "ActionName" (value, label) VALUES
 ('orderServiceAnalyst'
 ,'Заказ услуги аналитиком');
INSERT INTO "ActionName" (value, label) VALUES
 ('needPartner'
 ,'Требуется партнёр');
INSERT INTO "ActionName" (value, label) VALUES
 ('tellClient'
 ,'Сообщение клиенту о договорённости');
INSERT INTO "ActionName" (value, label) VALUES
 ('cancelService'
 ,'Отказ от услуги');
INSERT INTO "ActionName" (value, label) VALUES
 ('checkStatus'
 ,'Уточнить статус оказания услуги у партнёра');
INSERT INTO "ActionName" (value, label) VALUES
 ('tellDelayClient'
 ,'Оповещение клиента о задержке');
INSERT INTO "ActionName" (value, label) VALUES
 ('checkEndOfService'
 ,'Уточнения после оказания услуги');
INSERT INTO "ActionName" (value, label) VALUES
 ('complaintResolution'
 ,'Претензия');
INSERT INTO "ActionName" (value, label) VALUES
 ('addBill'
 ,'Прикрепить счет');
INSERT INTO "ActionName" (value, label) VALUES
 ('parguyNeedInfo'
 ,'Менеджер по Партнёрам запросил доп. информацию');
INSERT INTO "ActionName" (value, label) VALUES
 ('headCheck'
 ,'Проверка РКЦ');
INSERT INTO "ActionName" (value, label) VALUES
 ('directorCheck'
 ,'Проверка директором');
INSERT INTO "ActionName" (value, label) VALUES
 ('accountCheck'
 ,'Проверка бухгалтерией');
INSERT INTO "ActionName" (value, label) VALUES
 ('analystCheck'
 ,'Проверка аналитиком');
INSERT INTO "ActionName" (value, label) VALUES
 ('getInfoDealerVW'
 ,'Уточнить информацию о ремонте у дилера/партнёра (VW, PSA)');
INSERT INTO "ActionName" (value, label) VALUES
 ('closeCase'
 ,'Закрыть заявку');
INSERT INTO "ActionName" (value, label) VALUES
 ('dealerApproval'
 ,'Согласование с дилером');
INSERT INTO "ActionName" (value, label) VALUES
 ('carmakerApproval'
 ,'Согласование с автопроизводителем');
INSERT INTO "ActionName" (value, label) VALUES
 ('dealerConf'
 ,'Требуется конференция с дилером');
INSERT INTO "ActionName" (value, label) VALUES
 ('mechanicConf'
 ,'Требуется конференция с механиком');
INSERT INTO "ActionName" (value, label) VALUES
 ('tellDealerDenied'
 ,'Оповещение об отказе дилера');
INSERT INTO "ActionName" (value, label) VALUES
 ('tellMakerDenied'
 ,'Оповещение об отказе автопроизводителя');
