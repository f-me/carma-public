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
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'serviceOrdered',
         'Услуга заказана',
         id
    FROM "ActionName" WHERE value = 'orderService';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'serviceOrderedSMS',
         'Услуга заказана (SMS)',
         id
    FROM "ActionName" WHERE value = 'orderService';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'needPartner',
         'Требуется партнёр',
         id
    FROM "ActionName" WHERE value = 'orderService';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'moveToAnalyst',
         'Передать аналитику',
         id
    FROM "ActionName" WHERE value = 'orderService';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'busyLine',
         'Не дозвонился',
         id
    FROM "ActionName" WHERE value = 'orderService';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'clientCanceledService',
         'Клиент отказался от услуги',
         id
    FROM "ActionName" WHERE value = 'orderService';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'communicated',
         'Коммуникация произведена',
         id
    FROM "ActionName" WHERE value = 'callMeMaybe';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'busyLine',
         'Не дозвонился',
         id
    FROM "ActionName" WHERE value = 'callMeMaybe';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'serviceOrderedAnalyst',
         'Услуга заказана аналитиком',
         id
    FROM "ActionName" WHERE value = 'orderServiceAnalyst';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'needPartnerAnalyst',
         'Партнёр не найден',
         id
    FROM "ActionName" WHERE value = 'orderServiceAnalyst';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'moveToBack',
         'Вернуть в Back Office',
         id
    FROM "ActionName" WHERE value = 'orderServiceAnalyst';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'busyLine',
         'Не дозвонился',
         id
    FROM "ActionName" WHERE value = 'orderServiceAnalyst';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'clientCanceledService',
         'Клиент отказался от услуги',
         id
    FROM "ActionName" WHERE value = 'orderServiceAnalyst';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'partnerFound',
         'Партнёр найден',
         id
    FROM "ActionName" WHERE value = 'needPartner';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'partnerNotFound',
         'Партнёр не найден',
         id
    FROM "ActionName" WHERE value = 'needPartner';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'clientCanceledService',
         'Клиент отказался от услуги',
         id
    FROM "ActionName" WHERE value = 'needPartner';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'bigDelay',
         'Отложить на 6 часов',
         id
    FROM "ActionName" WHERE value = 'needPartner';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'busyLine',
         'Не дозвонился',
         id
    FROM "ActionName" WHERE value = 'tellClient';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'partnerNotOk',
         'Отказ от условий или партнёра',
         id
    FROM "ActionName" WHERE value = 'tellClient';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'partnerNotOkCancel',
         'Полный отказ от условий или партнёра',
         id
    FROM "ActionName" WHERE value = 'tellClient';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'partnerOk',
         'Клиент согласен с условиями',
         id
    FROM "ActionName" WHERE value = 'tellClient';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'clientCanceledService',
         'Клиент отказался от услуги',
         id
    FROM "ActionName" WHERE value = 'tellClient';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'busyLine',
         'Не дозвонился',
         id
    FROM "ActionName" WHERE value = 'cancelService';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'falseCallWBill',
         'Ложный вызов, с выставлением счета',
         id
    FROM "ActionName" WHERE value = 'cancelService';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'falseCallWOBill',
         'Ложный вызов, без выставления счета',
         id
    FROM "ActionName" WHERE value = 'cancelService';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'clientCanceledService',
         'Клиент отказался от услуги',
         id
    FROM "ActionName" WHERE value = 'cancelService';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'busyLine',
         'Не дозвонился',
         id
    FROM "ActionName" WHERE value = 'checkStatus';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'serviceDelayed',
         'Оказание услуги задерживается',
         id
    FROM "ActionName" WHERE value = 'checkStatus';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'serviceInProgress',
         'Услуга в процессе оказания',
         id
    FROM "ActionName" WHERE value = 'checkStatus';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'prescheduleService',
         'Услуга оказана',
         id
    FROM "ActionName" WHERE value = 'checkStatus';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'clientCanceledService',
         'Клиент отказался от услуги',
         id
    FROM "ActionName" WHERE value = 'checkStatus';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'busyLine',
         'Не дозвонился',
         id
    FROM "ActionName" WHERE value = 'tellDelayClient';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'clientWaiting',
         'Клиент ожидает',
         id
    FROM "ActionName" WHERE value = 'tellDelayClient';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'partnerNotOk',
         'Отказ от партнёра',
         id
    FROM "ActionName" WHERE value = 'tellDelayClient';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'partnerNotOkCalncel',
         'Полный отказ от услуг',
         id
    FROM "ActionName" WHERE value = 'tellDelayClient';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'clientCanceledService',
         'Клиент отказался от услуги',
         id
    FROM "ActionName" WHERE value = 'tellDelayClient';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'busyLine',
         'Не дозвонился',
         id
    FROM "ActionName" WHERE value = 'checkEndOfService';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'serviceStillInProgress',
         'Услуга ещё в процессе оказания',
         id
    FROM "ActionName" WHERE value = 'checkEndOfService';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'complaint',
         'Клиент предъявил претензию',
         id
    FROM "ActionName" WHERE value = 'checkEndOfService';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'serviceFinished',
         'Услуга успешно оказана',
         id
    FROM "ActionName" WHERE value = 'checkEndOfService';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'busyLine',
         'Не дозвонился',
         id
    FROM "ActionName" WHERE value = 'complaintResolution';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'complaintManaged',
         'Претензия разрешена',
         id
    FROM "ActionName" WHERE value = 'complaintResolution';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'bigDelay',
         'Отложить на 6 часов',
         id
    FROM "ActionName" WHERE value = 'complaintResolution';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'weekDelay',
         'Отложить на неделю',
         id
    FROM "ActionName" WHERE value = 'complaintResolution';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'billNotReady',
         'Счёт не предоставлен',
         id
    FROM "ActionName" WHERE value = 'addBill';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'billAttached',
         'Счёт введён',
         id
    FROM "ActionName" WHERE value = 'addBill';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'parguyToBack',
         'Вернуть в Backoffice',
         id
    FROM "ActionName" WHERE value = 'addBill';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'bigDelay',
         'Отложить на 6 часов',
         id
    FROM "ActionName" WHERE value = 'addBill';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'weekDelay',
         'Отложить на неделю',
         id
    FROM "ActionName" WHERE value = 'addBill';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'backToParyguy',
         'Вернуть менеджеру по партнёрам',
         id
    FROM "ActionName" WHERE value = 'parguyNeedInfo';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'bigDelay',
         'Отложить на 6 часов',
         id
    FROM "ActionName" WHERE value = 'parguyNeedInfo';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'weekDelay',
         'Отложить на неделю',
         id
    FROM "ActionName" WHERE value = 'parguyNeedInfo';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'headToParyguy',
         'На доработку МпП',
         id
    FROM "ActionName" WHERE value = 'headCheck';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'confirm',
         'Услуга проверена РКЦ',
         id
    FROM "ActionName" WHERE value = 'headCheck';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'confirmWODirector',
         'Ок (без проверки директором)',
         id
    FROM "ActionName" WHERE value = 'headCheck';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'confirmFinal',
         'Ок (без проверки директором и бухгалтерией)',
         id
    FROM "ActionName" WHERE value = 'headCheck';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'bigDelay',
         'Отложить на 6 часов',
         id
    FROM "ActionName" WHERE value = 'headCheck';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'weekDelay',
         'Отложить на неделю',
         id
    FROM "ActionName" WHERE value = 'headCheck';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'directorToHead',
         'Требуется доработка РКЦ',
         id
    FROM "ActionName" WHERE value = 'directorCheck';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'directorConfirm',
         'Услуга проверена директором',
         id
    FROM "ActionName" WHERE value = 'directorCheck';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'dirConfirmFinal',
         'Ок (без проверки бухгалтерией)',
         id
    FROM "ActionName" WHERE value = 'directorCheck';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'bigDelay',
         'Отложить на 6 часов',
         id
    FROM "ActionName" WHERE value = 'directorCheck';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'weekDelay',
         'Отложить на неделю',
         id
    FROM "ActionName" WHERE value = 'directorCheck';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'accountToDirector',
         'Требуется доработка директора',
         id
    FROM "ActionName" WHERE value = 'accountCheck';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'accountConfirm',
         'Проверено бухгалтерией',
         id
    FROM "ActionName" WHERE value = 'accountCheck';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'bigDelay',
         'Отложить на 6 часов',
         id
    FROM "ActionName" WHERE value = 'accountCheck';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'weekDelay',
         'Отложить на неделю',
         id
    FROM "ActionName" WHERE value = 'accountCheck';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'analystChecked',
         'Проверено аналитиком',
         id
    FROM "ActionName" WHERE value = 'analystCheck';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'bigDelay',
         'Отложить на 6 часов',
         id
    FROM "ActionName" WHERE value = 'analystCheck';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'weekDelay',
         'Отложить на неделю',
         id
    FROM "ActionName" WHERE value = 'analystCheck';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'busyLine',
         'Не дозвонился',
         id
    FROM "ActionName" WHERE value = 'getInfoDealerVW';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'callLater',
         'Требуется перезвонить',
         id
    FROM "ActionName" WHERE value = 'getInfoDealerVW';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'vwclosed',
         'Информация получена',
         id
    FROM "ActionName" WHERE value = 'getInfoDealerVW';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'bigDelay',
         'Отложить на 6 часов',
         id
    FROM "ActionName" WHERE value = 'getInfoDealerVW';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'weekDelay',
         'Отложить на неделю',
         id
    FROM "ActionName" WHERE value = 'getInfoDealerVW';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'busyLine',
         'Не дозвонился',
         id
    FROM "ActionName" WHERE value = 'closeCase';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'callLater',
         'Требуется перезвонить',
         id
    FROM "ActionName" WHERE value = 'closeCase';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'caseClosed',
         'Заявка закрыта',
         id
    FROM "ActionName" WHERE value = 'closeCase';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'partnerGivenCloseTime',
         'Закрыть заявку позже',
         id
    FROM "ActionName" WHERE value = 'closeCase';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'bigDelay',
         'Отложить на 6 часов',
         id
    FROM "ActionName" WHERE value = 'closeCase';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'weekDelay',
         'Отложить на неделю',
         id
    FROM "ActionName" WHERE value = 'closeCase';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'busyLine',
         'Не дозвонился',
         id
    FROM "ActionName" WHERE value = 'dealerApproval';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'callLater',
         'Перезвонить',
         id
    FROM "ActionName" WHERE value = 'dealerApproval';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'dealerApproved',
         'Дилер одобрил',
         id
    FROM "ActionName" WHERE value = 'dealerApproval';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'dealerNotApproved',
         'Дилер отказал',
         id
    FROM "ActionName" WHERE value = 'dealerApproval';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'clientCanceledService',
         'Клиент отказался от услуги',
         id
    FROM "ActionName" WHERE value = 'dealerApproval';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'busyLine',
         'Не дозвонился',
         id
    FROM "ActionName" WHERE value = 'carmakerApproval';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'callLater',
         'Перезвонить',
         id
    FROM "ActionName" WHERE value = 'carmakerApproval';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'carmakerApproved',
         'Автопроизводитель одобрил',
         id
    FROM "ActionName" WHERE value = 'carmakerApproval';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'carmakerNotApproved',
         'Автопроизводитель отказал',
         id
    FROM "ActionName" WHERE value = 'carmakerApproval';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'clientCanceledService',
         'Клиент отказался от услуги',
         id
    FROM "ActionName" WHERE value = 'carmakerApproval';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'busyLine',
         'Не дозвонился',
         id
    FROM "ActionName" WHERE value = 'dealerConf';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'callLater',
         'Перезвонить',
         id
    FROM "ActionName" WHERE value = 'dealerConf';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'needService',
         'Требуется заказ услуги',
         id
    FROM "ActionName" WHERE value = 'dealerConf';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'notNeedService',
         'Заказ услуги не требуется',
         id
    FROM "ActionName" WHERE value = 'dealerConf';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'clientCanceledService',
         'Клиент отказался от услуги',
         id
    FROM "ActionName" WHERE value = 'dealerConf';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'busyLine',
         'Не дозвонился',
         id
    FROM "ActionName" WHERE value = 'mechanicConf';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'callLater',
         'Перезвонить',
         id
    FROM "ActionName" WHERE value = 'mechanicConf';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'needService',
         'Требуется заказ услуги',
         id
    FROM "ActionName" WHERE value = 'mechanicConf';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'notNeedService',
         'Заказ услуги не требуется',
         id
    FROM "ActionName" WHERE value = 'mechanicConf';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'clientCanceledService',
         'Клиент отказался от услуги',
         id
    FROM "ActionName" WHERE value = 'mechanicConf';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'busyLine',
         'Не дозвонился',
         id
    FROM "ActionName" WHERE value = 'tellDealerDenied';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'clientNotified',
         'Клиент оповещён',
         id
    FROM "ActionName" WHERE value = 'tellDealerDenied';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'clientCanceledService',
         'Клиент отказался от услуги',
         id
    FROM "ActionName" WHERE value = 'tellDealerDenied';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'busyLine',
         'Не дозвонился',
         id
    FROM "ActionName" WHERE value = 'tellMakerDenied';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'clientNotified',
         'Клиент оповещён',
         id
    FROM "ActionName" WHERE value = 'tellMakerDenied';
INSERT INTO "ActionResult" (value, label, parent)
  SELECT 'clientCanceledService',
         'Клиент отказался от услуги',
         id
    FROM "ActionName" WHERE value = 'tellMakerDenied';
INSERT INTO "CallerType" (value, label) VALUES
 ('client'
 ,'Клиент');
INSERT INTO "CallerType" (value, label) VALUES
 ('contr'
 ,'Подрядчик');
INSERT INTO "CallerType" (value, label) VALUES
 ('dealer'
 ,'Дилерский центр');
INSERT INTO "CallerType" (value, label) VALUES
 ('partner'
 ,'Заказчик программы');
INSERT INTO "CallerType" (value, label) VALUES
 ('staff'
 ,'Сотрудник');
INSERT INTO "CallerType" (value, label) VALUES
 ('other'
 ,'Другое');
INSERT INTO "CallType" (value, label, parent)
  SELECT 'newCase',
         'Кейс/Создание нового кейса',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'processingCase',
         'Кейс/Обработка кейса',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoCase',
         'Кейс/Запрос информации',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseP',
         'Претензии/На подрядчика',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseD',
         'Претензии/На ДЦ',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseR',
         'Претензии/На РАМК',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseZ',
         'Претензии/На Заказчика',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoD',
         'Информация/О ДЦ',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoPr',
         'Информация/О программе',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoPrOther',
         'Информация/Другое',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchD',
         'Переключение/Партнёр',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchDealer',
         'Переключение/Дилер',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchIn',
         'Переключение/Внутренний номер',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'mechanicConsOk',
         'Консультация механика помогла',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'mechanicConsNotOk',
         'Консультация механика не помогла',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'operatorConsOk',
         'Консультация оператора помогла',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'operatorConsNotOk',
         'Консультация оператора не помогла',
         id
    FROM "CallerType" WHERE value = 'client';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'processingCase',
         'Кейс/Обработка кейса',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoCase',
         'Кейс/Запрос информации',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseCl',
         'Претензии/На клиента',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseP',
         'Претензии/На подрядчика',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseR',
         'Претензии/На РАМК',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchD',
         'Переключение/Партнёр',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchIn',
         'Переключение/Внутренний номер',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchDealer',
         'Переключение/Дилер',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoPrOther',
         'Информация/Другое',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'contracts',
         'Договора',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'acct',
         'Счета',
         id
    FROM "CallerType" WHERE value = 'contr';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'processingCase',
         'Кейс/Обработка кейса',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoCase',
         'Кейс/Запрос информации',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseCl',
         'Претензии/На клиента',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseP',
         'Претензии/На подрядчика',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseR',
         'Претензии/На РАМК',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchD',
         'Переключение/Партнёр',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchIn',
         'Переключение/Внутренний номер',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchDealer',
         'Переключение/Дилер',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoPrOther',
         'Информация/Другое',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'contracts',
         'Договора',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'acct',
         'Счета',
         id
    FROM "CallerType" WHERE value = 'dealer';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'newCase',
         'Кейс/Создание нового кейса',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'processingCase',
         'Кейс/Обработка кейса',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoCase',
         'Кейс/Запрос информации',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseD',
         'Претензии/На ДЦ',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseP',
         'Претензии/На подрядчика',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'pretenseR',
         'Претензии/На РАМК',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchD',
         'Переключение/Партнёр',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchIn',
         'Переключение/Внутренний номер',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchDealer',
         'Переключение/Дилер',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoPrOther',
         'Информация/Другое',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'contracts',
         'Договора',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'acct',
         'Счета',
         id
    FROM "CallerType" WHERE value = 'partner';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchD',
         'Переключение/Партнёр',
         id
    FROM "CallerType" WHERE value = 'staff';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchIn',
         'Переключение/Внутренний номер',
         id
    FROM "CallerType" WHERE value = 'staff';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'info',
         'Запрос информации',
         id
    FROM "CallerType" WHERE value = 'staff';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchD',
         'Переключение/Партнёр',
         id
    FROM "CallerType" WHERE value = 'other';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchIn',
         'Переключение/Внутренний номер',
         id
    FROM "CallerType" WHERE value = 'other';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'switchDealer',
         'Переключение/Дилер',
         id
    FROM "CallerType" WHERE value = 'other';
INSERT INTO "CallType" (value, label, parent)
  SELECT 'infoPrOther',
         'Информация/Другое',
         id
    FROM "CallerType" WHERE value = 'other';
INSERT INTO "CarMaker" (value, label) VALUES
 ('vw'
 ,'Volkswagen');
INSERT INTO "CarMaker" (value, label) VALUES
 ('chevy'
 ,'Chevrolet');
INSERT INTO "CarMaker" (value, label) VALUES
 ('opel'
 ,'Opel');
INSERT INTO "CarMaker" (value, label) VALUES
 ('cad'
 ,'Cadillac');
INSERT INTO "CarMaker" (value, label) VALUES
 ('hum'
 ,'Hummer');
INSERT INTO "CarMaker" (value, label) VALUES
 ('ford'
 ,'Ford');
INSERT INTO "CarMaker" (value, label) VALUES
 ('lada'
 ,'ВАЗ (Lada)');
INSERT INTO "CarMaker" (value, label) VALUES
 ('bmw'
 ,'BMW');
INSERT INTO "CarMaker" (value, label) VALUES
 ('alfa'
 ,'Alfa Romeo');
INSERT INTO "CarMaker" (value, label) VALUES
 ('audi'
 ,'Audi');
INSERT INTO "CarMaker" (value, label) VALUES
 ('bentley'
 ,'Bentley');
INSERT INTO "CarMaker" (value, label) VALUES
 ('chrysler'
 ,'Chrysler');
INSERT INTO "CarMaker" (value, label) VALUES
 ('citroen'
 ,'Citroen');
INSERT INTO "CarMaker" (value, label) VALUES
 ('daewoo'
 ,'Daewoo');
INSERT INTO "CarMaker" (value, label) VALUES
 ('dodge'
 ,'Dodge');
INSERT INTO "CarMaker" (value, label) VALUES
 ('fiat'
 ,'Fiat');
INSERT INTO "CarMaker" (value, label) VALUES
 ('holden'
 ,'Holden');
INSERT INTO "CarMaker" (value, label) VALUES
 ('honda'
 ,'Honda');
INSERT INTO "CarMaker" (value, label) VALUES
 ('hyundai'
 ,'Hyundai');
INSERT INTO "CarMaker" (value, label) VALUES
 ('jaguar'
 ,'Jaguar');
INSERT INTO "CarMaker" (value, label) VALUES
 ('jeep'
 ,'Jeep');
INSERT INTO "CarMaker" (value, label) VALUES
 ('kia'
 ,'Kia');
INSERT INTO "CarMaker" (value, label) VALUES
 ('lancia'
 ,'Lancia');
INSERT INTO "CarMaker" (value, label) VALUES
 ('land'
 ,'Land Rover');
INSERT INTO "CarMaker" (value, label) VALUES
 ('lexus'
 ,'Lexus');
INSERT INTO "CarMaker" (value, label) VALUES
 ('maserati'
 ,'Maserati');
INSERT INTO "CarMaker" (value, label) VALUES
 ('maybach'
 ,'Maybach');
INSERT INTO "CarMaker" (value, label) VALUES
 ('mazda'
 ,'Mazda');
INSERT INTO "CarMaker" (value, label) VALUES
 ('mercedes'
 ,'Mercedes-Benz');
INSERT INTO "CarMaker" (value, label) VALUES
 ('mini'
 ,'MINI');
INSERT INTO "CarMaker" (value, label) VALUES
 ('mitsubishi'
 ,'Mitsubishi');
INSERT INTO "CarMaker" (value, label) VALUES
 ('nissan'
 ,'Nissan');
INSERT INTO "CarMaker" (value, label) VALUES
 ('peugeot'
 ,'Peugeot');
INSERT INTO "CarMaker" (value, label) VALUES
 ('renault'
 ,'Renault');
INSERT INTO "CarMaker" (value, label) VALUES
 ('rolls'
 ,'Rolls-Royce');
INSERT INTO "CarMaker" (value, label) VALUES
 ('rover'
 ,'Rover');
INSERT INTO "CarMaker" (value, label) VALUES
 ('saab'
 ,'Saab');
INSERT INTO "CarMaker" (value, label) VALUES
 ('Samand'
 ,'Samand');
INSERT INTO "CarMaker" (value, label) VALUES
 ('seat'
 ,'Seat');
INSERT INTO "CarMaker" (value, label) VALUES
 ('skoda'
 ,'Skoda');
INSERT INTO "CarMaker" (value, label) VALUES
 ('smart'
 ,'Smart');
INSERT INTO "CarMaker" (value, label) VALUES
 ('subaru'
 ,'Subaru');
INSERT INTO "CarMaker" (value, label) VALUES
 ('suzuki'
 ,'Suzuki');
INSERT INTO "CarMaker" (value, label) VALUES
 ('toyota'
 ,'Toyota');
INSERT INTO "CarMaker" (value, label) VALUES
 ('volvo'
 ,'Volvo');
INSERT INTO "CarMaker" (value, label) VALUES
 ('maz'
 ,'Maz');
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'alfa147',
         '147',
         id
    FROM "CarMaker" WHERE value = 'alfa';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'alfa166',
         '166',
         id
    FROM "CarMaker" WHERE value = 'alfa';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'a2',
         'A2',
         id
    FROM "CarMaker" WHERE value = 'audi';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'a3',
         'A3',
         id
    FROM "CarMaker" WHERE value = 'audi';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'a4',
         'A4',
         id
    FROM "CarMaker" WHERE value = 'audi';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'a6',
         'A6',
         id
    FROM "CarMaker" WHERE value = 'audi';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'a8',
         'A8',
         id
    FROM "CarMaker" WHERE value = 'audi';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'arnage',
         'Arnage',
         id
    FROM "CarMaker" WHERE value = 'bentley';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'continental',
         'Continental Flying Spur',
         id
    FROM "CarMaker" WHERE value = 'bentley';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'chrysler300',
         '300',
         id
    FROM "CarMaker" WHERE value = 'chrysler';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sebring',
         'Sebring',
         id
    FROM "CarMaker" WHERE value = 'chrysler';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c1',
         'C1',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c2',
         'C2',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'berlingo',
         'Berlingo',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c15',
         'C15',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c3Picasso',
         'C3 Picasso',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c3Pluriel',
         'C3 Pluriel',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c4Aircross',
         'C4 Aircross',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c4Picasso',
         'C4 Picasso',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c8',
         'C8',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cCrosser',
         'C-Crosser',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cZero',
         'C-Zero',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ds3',
         'DS3',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ds4',
         'DS4',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ds5',
         'DS5',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'jumper',
         'Jumper',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'nemo',
         'Nemo',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'saxo',
         'Saxo',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'xsaraPicasso',
         'Xsara Picasso',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'zx',
         'ZX',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'evasionJumpy',
         'Evasion/Jumpy',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c3',
         'C3',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c4',
         'C4',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c5',
         'C5',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c6',
         'C6',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'xsara',
         'Xsara',
         id
    FROM "CarMaker" WHERE value = 'citroen';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'alero',
         'Alero',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'aveo',
         'Aveo',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'beretta',
         'Beretta',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'blazer',
         'Blazer',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'camaro',
         'Camaro',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'capriceCh',
         'Caprice',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'captiva',
         'Captiva',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cavalier',
         'Cavalier',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'celta',
         'Celta',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cheyenne',
         'Cheyenne',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cobaltCh',
         'Cobalt',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'colorado',
         'Colorado',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'wind',
         'Corsa Wind',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'corsica',
         'Corsica',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'corvette',
         'Corvette',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cruze',
         'Cruze',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'epicaCh',
         'Epica',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'evanda',
         'Evanda',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'express',
         'Express',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'orlando',
         'Orlando',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'hhr',
         'HHR',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'impalaSS',
         'Impala SS',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ipanemaGL',
         'Ipanema GL',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'jimmy',
         'Jimmy',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'lacetti',
         'Lacetti',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'lanosCh',
         'Lanos',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'impala',
         'Impala',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'lumina',
         'Lumina',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'malibu',
         'Malibu',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'metro',
         'Metro',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'monteCarlo',
         'Monte Carlo',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'monza',
         'Monza',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'niva',
         'NIVA',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'prism',
         'Prism',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 's10',
         'S-10',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'spark',
         'Spark',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ss',
         'SS',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'suburban',
         'Suburban',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'tacuma',
         'Tacuma',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'tahoe',
         'Tahoe',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'trackerConv',
         'Tracker Convertible',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'trackerHard',
         'Tracker Hardtop',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'trailBlazer',
         'Trail Blazer',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'Trans',
         'Trans Sport',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'venture',
         'Venture',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'viva',
         'Viva',
         id
    FROM "CarMaker" WHERE value = 'chevy';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'chairman',
         'Chairman',
         id
    FROM "CarMaker" WHERE value = 'daewoo';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'espero',
         'Espero',
         id
    FROM "CarMaker" WHERE value = 'daewoo';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'matiz',
         'Matiz',
         id
    FROM "CarMaker" WHERE value = 'daewoo';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'nexia',
         'Nexia',
         id
    FROM "CarMaker" WHERE value = 'daewoo';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'tosca',
         'Tosca',
         id
    FROM "CarMaker" WHERE value = 'daewoo';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'caliber',
         'Caliber',
         id
    FROM "CarMaker" WHERE value = 'dodge';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'albea',
         'Albea',
         id
    FROM "CarMaker" WHERE value = 'fiat';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'bravo',
         'Bravo',
         id
    FROM "CarMaker" WHERE value = 'fiat';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'panda',
         'Panda',
         id
    FROM "CarMaker" WHERE value = 'fiat';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'punto',
         'Punto',
         id
    FROM "CarMaker" WHERE value = 'fiat';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'epica',
         'Epica',
         id
    FROM "CarMaker" WHERE value = 'holden';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'statesman',
         'Statesman',
         id
    FROM "CarMaker" WHERE value = 'holden';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'caprice',
         'Caprice',
         id
    FROM "CarMaker" WHERE value = 'holden';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'accord',
         'Accord',
         id
    FROM "CarMaker" WHERE value = 'honda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'civic',
         'Civic',
         id
    FROM "CarMaker" WHERE value = 'honda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'fit',
         'Fit',
         id
    FROM "CarMaker" WHERE value = 'honda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'jazz',
         'Jazz',
         id
    FROM "CarMaker" WHERE value = 'honda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'legend',
         'Legend',
         id
    FROM "CarMaker" WHERE value = 'honda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'logo',
         'Logo',
         id
    FROM "CarMaker" WHERE value = 'honda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'i10',
         'i10',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'i20',
         'i20',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'i30',
         'i30',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'centennial',
         'Centennial',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'equus',
         'Equus',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cheryQQ',
         'Chery QQ (Sweet)',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'accent',
         'Accent',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'atos',
         'Atos',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'elantra',
         'Elantra',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'genesis',
         'Genesis',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'getz',
         'Getz',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'santaFe',
         'Santa Fe new',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'solaris',
         'Solaris',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sonata',
         'Sonata',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'xg',
         'XG',
         id
    FROM "CarMaker" WHERE value = 'hyundai';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sType',
         'S-Type',
         id
    FROM "CarMaker" WHERE value = 'jaguar';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'xf',
         'XF',
         id
    FROM "CarMaker" WHERE value = 'jaguar';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'xj',
         'XJ',
         id
    FROM "CarMaker" WHERE value = 'jaguar';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'xType',
         'X-Type',
         id
    FROM "CarMaker" WHERE value = 'jaguar';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ceed',
         'Cee''d',
         id
    FROM "CarMaker" WHERE value = 'kia';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cerato',
         'Cerato',
         id
    FROM "CarMaker" WHERE value = 'kia';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'magentis',
         'Magentis',
         id
    FROM "CarMaker" WHERE value = 'kia';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'opirus',
         'Opirus',
         id
    FROM "CarMaker" WHERE value = 'kia';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'picanto',
         'Picanto',
         id
    FROM "CarMaker" WHERE value = 'kia';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'rio',
         'Rio',
         id
    FROM "CarMaker" WHERE value = 'kia';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'spectra',
         'Spectra',
         id
    FROM "CarMaker" WHERE value = 'kia';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '2104',
         '2104',
         id
    FROM "CarMaker" WHERE value = 'lada';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '2107',
         '2107',
         id
    FROM "CarMaker" WHERE value = 'lada';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '2113Samara',
         '2113 (Samara)',
         id
    FROM "CarMaker" WHERE value = 'lada';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '2114Samara',
         '2114 (Samara)',
         id
    FROM "CarMaker" WHERE value = 'lada';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '2115Samara',
         '2115 (Samara)',
         id
    FROM "CarMaker" WHERE value = 'lada';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '2121Niva',
         '2121 (Niva)',
         id
    FROM "CarMaker" WHERE value = 'lada';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'granta',
         'Granta',
         id
    FROM "CarMaker" WHERE value = 'lada';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'kalina',
         'Kalina',
         id
    FROM "CarMaker" WHERE value = 'lada';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'priora',
         'Priora',
         id
    FROM "CarMaker" WHERE value = 'lada';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'largus',
         'Largus',
         id
    FROM "CarMaker" WHERE value = 'lada';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'thesis',
         'Thesis',
         id
    FROM "CarMaker" WHERE value = 'lancia';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'defender',
         'Defender',
         id
    FROM "CarMaker" WHERE value = 'land';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'discavery',
         'Discavery',
         id
    FROM "CarMaker" WHERE value = 'land';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'freelander',
         'Freelander',
         id
    FROM "CarMaker" WHERE value = 'land';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'range',
         'Range Rover',
         id
    FROM "CarMaker" WHERE value = 'land';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'rangeEvoque',
         'Range Rover Evoque',
         id
    FROM "CarMaker" WHERE value = 'land';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'rangeSport',
         'Range Rover Sport',
         id
    FROM "CarMaker" WHERE value = 'land';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'gs',
         'GS',
         id
    FROM "CarMaker" WHERE value = 'lexus';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'is',
         'IS',
         id
    FROM "CarMaker" WHERE value = 'lexus';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ls',
         'LS',
         id
    FROM "CarMaker" WHERE value = 'lexus';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'quattroporte',
         'Quattroporte',
         id
    FROM "CarMaker" WHERE value = 'maserati';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'mazda2',
         '2',
         id
    FROM "CarMaker" WHERE value = 'mazda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'mazda3',
         '3',
         id
    FROM "CarMaker" WHERE value = 'mazda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'mazda6',
         '6',
         id
    FROM "CarMaker" WHERE value = 'mazda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'demio',
         'Demio',
         id
    FROM "CarMaker" WHERE value = 'mazda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'aClass',
         'A',
         id
    FROM "CarMaker" WHERE value = 'mercedes';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'bClass',
         'B',
         id
    FROM "CarMaker" WHERE value = 'mercedes';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cClass',
         'C',
         id
    FROM "CarMaker" WHERE value = 'mercedes';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'eClass',
         'E',
         id
    FROM "CarMaker" WHERE value = 'mercedes';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sClass',
         'S',
         id
    FROM "CarMaker" WHERE value = 'mercedes';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cls',
         'CLS',
         id
    FROM "CarMaker" WHERE value = 'mercedes';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'colt',
         'Colt',
         id
    FROM "CarMaker" WHERE value = 'mitsubishi';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'galant',
         'Galant',
         id
    FROM "CarMaker" WHERE value = 'mitsubishi';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'lancer',
         'Lancer',
         id
    FROM "CarMaker" WHERE value = 'mitsubishi';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'proudia',
         'Proudia',
         id
    FROM "CarMaker" WHERE value = 'mitsubishi';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'almera',
         'Almera',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cedric',
         'Cedric',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cima',
         'Cima',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'laurel',
         'Laurel',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'maxima',
         'Maxima',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'micra',
         'Micra',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'murano',
         'Murano',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'note',
         'Note',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'president',
         'President',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'primera',
         'Primera',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'pathfinder',
         'Pathfinder',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'teana',
         'Teana',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'xTrail',
         'X-Trail',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'tiida',
         'Tiida',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ssangYong',
         'Ssang Yong',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'qashqai',
         'Qashqai',
         id
    FROM "CarMaker" WHERE value = 'nissan';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'agila',
         'Agila',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'antara',
         'Antara',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'astra',
         'Astra',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'astragtc',
         'Astra GTC',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'calibra',
         'Calibra',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'combo',
         'Combo',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'corsa',
         'Corsa',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'frontera',
         'Frontera',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'insignia',
         'Insignia',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'kadett',
         'Kadett',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'meriva',
         'Meriva',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'mokka',
         'Mokka',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'monterey',
         'Monterey',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'movano',
         'Movano',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'omega',
         'Omega',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'signum',
         'Signum',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sintra',
         'Sintra',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'tigra',
         'Tigra',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'vectra',
         'Vectra',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'vita',
         'Vita',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'vivaro',
         'Vivaro',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'zafira',
         'Zafira',
         id
    FROM "CarMaker" WHERE value = 'opel';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'allante',
         'Allante',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'bls',
         'BLS',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'brougham',
         'Brougham',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'catera',
         'Catera',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cts',
         'CTS',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ville',
         'DE Ville',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'dts',
         'DTS',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'eldorado',
         'Eldorado',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'escalade',
         'Escalade',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'fleetwood',
         'Fleetwood',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'lse',
         'LSE',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'seville',
         'Seville',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'srx',
         'SRX',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sts',
         'STS',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'xlr',
         'XLR',
         id
    FROM "CarMaker" WHERE value = 'cad';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'caddy',
         'Caddy',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'caravelle',
         'Caravelle',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'amarok',
         'Amarok',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'crafter',
         'Crafter',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 't5',
         'T5',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'tiguan',
         'Tiguan',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'polo',
         'Polo',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'touareg',
         'Touareg',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'passat',
         'Passat',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'passatCC',
         'Passat CC',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'jetta',
         'Jetta',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'multivan',
         'Multivan',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'golf',
         'Golf',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'golfPlus',
         'Golf Plus',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sharan',
         'Sharan',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'touran',
         'Touran',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'phaeton',
         'Phaeton',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'eos',
         'Eos',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'lupo',
         'Lupo',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'pointer',
         'Pointer',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'transporter',
         'Transporter',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'scirocco',
         'Scirocco',
         id
    FROM "CarMaker" WHERE value = 'vw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ford427',
         '427',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'aerostar',
         'Aerostar',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cMax',
         'Focus C-Max',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'aspire',
         'Aspire',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'bronco',
         'Bronco',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sMax',
         'S-Max',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cMaxII',
         'C-Max II',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'contour',
         'Contour',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cougar',
         'Cougar',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'kuga',
         'Kuga',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'crownVictoria',
         'Crown Victoria',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'econoline',
         'Econoline',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'escape',
         'Escape',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'escort',
         'Escort',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'escortCabrio',
         'Escort Cabrio',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'escortClassic',
         'Escort Classic',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'escortEstate',
         'Escort Estate',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'escortHatchback',
         'Escort Hatchback',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'escortTurnier',
         'Escort Turnier',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'escortZX2',
         'Escort ZX2',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'excursion',
         'Excursion',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'expedition',
         'Expedition',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'explorer',
         'Explorer',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'faction',
         'Faction',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'fairlane',
         'Fairlane',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'falconGT',
         'Falcon GT',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'fiesta',
         'Fiesta',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'focus',
         'Focus',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'fusion',
         'Fusion',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'galaxy',
         'Galaxy',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'fordGT',
         'Ford GT',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ikon',
         'Ikon',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ka',
         'Ka',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ltd',
         'LTD',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'maverick',
         'Maverick',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'modelU',
         'Model U',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'mondeo',
         'Mondeo',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'mustang',
         'Mustang',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'probe',
         'Probe',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'puma',
         'Puma',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ranger',
         'Ranger',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'scorpio',
         'Scorpio',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sierra',
         'Sierra',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'shelbyGR',
         'Shelby GR',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sportKa',
         'SportKa',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'streetKa',
         'StreetKa',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'taurus',
         'Taurus',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'thunderbird',
         'Thunderbird',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'tourenoConnect',
         'Tourneo Connect',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'transit',
         'Transit',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'windstar',
         'Windstar',
         id
    FROM "CarMaker" WHERE value = 'ford';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '1s',
         '1 series',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '3s',
         '3 series',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '5s',
         '5 series',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '6s',
         '6 series',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '7s',
         '7 series',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '8s',
         '8 series',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'm3',
         'M3',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'm5',
         'M5',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'x1',
         'X1',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'x3',
         'X3',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'x5',
         'X5',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'x6',
         'X6',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'xActivity',
         'xActivity',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'z1',
         'Z1',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'z3',
         'Z3',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'z4',
         'Z4',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'z8',
         'Z8',
         id
    FROM "CarMaker" WHERE value = 'bmw';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '107',
         '107',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '206',
         '206',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '106',
         '106',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '208',
         '208',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '309',
         '309',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '508',
         '508',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '807',
         '807',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '1007',
         '1007',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '3008',
         '3008',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '4007',
         '4007',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '4008',
         '4008',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '5008',
         '5008',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '206+',
         '206+',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'bipper',
         'Bipper',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'boxer',
         'Boxer',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'expert',
         'Expert',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ion',
         'Ion',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'partner',
         'Partner',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'rcz',
         'RCZ',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '207',
         '207',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '307',
         '307',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '308',
         '308',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '407',
         '407',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '408',
         '408',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '607',
         '607',
         id
    FROM "CarMaker" WHERE value = 'peugeot';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'clio',
         'Clio',
         id
    FROM "CarMaker" WHERE value = 'renault';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'laguna',
         'Laguna',
         id
    FROM "CarMaker" WHERE value = 'renault';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'logan',
         'Logan',
         id
    FROM "CarMaker" WHERE value = 'renault';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'megane',
         'Megane',
         id
    FROM "CarMaker" WHERE value = 'renault';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'symbol',
         'Symbol',
         id
    FROM "CarMaker" WHERE value = 'renault';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'twingo',
         'Twingo',
         id
    FROM "CarMaker" WHERE value = 'renault';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'velSatis',
         'Vel Satis',
         id
    FROM "CarMaker" WHERE value = 'renault';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'phantom',
         'Phantom',
         id
    FROM "CarMaker" WHERE value = 'rolls';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '111',
         '111',
         id
    FROM "CarMaker" WHERE value = 'rover';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '75',
         '75',
         id
    FROM "CarMaker" WHERE value = 'rover';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'saab93',
         '9-3',
         id
    FROM "CarMaker" WHERE value = 'saab';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'leon',
         'Leon',
         id
    FROM "CarMaker" WHERE value = 'seat';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'fabia',
         'Fabia',
         id
    FROM "CarMaker" WHERE value = 'skoda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'octavia',
         'Octavia',
         id
    FROM "CarMaker" WHERE value = 'skoda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'superb',
         'Superb',
         id
    FROM "CarMaker" WHERE value = 'skoda';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'fortwo',
         'Fortwo',
         id
    FROM "CarMaker" WHERE value = 'smart';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'impreza',
         'Impreza',
         id
    FROM "CarMaker" WHERE value = 'subaru';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'legacy',
         'Legacy',
         id
    FROM "CarMaker" WHERE value = 'subaru';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ignis',
         'Ignis',
         id
    FROM "CarMaker" WHERE value = 'suzuki';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'liana',
         'Liana',
         id
    FROM "CarMaker" WHERE value = 'suzuki';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'swift',
         'Swift',
         id
    FROM "CarMaker" WHERE value = 'suzuki';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'sx4',
         'SX4',
         id
    FROM "CarMaker" WHERE value = 'suzuki';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'vitara',
         'Vitara',
         id
    FROM "CarMaker" WHERE value = 'suzuki';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'allion',
         'Allion',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'auris',
         'Auris',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'avensis',
         'Avensis',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'aygo',
         'Aygo',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'camry',
         'Camry',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'corolla',
         'Corolla',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'ist',
         'Ist',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'landCruiser1',
         'Land Cruiser 1',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'landCruiser105',
         'Land Cruiser 105',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'mark',
         'Mark II',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'chaser',
         'Chaser',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'cresta',
         'Cresta',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'rav4',
         'RAV-4',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'vitz',
         'Vitz',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'yaris',
         'Yaris',
         id
    FROM "CarMaker" WHERE value = 'toyota';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'c30',
         'C30',
         id
    FROM "CarMaker" WHERE value = 'volvo';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 's40',
         'S40',
         id
    FROM "CarMaker" WHERE value = 'volvo';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 's60',
         'S60',
         id
    FROM "CarMaker" WHERE value = 'volvo';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 's80',
         'S80',
         id
    FROM "CarMaker" WHERE value = 'volvo';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'v50',
         'V50',
         id
    FROM "CarMaker" WHERE value = 'volvo';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'h1',
         'H1',
         id
    FROM "CarMaker" WHERE value = 'hum';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'h2',
         'H2',
         id
    FROM "CarMaker" WHERE value = 'hum';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'h3',
         'H3',
         id
    FROM "CarMaker" WHERE value = 'hum';
INSERT INTO "CarModel" (value, label, parent)
  SELECT 'shalanda',
         'Shalanda',
         id
    FROM "CarMaker" WHERE value = 'maz';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '5516',
         '5516',
         id
    FROM "CarMaker" WHERE value = 'maz';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '6430A9',
         '6430A9',
         id
    FROM "CarMaker" WHERE value = 'maz';
INSERT INTO "CarModel" (value, label, parent)
  SELECT '4370',
         '4370',
         id
    FROM "CarMaker" WHERE value = 'maz';
INSERT INTO "City" (value, label) VALUES
 ('Abinsk'
 ,'Абинск');
INSERT INTO "City" (value, label) VALUES
 ('Agryz'
 ,'Агрыз');
INSERT INTO "City" (value, label) VALUES
 ('Abakan'
 ,'Абакан');
INSERT INTO "City" (value, label) VALUES
 ('Bratsk'
 ,'Братск');
INSERT INTO "City" (value, label) VALUES
 ('UstIlimsk'
 ,'Усть-Илимск');
INSERT INTO "City" (value, label) VALUES
 ('Buluzuk'
 ,'Бузулук');
INSERT INTO "City" (value, label) VALUES
 ('Noyabrsk'
 ,'Ноябрьск');
INSERT INTO "City" (value, label) VALUES
 ('Adler'
 ,'Адлер');
INSERT INTO "City" (value, label) VALUES
 ('Adygejsk'
 ,'Адыгейск');
INSERT INTO "City" (value, label) VALUES
 ('Aznakaevo'
 ,'Азнакаево');
INSERT INTO "City" (value, label) VALUES
 ('Azov'
 ,'Азов');
INSERT INTO "City" (value, label) VALUES
 ('Alapaevsk'
 ,'Алапаевск');
INSERT INTO "City" (value, label) VALUES
 ('Aleksandrov'
 ,'Александров');
INSERT INTO "City" (value, label) VALUES
 ('Aleksin'
 ,'Алексин');
INSERT INTO "City" (value, label) VALUES
 ('Almaty'
 ,'Алматы');
INSERT INTO "City" (value, label) VALUES
 ('Al''met''evsk'
 ,'Альметьевск');
INSERT INTO "City" (value, label) VALUES
 ('Anapa'
 ,'Анапа');
INSERT INTO "City" (value, label) VALUES
 ('Angarsk'
 ,'Ангарск');
INSERT INTO "City" (value, label) VALUES
 ('Anzhero-Sudzhensk'
 ,'Анжеро-Судженск');
INSERT INTO "City" (value, label) VALUES
 ('Apatity'
 ,'Апатиты');
INSERT INTO "City" (value, label) VALUES
 ('Apejsk'
 ,'Апейск');
INSERT INTO "City" (value, label) VALUES
 ('Aprelevka'
 ,'Апрелевка');
INSERT INTO "City" (value, label) VALUES
 ('Aramil'''
 ,'Арамиль');
INSERT INTO "City" (value, label) VALUES
 ('Aretemovskij'
 ,'Аретемовский');
INSERT INTO "City" (value, label) VALUES
 ('Arzamas'
 ,'Арзамас');
INSERT INTO "City" (value, label) VALUES
 ('Armavir'
 ,'Армавир');
INSERT INTO "City" (value, label) VALUES
 ('Arsk'
 ,'Арск');
INSERT INTO "City" (value, label) VALUES
 ('Artem'
 ,'Артем');
INSERT INTO "City" (value, label) VALUES
 ('Arkhangel''sk'
 ,'Архангельск');
INSERT INTO "City" (value, label) VALUES
 ('Asbest'
 ,'Асбест');
INSERT INTO "City" (value, label) VALUES
 ('Asino'
 ,'Асино');
INSERT INTO "City" (value, label) VALUES
 ('Astrakhan'''
 ,'Астрахань');
INSERT INTO "City" (value, label) VALUES
 ('Atkarsk'
 ,'Аткарск');
INSERT INTO "City" (value, label) VALUES
 ('Achinsk'
 ,'Ачинск');
INSERT INTO "City" (value, label) VALUES
 ('Bajkal''sk'
 ,'Байкальск');
INSERT INTO "City" (value, label) VALUES
 ('Balakhna'
 ,'Балахна');
INSERT INTO "City" (value, label) VALUES
 ('Borisoglebsk'
 ,'Борисоглебск');
INSERT INTO "City" (value, label) VALUES
 ('Balashikha'
 ,'Балашиха');
INSERT INTO "City" (value, label) VALUES
 ('Baltijsk'
 ,'Балтийск');
INSERT INTO "City" (value, label) VALUES
 ('Barnaul'
 ,'Барнаул');
INSERT INTO "City" (value, label) VALUES
 ('Batajsk'
 ,'Батайск');
INSERT INTO "City" (value, label) VALUES
 ('Belaya Kalitva'
 ,'Белая Калитва');
INSERT INTO "City" (value, label) VALUES
 ('Belgorod'
 ,'Белгород');
INSERT INTO "City" (value, label) VALUES
 ('Belogorsk'
 ,'Белогорск');
INSERT INTO "City" (value, label) VALUES
 ('Belorechensk'
 ,'Белореченск');
INSERT INTO "City" (value, label) VALUES
 ('Berdsk'
 ,'Бердск');
INSERT INTO "City" (value, label) VALUES
 ('Berezniki'
 ,'Березники');
INSERT INTO "City" (value, label) VALUES
 ('Berezovskij'
 ,'Березовский');
INSERT INTO "City" (value, label) VALUES
 ('Bijsk'
 ,'Бийск');
INSERT INTO "City" (value, label) VALUES
 ('Birsk'
 ,'Бирск');
INSERT INTO "City" (value, label) VALUES
 ('Blagoveschensk'
 ,'Благовещенск');
INSERT INTO "City" (value, label) VALUES
 ('Bogdanovich'
 ,'Богданович');
INSERT INTO "City" (value, label) VALUES
 ('Bogorodsk'
 ,'Богородск');
INSERT INTO "City" (value, label) VALUES
 ('Bolkhov'
 ,'Болхов');
INSERT INTO "City" (value, label) VALUES
 ('Bol''shoj Kamen'''
 ,'Большой Камень');
INSERT INTO "City" (value, label) VALUES
 ('Bor'
 ,'Бор');
INSERT INTO "City" (value, label) VALUES
 ('Bronnitsy'
 ,'Бронницы');
INSERT INTO "City" (value, label) VALUES
 ('Bryansk'
 ,'Брянск');
INSERT INTO "City" (value, label) VALUES
 ('Bugul''ma'
 ,'Бугульма');
INSERT INTO "City" (value, label) VALUES
 ('Budenovsk'
 ,'Буденовск');
INSERT INTO "City" (value, label) VALUES
 ('Buinsk'
 ,'Буинск');
INSERT INTO "City" (value, label) VALUES
 ('Valujki'
 ,'Валуйки');
INSERT INTO "City" (value, label) VALUES
 ('Velikij Novgorod'
 ,'Великий Новгород');
INSERT INTO "City" (value, label) VALUES
 ('Velikij Ustyug'
 ,'Великий Устюг');
INSERT INTO "City" (value, label) VALUES
 ('Venev'
 ,'Венев');
INSERT INTO "City" (value, label) VALUES
 ('Vereschagino'
 ,'Верещагино');
INSERT INTO "City" (value, label) VALUES
 ('Verkhneural''sk'
 ,'Верхнеуральск');
INSERT INTO "City" (value, label) VALUES
 ('Verkhnij Tagil'
 ,'Верхний Тагил');
INSERT INTO "City" (value, label) VALUES
 ('Verkhnyaya Pyshma'
 ,'Верхняя Пышма');
INSERT INTO "City" (value, label) VALUES
 ('Vidnoe'
 ,'Видное');
INSERT INTO "City" (value, label) VALUES
 ('Vichuga'
 ,'Вичуга');
INSERT INTO "City" (value, label) VALUES
 ('Vladivostok'
 ,'Владивосток');
INSERT INTO "City" (value, label) VALUES
 ('Vladikavkaz'
 ,'Владикавказ');
INSERT INTO "City" (value, label) VALUES
 ('Vladimir'
 ,'Владимир');
INSERT INTO "City" (value, label) VALUES
 ('Voznesen''e'
 ,'Вознесенье');
INSERT INTO "City" (value, label) VALUES
 ('Volgograd'
 ,'Волгоград');
INSERT INTO "City" (value, label) VALUES
 ('Volzhskij'
 ,'Волжский');
INSERT INTO "City" (value, label) VALUES
 ('Vologda'
 ,'Вологда');
INSERT INTO "City" (value, label) VALUES
 ('Volokolamsk'
 ,'Волоколамск');
INSERT INTO "City" (value, label) VALUES
 ('Volkhov'
 ,'Волхов');
INSERT INTO "City" (value, label) VALUES
 ('Voronezh'
 ,'Воронеж');
INSERT INTO "City" (value, label) VALUES
 ('Vorsma'
 ,'Ворсма');
INSERT INTO "City" (value, label) VALUES
 ('Voskresensk'
 ,'Воскресенск');
INSERT INTO "City" (value, label) VALUES
 ('Votkinsk'
 ,'Воткинск');
INSERT INTO "City" (value, label) VALUES
 ('Vsevolozhsk'
 ,'Всеволожск');
INSERT INTO "City" (value, label) VALUES
 ('Vyborg'
 ,'Выборг');
INSERT INTO "City" (value, label) VALUES
 ('Vyshnyj Volochek'
 ,'Вышный Волочек');
INSERT INTO "City" (value, label) VALUES
 ('Vyazniki'
 ,'Вязники');
INSERT INTO "City" (value, label) VALUES
 ('Gavrilov YAm'
 ,'Гаврилов Ям');
INSERT INTO "City" (value, label) VALUES
 ('Galich'
 ,'Галич');
INSERT INTO "City" (value, label) VALUES
 ('Gatchina'
 ,'Гатчина');
INSERT INTO "City" (value, label) VALUES
 ('Gvardejsk'
 ,'Гвардейск');
INSERT INTO "City" (value, label) VALUES
 ('Gelendzhik'
 ,'Геленджик');
INSERT INTO "City" (value, label) VALUES
 ('Georgievsk'
 ,'Георгиевск');
INSERT INTO "City" (value, label) VALUES
 ('Gorodetsk'
 ,'Городецк');
INSERT INTO "City" (value, label) VALUES
 ('Gremyachinsk'
 ,'Гремячинск');
INSERT INTO "City" (value, label) VALUES
 ('gryazovo'
 ,'грязово');
INSERT INTO "City" (value, label) VALUES
 ('Gubakha'
 ,'Губаха');
INSERT INTO "City" (value, label) VALUES
 ('Gubkin'
 ,'Губкин');
INSERT INTO "City" (value, label) VALUES
 ('Gukovo'
 ,'Гуково');
INSERT INTO "City" (value, label) VALUES
 ('Gus'' KHrustal''nyj'
 ,'Гусь Хрустальный');
INSERT INTO "City" (value, label) VALUES
 ('Danilov'
 ,'Данилов');
INSERT INTO "City" (value, label) VALUES
 ('Dedovsk'
 ,'Дедовск');
INSERT INTO "City" (value, label) VALUES
 ('Dzerzhinsk'
 ,'Дзержинск');
INSERT INTO "City" (value, label) VALUES
 ('Dimitrovgrad'
 ,'Димитровград');
INSERT INTO "City" (value, label) VALUES
 ('Divnogorsk'
 ,'Дивногорск');
INSERT INTO "City" (value, label) VALUES
 ('Dmitrov'
 ,'Дмитров');
INSERT INTO "City" (value, label) VALUES
 ('Dmitrovgrad'
 ,'Дмитровград');
INSERT INTO "City" (value, label) VALUES
 ('Dno'
 ,'Дно');
INSERT INTO "City" (value, label) VALUES
 ('Dobryanka'
 ,'Добрянка');
INSERT INTO "City" (value, label) VALUES
 ('Dolgoprudnyj'
 ,'Долгопрудный');
INSERT INTO "City" (value, label) VALUES
 ('Domodedovo'
 ,'Домодедово');
INSERT INTO "City" (value, label) VALUES
 ('Dubna'
 ,'Дубна');
INSERT INTO "City" (value, label) VALUES
 ('Dyat''kovo'
 ,'Дятьково');
INSERT INTO "City" (value, label) VALUES
 ('Egor''evsk'
 ,'Егорьевск');
INSERT INTO "City" (value, label) VALUES
 ('Ejsk'
 ,'Ейск');
INSERT INTO "City" (value, label) VALUES
 ('Ekaterinburg'
 ,'Екатеринбург');
INSERT INTO "City" (value, label) VALUES
 ('Elabuga'
 ,'Елабуга');
INSERT INTO "City" (value, label) VALUES
 ('Elets'
 ,'Елец');
INSERT INTO "City" (value, label) VALUES
 ('Essentuki'
 ,'Ессентуки');
INSERT INTO "City" (value, label) VALUES
 ('ZHeznogorsk'
 ,'Жезногорск');
INSERT INTO "City" (value, label) VALUES
 ('ZHeleznovodsk'
 ,'Железноводск');
INSERT INTO "City" (value, label) VALUES
 ('ZHeleznodorozhnyj'
 ,'Железнодорожный');
INSERT INTO "City" (value, label) VALUES
 ('ZHigulevsk'
 ,'Жигулевск');
INSERT INTO "City" (value, label) VALUES
 ('ZHukovskij'
 ,'Жуковский');
INSERT INTO "City" (value, label) VALUES
 ('Zavolzhsk'
 ,'Заволжск');
INSERT INTO "City" (value, label) VALUES
 ('Zainsk'
 ,'Заинск');
INSERT INTO "City" (value, label) VALUES
 ('Zaozernyj'
 ,'Заозерный');
INSERT INTO "City" (value, label) VALUES
 ('Zapolyar''e'
 ,'Заполярье');
INSERT INTO "City" (value, label) VALUES
 ('Zarajsk'
 ,'Зарайск');
INSERT INTO "City" (value, label) VALUES
 ('Zarechnyj'
 ,'Заречный');
INSERT INTO "City" (value, label) VALUES
 ('Zarinsk'
 ,'Заринск');
INSERT INTO "City" (value, label) VALUES
 ('Zvenigorod'
 ,'Звенигород');
INSERT INTO "City" (value, label) VALUES
 ('Zelenogorsk_kr'
 ,'Зеленогорск (Красноярский край)');
INSERT INTO "City" (value, label) VALUES
 ('Zelenogorsk_spb'
 ,'Зеленогорск (Ленинградская область)');
INSERT INTO "City" (value, label) VALUES
 ('Zelenodol''sk'
 ,'Зеленодольск');
INSERT INTO "City" (value, label) VALUES
 ('Zelenokumsk'
 ,'Зеленокумск');
INSERT INTO "City" (value, label) VALUES
 ('Zlatoust'
 ,'Златоуст');
INSERT INTO "City" (value, label) VALUES
 ('Znamensk'
 ,'Знаменск');
INSERT INTO "City" (value, label) VALUES
 ('Ivanovo'
 ,'Иваново');
INSERT INTO "City" (value, label) VALUES
 ('Izhevsk'
 ,'Ижевск');
INSERT INTO "City" (value, label) VALUES
 ('Ilovlya'
 ,'Иловля');
INSERT INTO "City" (value, label) VALUES
 ('Izmail'
 ,'Измаил');
INSERT INTO "City" (value, label) VALUES
 ('Joshkar-Ola'
 ,'Йошкар-Ола');
INSERT INTO "City" (value, label) VALUES
 ('Irkutsk'
 ,'Иркутск');
INSERT INTO "City" (value, label) VALUES
 ('Istra'
 ,'Истра');
INSERT INTO "City" (value, label) VALUES
 ('Ishimbaj'
 ,'Ишимбай');
INSERT INTO "City" (value, label) VALUES
 ('Kadnikov'
 ,'Кадников');
INSERT INTO "City" (value, label) VALUES
 ('Kazan'''
 ,'Казань');
INSERT INTO "City" (value, label) VALUES
 ('Kalach-na-Donu'
 ,'Калач-на-Дону');
INSERT INTO "City" (value, label) VALUES
 ('Kaliningrad'
 ,'Калининград');
INSERT INTO "City" (value, label) VALUES
 ('Kaluga'
 ,'Калуга');
INSERT INTO "City" (value, label) VALUES
 ('Kalyazin'
 ,'Калязин');
INSERT INTO "City" (value, label) VALUES
 ('Kamensk-Ural''skij'
 ,'Каменск-Уральский');
INSERT INTO "City" (value, label) VALUES
 ('Kamensk-SHakhtinskij'
 ,'Каменск-Шахтинский');
INSERT INTO "City" (value, label) VALUES
 ('Kamyshlov'
 ,'Камышлов');
INSERT INTO "City" (value, label) VALUES
 ('Kanash'
 ,'Канаш');
INSERT INTO "City" (value, label) VALUES
 ('Kandalaksha'
 ,'Кандалакша');
INSERT INTO "City" (value, label) VALUES
 ('Karabanovo'
 ,'Карабаново');
INSERT INTO "City" (value, label) VALUES
 ('Karabash'
 ,'Карабаш');
INSERT INTO "City" (value, label) VALUES
 ('Karachev'
 ,'Карачев');
INSERT INTO "City" (value, label) VALUES
 ('Kasimov'
 ,'Касимов');
INSERT INTO "City" (value, label) VALUES
 ('Kemerovo'
 ,'Кемерово');
INSERT INTO "City" (value, label) VALUES
 ('Kimovsk'
 ,'Кимовск');
INSERT INTO "City" (value, label) VALUES
 ('Kingesepp'
 ,'Кингесепп');
INSERT INTO "City" (value, label) VALUES
 ('Kinel'''
 ,'Кинель');
INSERT INTO "City" (value, label) VALUES
 ('Kineshma'
 ,'Кинешма');
INSERT INTO "City" (value, label) VALUES
 ('Kirzhach'
 ,'Киржач');
INSERT INTO "City" (value, label) VALUES
 ('Kirillov'
 ,'Кириллов');
INSERT INTO "City" (value, label) VALUES
 ('Kirov'
 ,'Киров');
INSERT INTO "City" (value, label) VALUES
 ('Kirovograd'
 ,'Кировоград');
INSERT INTO "City" (value, label) VALUES
 ('Kirovsk'
 ,'Кировск');
INSERT INTO "City" (value, label) VALUES
 ('Kirsanov'
 ,'Кирсанов');
INSERT INTO "City" (value, label) VALUES
 ('Kisilevsk'
 ,'Кисилевск');
INSERT INTO "City" (value, label) VALUES
 ('Kislovodsk'
 ,'Кисловодск');
INSERT INTO "City" (value, label) VALUES
 ('Klimovsk'
 ,'Климовск');
INSERT INTO "City" (value, label) VALUES
 ('Klin'
 ,'Клин');
INSERT INTO "City" (value, label) VALUES
 ('Kovrov'
 ,'Ковров');
INSERT INTO "City" (value, label) VALUES
 ('Koz''modem''yansk'
 ,'Козьмодемьянск');
INSERT INTO "City" (value, label) VALUES
 ('Kolomna'
 ,'Коломна');
INSERT INTO "City" (value, label) VALUES
 ('Kol''chugino'
 ,'Кольчугино');
INSERT INTO "City" (value, label) VALUES
 ('Komsomol''sk'
 ,'Комсомольск');
INSERT INTO "City" (value, label) VALUES
 ('Komsomol''skNaAmure'
 ,'Комсомольск-на-Амуре');
INSERT INTO "City" (value, label) VALUES
 ('Kondopoga'
 ,'Кондопога');
INSERT INTO "City" (value, label) VALUES
 ('Konstantinovsk'
 ,'Константиновск');
INSERT INTO "City" (value, label) VALUES
 ('Kopejsk'
 ,'Копейск');
INSERT INTO "City" (value, label) VALUES
 ('Korenovsk'
 ,'Кореновск');
INSERT INTO "City" (value, label) VALUES
 ('Korolev'
 ,'Королев');
INSERT INTO "City" (value, label) VALUES
 ('Korocha'
 ,'Короча');
INSERT INTO "City" (value, label) VALUES
 ('Koryazhma'
 ,'Коряжма');
INSERT INTO "City" (value, label) VALUES
 ('Kostroma'
 ,'Кострома');
INSERT INTO "City" (value, label) VALUES
 ('Kotel''nich'
 ,'Котельнич');
INSERT INTO "City" (value, label) VALUES
 ('Kotlas'
 ,'Котлас');
INSERT INTO "City" (value, label) VALUES
 ('Kotovsk'
 ,'Котовск');
INSERT INTO "City" (value, label) VALUES
 ('Kochenovsk'
 ,'Коченовск');
INSERT INTO "City" (value, label) VALUES
 ('Krasnoarmejsk'
 ,'Красноармейск');
INSERT INTO "City" (value, label) VALUES
 ('Krasnoznamensk'
 ,'Краснознаменск');
INSERT INTO "City" (value, label) VALUES
 ('Krasnovishersk'
 ,'Красновишерск');
INSERT INTO "City" (value, label) VALUES
 ('Krasnogorsk'
 ,'Красногорск');
INSERT INTO "City" (value, label) VALUES
 ('Krasnodar'
 ,'Краснодар');
INSERT INTO "City" (value, label) VALUES
 ('Krasnokamsk'
 ,'Краснокамск');
INSERT INTO "City" (value, label) VALUES
 ('Krasnoyarsk'
 ,'Красноярск');
INSERT INTO "City" (value, label) VALUES
 ('Kropotkin'
 ,'Кропоткин');
INSERT INTO "City" (value, label) VALUES
 ('Krymsk'
 ,'Крымск');
INSERT INTO "City" (value, label) VALUES
 ('Kuznetsk'
 ,'Кузнецк');
INSERT INTO "City" (value, label) VALUES
 ('Kumertau'
 ,'Кумертау');
INSERT INTO "City" (value, label) VALUES
 ('Kungur'
 ,'Кунгур');
INSERT INTO "City" (value, label) VALUES
 ('Kurgan'
 ,'Курган');
INSERT INTO "City" (value, label) VALUES
 ('Kursk'
 ,'Курск');
INSERT INTO "City" (value, label) VALUES
 ('Kurtamysh'
 ,'Куртамыш');
INSERT INTO "City" (value, label) VALUES
 ('Kutuluk'
 ,'Кутулук');
INSERT INTO "City" (value, label) VALUES
 ('Kyshtym'
 ,'Кыштым');
INSERT INTO "City" (value, label) VALUES
 ('Langepas'
 ,'Лангепас');
INSERT INTO "City" (value, label) VALUES
 ('Lebedyan'''
 ,'Лебедянь');
INSERT INTO "City" (value, label) VALUES
 ('Leninsk'
 ,'Ленинск');
INSERT INTO "City" (value, label) VALUES
 ('Leninsk-Kuznetskij'
 ,'Ленинск-Кузнецкий');
INSERT INTO "City" (value, label) VALUES
 ('Lermontov'
 ,'Лермонтов');
INSERT INTO "City" (value, label) VALUES
 ('Lesnoe'
 ,'Лесное');
INSERT INTO "City" (value, label) VALUES
 ('Liga'
 ,'Лига');
INSERT INTO "City" (value, label) VALUES
 ('Lipetsk'
 ,'Липецк');
INSERT INTO "City" (value, label) VALUES
 ('Liski'
 ,'Лиски');
INSERT INTO "City" (value, label) VALUES
 ('Lobnya'
 ,'Лобня');
INSERT INTO "City" (value, label) VALUES
 ('Lys''va'
 ,'Лысьва');
INSERT INTO "City" (value, label) VALUES
 ('Lyubertsy'
 ,'Люберцы');
INSERT INTO "City" (value, label) VALUES
 ('Magnitogorsk'
 ,'Магнитогорск');
INSERT INTO "City" (value, label) VALUES
 ('Majkop'
 ,'Майкоп');
INSERT INTO "City" (value, label) VALUES
 ('Malmyzh'
 ,'Малмыж');
INSERT INTO "City" (value, label) VALUES
 ('Maloarkhangel''sk'
 ,'Малоархангельск');
INSERT INTO "City" (value, label) VALUES
 ('Maloyaroslavets'
 ,'Малоярославец');
INSERT INTO "City" (value, label) VALUES
 ('Mamadysh'
 ,'Мамадыш');
INSERT INTO "City" (value, label) VALUES
 ('Marks'
 ,'Маркс');
INSERT INTO "City" (value, label) VALUES
 ('Megion'
 ,'Мегион');
INSERT INTO "City" (value, label) VALUES
 ('Mahachkala'
 ,'Махачкала');
INSERT INTO "City" (value, label) VALUES
 ('Mednogorsk'
 ,'Медногорск');
INSERT INTO "City" (value, label) VALUES
 ('Mezhdurechensk'
 ,'Междуреченск');
INSERT INTO "City" (value, label) VALUES
 ('Mezelinsk'
 ,'Мезелинск');
INSERT INTO "City" (value, label) VALUES
 ('Melenki'
 ,'Меленки');
INSERT INTO "City" (value, label) VALUES
 ('Meliuz'
 ,'Мелиуз');
INSERT INTO "City" (value, label) VALUES
 ('Mendeleevsk'
 ,'Менделеевск');
INSERT INTO "City" (value, label) VALUES
 ('Miass'
 ,'Миасс');
INSERT INTO "City" (value, label) VALUES
 ('Mineral''nye Vody'
 ,'Минеральные Воды');
INSERT INTO "City" (value, label) VALUES
 ('Mikhajlov'
 ,'Михайлов');
INSERT INTO "City" (value, label) VALUES
 ('Mikhajlovsk'
 ,'Михайловск');
INSERT INTO "City" (value, label) VALUES
 ('Michurinsk'
 ,'Мичуринск');
INSERT INTO "City" (value, label) VALUES
 ('Mozhajsk'
 ,'Можайск');
INSERT INTO "City" (value, label) VALUES
 ('Mozhga'
 ,'Можга');
INSERT INTO "City" (value, label) VALUES
 ('Monchegorsk'
 ,'Мончегорск');
INSERT INTO "City" (value, label) VALUES
 ('Morshansk'
 ,'Моршанск');
INSERT INTO "City" (value, label) VALUES
 ('Moskva'
 ,'Москва');
INSERT INTO "City" (value, label) VALUES
 ('Murmansk'
 ,'Мурманск');
INSERT INTO "City" (value, label) VALUES
 ('Murom'
 ,'Муром');
INSERT INTO "City" (value, label) VALUES
 ('Mtsensk'
 ,'Мценск');
INSERT INTO "City" (value, label) VALUES
 ('Mytischi'
 ,'Мытищи');
INSERT INTO "City" (value, label) VALUES
 ('Myshkin'
 ,'Мышкин');
INSERT INTO "City" (value, label) VALUES
 ('Nizhnij Novgorod'
 ,'Нижний Новгород');
INSERT INTO "City" (value, label) VALUES
 ('Naberezhnye CHelny'
 ,'Набережные Челны');
INSERT INTO "City" (value, label) VALUES
 ('Nal''chik'
 ,'Нальчик');
INSERT INTO "City" (value, label) VALUES
 ('Narimanov'
 ,'Нариманов');
INSERT INTO "City" (value, label) VALUES
 ('Naro-Fominsk'
 ,'Наро-Фоминск');
INSERT INTO "City" (value, label) VALUES
 ('Nakhodka'
 ,'Находка');
INSERT INTO "City" (value, label) VALUES
 ('Nevinnomysk'
 ,'Невинномыск');
INSERT INTO "City" (value, label) VALUES
 ('Nerekhta'
 ,'Нерехта');
INSERT INTO "City" (value, label) VALUES
 ('Neftegorsk'
 ,'Нефтегорск');
INSERT INTO "City" (value, label) VALUES
 ('Nefteugansk'
 ,'Нефтеюганск');
INSERT INTO "City" (value, label) VALUES
 ('Neftekamsk'
 ,'Нефтекамск');
INSERT INTO "City" (value, label) VALUES
 ('Nizhnevartovsk'
 ,'Нижневартовск');
INSERT INTO "City" (value, label) VALUES
 ('Nizhnekamsk'
 ,'Нижнекамск');
INSERT INTO "City" (value, label) VALUES
 ('Nizhnij Tagil'
 ,'Нижний Тагил');
INSERT INTO "City" (value, label) VALUES
 ('Novaya Ladoga'
 ,'Новая Ладога');
INSERT INTO "City" (value, label) VALUES
 ('Novodvinsk'
 ,'Новодвинск');
INSERT INTO "City" (value, label) VALUES
 ('Novokuznetsk'
 ,'Новокузнецк');
INSERT INTO "City" (value, label) VALUES
 ('Novokujbyshevsk'
 ,'Новокуйбышевск');
INSERT INTO "City" (value, label) VALUES
 ('Novomichurinsk'
 ,'Новомичуринск');
INSERT INTO "City" (value, label) VALUES
 ('Novomoskovsk'
 ,'Новомосковск');
INSERT INTO "City" (value, label) VALUES
 ('Novopavlovsk'
 ,'Новопавловск');
INSERT INTO "City" (value, label) VALUES
 ('Novorossijsk'
 ,'Новороссийск');
INSERT INTO "City" (value, label) VALUES
 ('Novosibirsk'
 ,'Новосибирск');
INSERT INTO "City" (value, label) VALUES
 ('Novoul''yanovsk'
 ,'Новоульяновск');
INSERT INTO "City" (value, label) VALUES
 ('Novoural''sk'
 ,'Новоуральск');
INSERT INTO "City" (value, label) VALUES
 ('Novocheboksarsk'
 ,'Новочебоксарск');
INSERT INTO "City" (value, label) VALUES
 ('Novocherkassk'
 ,'Новочеркасск');
INSERT INTO "City" (value, label) VALUES
 ('Novoshakhtinsk'
 ,'Новошахтинск');
INSERT INTO "City" (value, label) VALUES
 ('Novyj Oskol'
 ,'Новый Оскол');
INSERT INTO "City" (value, label) VALUES
 ('NoviyYrengoy'
 ,'Новый Уренгой');
INSERT INTO "City" (value, label) VALUES
 ('Noginsk'
 ,'Ногинск');
INSERT INTO "City" (value, label) VALUES
 ('Nurlat'
 ,'Нурлат');
INSERT INTO "City" (value, label) VALUES
 ('Nytva'
 ,'Нытва');
INSERT INTO "City" (value, label) VALUES
 ('Obninsk'
 ,'Обнинск');
INSERT INTO "City" (value, label) VALUES
 ('Odintsovo'
 ,'Одинцово');
INSERT INTO "City" (value, label) VALUES
 ('Ozersk'
 ,'Озерск');
INSERT INTO "City" (value, label) VALUES
 ('Ozery'
 ,'Озеры');
INSERT INTO "City" (value, label) VALUES
 ('Oktyabr''skij'
 ,'Октябрьский');
INSERT INTO "City" (value, label) VALUES
 ('Olenegorsk'
 ,'Оленегорск');
INSERT INTO "City" (value, label) VALUES
 ('Olonets'
 ,'Олонец');
INSERT INTO "City" (value, label) VALUES
 ('Omsk'
 ,'Омск');
INSERT INTO "City" (value, label) VALUES
 ('Orel'
 ,'Орел');
INSERT INTO "City" (value, label) VALUES
 ('Orenburg'
 ,'Оренбург');
INSERT INTO "City" (value, label) VALUES
 ('Orekhovo-Zuevo'
 ,'Орехово-Зуево');
INSERT INTO "City" (value, label) VALUES
 ('Orsk'
 ,'Орск');
INSERT INTO "City" (value, label) VALUES
 ('Osa'
 ,'Оса');
INSERT INTO "City" (value, label) VALUES
 ('Ostrov'
 ,'Остров');
INSERT INTO "City" (value, label) VALUES
 ('Otradnoe'
 ,'Отрадное');
INSERT INTO "City" (value, label) VALUES
 ('Otradnyj'
 ,'Отрадный');
INSERT INTO "City" (value, label) VALUES
 ('Okhansk'
 ,'Оханск');
INSERT INTO "City" (value, label) VALUES
 ('Partizansk'
 ,'Партизанск');
INSERT INTO "City" (value, label) VALUES
 ('PolyarnieZory'
 ,'Полярные Зори');
INSERT INTO "City" (value, label) VALUES
 ('Penza'
 ,'Пенза');
INSERT INTO "City" (value, label) VALUES
 ('Pervoural''sk'
 ,'Первоуральск');
INSERT INTO "City" (value, label) VALUES
 ('Peresvet'
 ,'Пересвет');
INSERT INTO "City" (value, label) VALUES
 ('Perm'''
 ,'Пермь');
INSERT INTO "City" (value, label) VALUES
 ('Petrovsk'
 ,'Петровск');
INSERT INTO "City" (value, label) VALUES
 ('Petrozavodsk'
 ,'Петрозаводск');
INSERT INTO "City" (value, label) VALUES
 ('Petropavlovsk'
 ,'Петропавловск-Камчатский');
INSERT INTO "City" (value, label) VALUES
 ('Petushki'
 ,'Петушки');
INSERT INTO "City" (value, label) VALUES
 ('Pionerskij'
 ,'Пионерский');
INSERT INTO "City" (value, label) VALUES
 ('Ples'
 ,'Плес');
INSERT INTO "City" (value, label) VALUES
 ('Podol''sk'
 ,'Подольск');
INSERT INTO "City" (value, label) VALUES
 ('Pokrov'
 ,'Покров');
INSERT INTO "City" (value, label) VALUES
 ('Polevskoj'
 ,'Полевской');
INSERT INTO "City" (value, label) VALUES
 ('Polessk'
 ,'Полесск');
INSERT INTO "City" (value, label) VALUES
 ('Polyarnyj Zori'
 ,'Полярный Зори');
INSERT INTO "City" (value, label) VALUES
 ('Pravdinskij'
 ,'Правдинский');
INSERT INTO "City" (value, label) VALUES
 ('Predivinsk'
 ,'Предивинск');
INSERT INTO "City" (value, label) VALUES
 ('Privolzhsk'
 ,'Приволжск');
INSERT INTO "City" (value, label) VALUES
 ('Primorsk'
 ,'Приморск');
INSERT INTO "City" (value, label) VALUES
 ('Prokop''evsk'
 ,'Прокопьевск');
INSERT INTO "City" (value, label) VALUES
 ('Pskov'
 ,'Псков');
INSERT INTO "City" (value, label) VALUES
 ('Pushkin'
 ,'Пушкин');
INSERT INTO "City" (value, label) VALUES
 ('Pushkino'
 ,'Пушкино');
INSERT INTO "City" (value, label) VALUES
 ('Pyt''-YAkh'
 ,'Пыть-Ях');
INSERT INTO "City" (value, label) VALUES
 ('Pyatigorsk'
 ,'Пятигорск');
INSERT INTO "City" (value, label) VALUES
 ('Ramenskoe'
 ,'Раменское');
INSERT INTO "City" (value, label) VALUES
 ('Revda'
 ,'Ревда');
INSERT INTO "City" (value, label) VALUES
 ('Reutov'
 ,'Реутов');
INSERT INTO "City" (value, label) VALUES
 ('Rzhev'
 ,'Ржев');
INSERT INTO "City" (value, label) VALUES
 ('Roslavl'''
 ,'Рославль');
INSERT INTO "City" (value, label) VALUES
 ('Rostov'
 ,'Ростов');
INSERT INTO "City" (value, label) VALUES
 ('Rostov-na-Donu'
 ,'Ростов-на-Дону');
INSERT INTO "City" (value, label) VALUES
 ('Roshal'''
 ,'Рошаль');
INSERT INTO "City" (value, label) VALUES
 ('Ruza'
 ,'Руза');
INSERT INTO "City" (value, label) VALUES
 ('Rybinsk'
 ,'Рыбинск');
INSERT INTO "City" (value, label) VALUES
 ('Rybnoe'
 ,'Рыбное');
INSERT INTO "City" (value, label) VALUES
 ('Ryl''sk'
 ,'Рыльск');
INSERT INTO "City" (value, label) VALUES
 ('Ryazhsk'
 ,'Ряжск');
INSERT INTO "City" (value, label) VALUES
 ('Ryazan'''
 ,'Рязань');
INSERT INTO "City" (value, label) VALUES
 ('Salavat'
 ,'Салават');
INSERT INTO "City" (value, label) VALUES
 ('Samara'
 ,'Самара');
INSERT INTO "City" (value, label) VALUES
 ('Sankt-Peterburg'
 ,'Санкт-Петербург');
INSERT INTO "City" (value, label) VALUES
 ('Saransk'
 ,'Саранск');
INSERT INTO "City" (value, label) VALUES
 ('Sarapul'
 ,'Сарапул');
INSERT INTO "City" (value, label) VALUES
 ('Saratov'
 ,'Саратов');
INSERT INTO "City" (value, label) VALUES
 ('Svetlograd'
 ,'Светлоград');
INSERT INTO "City" (value, label) VALUES
 ('Svetlyj YAr'
 ,'Светлый Яр');
INSERT INTO "City" (value, label) VALUES
 ('Severodvinsk'
 ,'Северодвинск');
INSERT INTO "City" (value, label) VALUES
 ('Severomorsk'
 ,'Североморск');
INSERT INTO "City" (value, label) VALUES
 ('Seversk'
 ,'Северск');
INSERT INTO "City" (value, label) VALUES
 ('Semenov'
 ,'Семенов');
INSERT INTO "City" (value, label) VALUES
 ('Sergiev Posad'
 ,'Сергиев Посад');
INSERT INTO "City" (value, label) VALUES
 ('Serdobsk'
 ,'Сердобск');
INSERT INTO "City" (value, label) VALUES
 ('Serpukhov'
 ,'Серпухов');
INSERT INTO "City" (value, label) VALUES
 ('Sestroretsk'
 ,'Сестрорецк');
INSERT INTO "City" (value, label) VALUES
 ('Skopin'
 ,'Скопин');
INSERT INTO "City" (value, label) VALUES
 ('Slavsk'
 ,'Славск');
INSERT INTO "City" (value, label) VALUES
 ('Slavyansk-na-Kubani'
 ,'Славянск-на-Кубани');
INSERT INTO "City" (value, label) VALUES
 ('Smolensk'
 ,'Смоленск');
INSERT INTO "City" (value, label) VALUES
 ('Snezhinsk'
 ,'Снежинск');
INSERT INTO "City" (value, label) VALUES
 ('Sovetsk'
 ,'Советск');
INSERT INTO "City" (value, label) VALUES
 ('Solikamsk'
 ,'Соликамск');
INSERT INTO "City" (value, label) VALUES
 ('Solnechnogorsk'
 ,'Солнечногорск');
INSERT INTO "City" (value, label) VALUES
 ('Sol''-Iletsk'
 ,'Соль-Илецк');
INSERT INTO "City" (value, label) VALUES
 ('Sosnovoborsk'
 ,'Сосновоборск');
INSERT INTO "City" (value, label) VALUES
 ('Sochi'
 ,'Сочи');
INSERT INTO "City" (value, label) VALUES
 ('Spask Ryazanskij'
 ,'Спаск Рязанский');
INSERT INTO "City" (value, label) VALUES
 ('Spas-Klepiki'
 ,'Спас-Клепики');
INSERT INTO "City" (value, label) VALUES
 ('Sredneural''sk'
 ,'Среднеуральск');
INSERT INTO "City" (value, label) VALUES
 ('Stavropol'''
 ,'Ставрополь');
INSERT INTO "City" (value, label) VALUES
 ('Staryj Oskol'
 ,'Старый Оскол');
INSERT INTO "City" (value, label) VALUES
 ('Sterlitamak'
 ,'Стерлитамак');
INSERT INTO "City" (value, label) VALUES
 ('Stupino'
 ,'Ступино');
INSERT INTO "City" (value, label) VALUES
 ('Suzdal'''
 ,'Суздаль');
INSERT INTO "City" (value, label) VALUES
 ('Surgut'
 ,'Сургут');
INSERT INTO "City" (value, label) VALUES
 ('Sukhoj Log'
 ,'Сухой Лог');
INSERT INTO "City" (value, label) VALUES
 ('Syzran'''
 ,'Сызрань');
INSERT INTO "City" (value, label) VALUES
 ('Syktyvkar'
 ,'Сыктывкар');
INSERT INTO "City" (value, label) VALUES
 ('Taganrog'
 ,'Таганрог');
INSERT INTO "City" (value, label) VALUES
 ('Taldom'
 ,'Талдом');
INSERT INTO "City" (value, label) VALUES
 ('Tambov'
 ,'Тамбов');
INSERT INTO "City" (value, label) VALUES
 ('Tver'''
 ,'Тверь');
INSERT INTO "City" (value, label) VALUES
 ('Temryuk'
 ,'Темрюк');
INSERT INTO "City" (value, label) VALUES
 ('Tetyushi'
 ,'Тетюши');
INSERT INTO "City" (value, label) VALUES
 ('Timashevsk'
 ,'Тимашевск');
INSERT INTO "City" (value, label) VALUES
 ('Tol''yatti'
 ,'Тольятти');
INSERT INTO "City" (value, label) VALUES
 ('Tomsk'
 ,'Томск');
INSERT INTO "City" (value, label) VALUES
 ('Tobol`sk'
 ,'Тобольск');
INSERT INTO "City" (value, label) VALUES
 ('Torzhok'
 ,'Торжок');
INSERT INTO "City" (value, label) VALUES
 ('Tosno'
 ,'Тосно');
INSERT INTO "City" (value, label) VALUES
 ('Troitsk'
 ,'Троицк');
INSERT INTO "City" (value, label) VALUES
 ('Trubchevsk'
 ,'Трубчевск');
INSERT INTO "City" (value, label) VALUES
 ('Tuapse'
 ,'Туапсе');
INSERT INTO "City" (value, label) VALUES
 ('Tujmazy'
 ,'Туймазы');
INSERT INTO "City" (value, label) VALUES
 ('Tula'
 ,'Тула');
INSERT INTO "City" (value, label) VALUES
 ('Tutaev'
 ,'Тутаев');
INSERT INTO "City" (value, label) VALUES
 ('Tyumen'''
 ,'Тюмень');
INSERT INTO "City" (value, label) VALUES
 ('Uglich'
 ,'Углич');
INSERT INTO "City" (value, label) VALUES
 ('Ul''yanovsk'
 ,'Ульяновск');
INSERT INTO "City" (value, label) VALUES
 ('Usol''e Sibirskoe'
 ,'Усолье Сибирское');
INSERT INTO "City" (value, label) VALUES
 ('Ussurijsk'
 ,'Уссурийск');
INSERT INTO "City" (value, label) VALUES
 ('Ufa'
 ,'Уфа');
INSERT INTO "City" (value, label) VALUES
 ('Uhta'
 ,'Ухта');
INSERT INTO "City" (value, label) VALUES
 ('Uyar'
 ,'Уяр');
INSERT INTO "City" (value, label) VALUES
 ('UlanUde'
 ,'Улан-Удэ');
INSERT INTO "City" (value, label) VALUES
 ('KHabarovsk'
 ,'Хабаровск');
INSERT INTO "City" (value, label) VALUES
 ('KHimki'
 ,'Химки');
INSERT INTO "City" (value, label) VALUES
 ('TSivil''sk'
 ,'Цивильск');
INSERT INTO "City" (value, label) VALUES
 ('CHajkovskij'
 ,'Чайковский');
INSERT INTO "City" (value, label) VALUES
 ('CHapaevsk'
 ,'Чапаевск');
INSERT INTO "City" (value, label) VALUES
 ('CHebarkul'''
 ,'Чебаркуль');
INSERT INTO "City" (value, label) VALUES
 ('CHeboksary'
 ,'Чебоксары');
INSERT INTO "City" (value, label) VALUES
 ('CHelyabinsk'
 ,'Челябинск');
INSERT INTO "City" (value, label) VALUES
 ('CHerdyn'''
 ,'Чердынь');
INSERT INTO "City" (value, label) VALUES
 ('CHerepovets'
 ,'Череповец');
INSERT INTO "City" (value, label) VALUES
 ('CHerkessk'
 ,'Черкесск');
INSERT INTO "City" (value, label) VALUES
 ('CHermoz'
 ,'Чермоз');
INSERT INTO "City" (value, label) VALUES
 ('CHernyakhovsk'
 ,'Черняховск');
INSERT INTO "City" (value, label) VALUES
 ('CHekhov'
 ,'Чехов');
INSERT INTO "City" (value, label) VALUES
 ('CHistopol'''
 ,'Чистополь');
INSERT INTO "City" (value, label) VALUES
 ('CHita'
 ,'Чита');
INSERT INTO "City" (value, label) VALUES
 ('CHudovo'
 ,'Чудово');
INSERT INTO "City" (value, label) VALUES
 ('CHusovoj'
 ,'Чусовой');
INSERT INTO "City" (value, label) VALUES
 ('SHatura'
 ,'Шатура');
INSERT INTO "City" (value, label) VALUES
 ('SHakhty'
 ,'Шахты');
INSERT INTO "City" (value, label) VALUES
 ('SHelekhov'
 ,'Шелехов');
INSERT INTO "City" (value, label) VALUES
 ('SHumerlya'
 ,'Шумерля');
INSERT INTO "City" (value, label) VALUES
 ('SCHebekino'
 ,'Щебекино');
INSERT INTO "City" (value, label) VALUES
 ('SCHelkovo'
 ,'Щелково');
INSERT INTO "City" (value, label) VALUES
 ('Elektrostal'''
 ,'Электросталь');
INSERT INTO "City" (value, label) VALUES
 ('Elektrougli'
 ,'Электроугли');
INSERT INTO "City" (value, label) VALUES
 ('Engel''s'
 ,'Энгельс');
INSERT INTO "City" (value, label) VALUES
 ('YUrga'
 ,'Юрга');
INSERT INTO "City" (value, label) VALUES
 ('YAkutsk'
 ,'Якутск');
INSERT INTO "City" (value, label) VALUES
 ('YAnaul'
 ,'Янаул');
INSERT INTO "City" (value, label) VALUES
 ('YAransk'
 ,'Яранск');
INSERT INTO "City" (value, label) VALUES
 ('YAroslavl'''
 ,'Ярославль');
INSERT INTO "City" (value, label) VALUES
 ('YAsnogorsk'
 ,'Ясногорск');
