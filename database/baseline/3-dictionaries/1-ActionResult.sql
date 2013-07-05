CREATE TABLE "ActionResult"
  (id     SERIAL PRIMARY KEY
  ,value  text
  ,label  text NOT NULL
  ,parent int4 REFERENCES "ActionName" ON DELETE SET NULL
  ,UNIQUE (label, parent)
  );
CREATE UNIQUE INDEX ON "ActionResult" (label) WHERE parent IS NULL;
INSERT INTO Dictionary (name, parent)
  SELECT 'ActionResult', id
    FROM Dictionary WHERE name = 'ActionName';

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

