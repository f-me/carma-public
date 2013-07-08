{
  "fields": [
    {
      "meta": {
        "invisible": true
      },
      "type": null,
      "groupName": null,
      "name": "parentId"
    },
    {
      "meta": {
        "label": "Дата создания услуги",
        "readonly": true
      },
      "type": "datetime",
      "groupName": null,
      "name": "createTime"
    },
    {
      "meta": {
        "dictionaryName": "PaymentTypes",
        "bounded": true,
        "label": "Тип оплаты"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "payType"
    },
    {
      "meta": null,
      "type": null,
      "groupName": "payment",
      "name": "payment"
    },
    {
      "meta": null,
      "type": null,
      "groupName": "times",
      "name": "times"
    },
    {
      "meta": {
        "dictionaryName": "FalseStatuses",
        "bounded": true,
        "label": "Ложный вызов",
        "infoText": "falsecall"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "falseCall"
    },
    {
      "meta": {
        "dictionaryName": "ClientCancelReason",
        "label": "Причина отказа клиента"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "clientCancelReason"
    },
    {
      "meta": {
        "invisible": true
      },
      "type": null,
      "groupName": null,
      "name": "falseCallPercent"
    },
    {
      "meta": null,
      "type": null,
      "groupName": "bill",
      "name": "bill"
    },
    {
      "meta": {
        "label": "Название партнёра"
      },
      "type": null,
      "groupName": "partner",
      "name": "contractor"
    },
    {
      "meta": {
        "label": "Контакт 1"
      },
      "type": null,
      "groupName": null,
      "name": "contact1"
    },
    {
      "meta": {
        "label": "Телефон 1",
        "regexp": "phone"
      },
      "type": "phone",
      "groupName": null,
      "name": "contactPhone1"
    },
    {
      "meta": {
        "label": "Что сказать 1"
      },
      "type": "textarea",
      "groupName": null,
      "name": "whatToSay1"
    },
    {
      "meta": {
        "label": "Контакт 2"
      },
      "type": null,
      "groupName": null,
      "name": "contact2"
    },
    {
      "meta": {
        "label": "Телефон 2",
        "regexp": "phone"
      },
      "type": "phone",
      "groupName": null,
      "name": "contactPhone2"
    },
    {
      "meta": {
        "label": "Что сказать 2"
      },
      "type": "textarea",
      "groupName": null,
      "name": "whatToSay2"
    },
    {
      "meta": {
        "label": "Контакт 3"
      },
      "type": null,
      "groupName": null,
      "name": "contact3"
    },
    {
      "meta": {
        "label": "Телефон 3",
        "regexp": "phone"
      },
      "type": "phone",
      "groupName": null,
      "name": "contactPhone3"
    },
    {
      "meta": {
        "label": "Что сказать 3"
      },
      "type": "textarea",
      "groupName": null,
      "name": "whatToSay3"
    },
    {
      "meta": {
        "label": "Оплата"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "paid"
    },
    {
      "meta": {
        "label": "Скан загружен"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "scan"
    },
    {
      "meta": {
        "label": "Оригинал получен"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "original"
    },
    {
      "meta": {
        "bounded": false,
        "dictionaryName": "UrgentServiceReason",
        "label": "Приоритетная услуга"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "urgentService"
    },
    {
      "meta": {
        "label": "Статус услуги",
        "bounded": true,
        "dictionaryName": "ServiceStatuses"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "status"
    },
    {
      "meta": {
        "dictionaryName": "Satisfaction",
        "label": "Клиент доволен"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "clientSatisfied"
    },
    {
      "meta": {
        "label": "Гарантийный случай"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "warrantyCase"
    },
    {
      "meta": {
        "label": "Прикрепленные файлы",
        "widget": "inline-uploader"
      },
      "type": "reference",
      "groupName": null,
      "name": "files"
    },
    {
      "meta": {
        "invisible": true,
        "readonly": true
      },
      "type": null,
      "groupName": null,
      "name": "assignedTo"
    },
    {
      "meta": {
        "invisible": true,
        "readonly": true
      },
      "type": null,
      "groupName": null,
      "name": "falseCallPercent"
    }
  ],
  "applications": [
    {
      "meta": {
        "regexp": "datetime"
      },
      "targets": [
        "expectedServiceStart",
        "factServiceStart",
        "expectedServiceEnd",
        "factServiceEnd",
        "expectedServiceFinancialClosure",
        "factServiceFinancialClosure",
        "expectedDealerInfo",
        "factDealerInfo",
        "expectedServiceClosure",
        "factServiceClosure"
      ]
    }
  ],
  "canDelete": true,
  "canUpdate": true,
  "canRead": true,
  "canCreate": true,
  "title": "Информирование о происшествии",
  "name": "information"
}
