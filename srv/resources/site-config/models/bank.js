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
        "label": "Ложный вызов",
        "bounded": true,
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
        "label": "Тип запроса",
        "bounded": true,
        "dictionaryName": "RequestType"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "requestType"
    },
    {
      "meta": {
        "label": "Описание проблемы"
      },
      "type": "textarea",
      "groupName": null,
      "name": "whatToSay1"
    },
    {
      "meta": {
        "label": "Тип действия",
        "bounded": true,
        "dictionaryName": "Activity"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "activity"
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
        "label": "Расчетная стоимость"
      },
      "type": null,
      "groupName": "countedCost",
      "name": "cost"
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
    }
  ],
  "applications": [
    {
      "meta": {
        "label": "Партнёр"
      },
      "targets": [
        "contractor_partner"
      ]
    }
  ],
  "canDelete": true,
  "canUpdate": true,
  "canRead": true,
  "canCreate": true,
  "title": "Банковская поддержка",
  "name": "bank"
}
