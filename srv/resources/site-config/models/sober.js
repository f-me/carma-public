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
        "label": "Где забрать"
      },
      "type": null,
      "groupName": "address",
      "name": "fromAddress"
    },
    {
      "meta": {
        "label": "Куда доставить"
      },
      "type": null,
      "groupName": "address",
      "name": "toAddress"
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
        "label": "Предельная стоимость",
        "readonly": true
      },
      "type": null,
      "groupName": null,
      "name": "marginalCost"
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
        "label": "Мультидрайв"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "multidrive"
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
        "invisible": true
      },
      "type": "reference",
      "groupName": null,
      "name": "service_tarifOptions"
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
        "label": "Где забрать"
      },
      "targets": [
        "fromAddress_address"
      ]
    },
    {
      "meta": {
        "label": "Куда доставить"
      },
      "targets": [
        "toAddress_address"
      ]
    },
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
  "title": "Трезвый водитель",
  "name": "sober"
}
