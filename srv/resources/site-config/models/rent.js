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
        "label": "Дилер"
      },
      "type": null,
      "groupName": "partner",
      "name": "towDealer"
    },
    {
      "meta": {
        "label": "Адрес (куда доставить)"
      },
      "type": null,
      "groupName": "address",
      "name": "rentAddress"
    },
    {
      "meta": {
        "label": "VIN подменного автомобиля",
        "transform": "uppercase"
      },
      "type": null,
      "groupName": null,
      "name": "vinRent"
    },
    {
      "meta": {
        "dictionaryName": "CarClasses",
        "bounded": true,
        "label": "Класс автомобиля"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "carClass"
    },
    {
      "meta": {
        "label": "Партнёр"
      },
      "type": null,
      "groupName": "partner",
      "name": "contractor"
    },
    {
      "meta": null,
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
        "label": "Срок, на который предоставлен автомобиль (дней)"
      },
      "type": null,
      "groupName": null,
      "name": "providedFor"
    },
    {
      "meta": {
        "dictionaryName": "CarMakers",
        "bounded": true,
        "label": "Марка, предоставленного автомобиля"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "rentedMake"
    },
    {
      "meta": {
        "dictionaryName": "CarModels",
        "dictionaryParent": "rentedMake",
        "bounded": true,
        "label": "Модель, предоставленного автомобиля"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "rentedModel"
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
        "label": "Номер заказ-наряда"
      },
      "type": null,
      "groupName": null,
      "name": "orderNumber"
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
        "label": "Гарантийный случай"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "warrantyCase"
    },
    {
      "meta": {
        "label": "Прикрепленные файлы"
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
        "label": "Куда доставить"
      },
      "targets": [
        "rentAddress_address"
      ]
    },
    {
      "meta": {
        "label": "Дилер"
      },
      "targets": [
        "towDealer_partner"
      ]
    },
    {
      "meta": {
        "label": "Партнёр"
      },
      "targets": [
        "contractor_partner"
      ]
    },
    {
      "meta": {
        "mainToo": true
      },
      "targets": [
        "times_expectedServiceStart"
      ]
    }
  ],
  "canDelete": true,
  "canUpdate": true,
  "canRead": true,
  "canCreate": true,
  "title": "Подменный автомобиль",
  "name": "rent"
}
