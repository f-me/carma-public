{
  "fields": [
    {
      "meta": {
        "invisible": true
      },
      "canWrite": true,
      "canRead": true,
      "name": "parentId"
    },
    {
      "meta": {
        "readonly": true,
        "label": "Дата создания услуги"
      },
      "type": "datetime",
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy",
        "account"
      ],
      "name": "createTime"
    },
    {
      "meta": {
        "label": "Тип оплаты",
        "bounded": true,
        "dictionaryName": "PaymentTypes"
      },
      "type": "dictionary",
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy",
        "account"
      ],
      "name": "payType"
    },
    {
      "groupName": "payment",
      "name": "payment"
    },
    {
      "groupName": "times",
      "name": "times"
    },
    {
      "meta": {
        "infoText": "falsecall",
        "label": "Ложный вызов",
        "bounded": true,
        "dictionaryName": "FalseStatuses"
      },
      "type": "dictionary",
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "name": "falseCall"
    },
    {
      "meta": {
        "label": "Причина отказа клиента",
        "dictionaryName": "ClientCancelReason"
      },
      "type": "dictionary",
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "name": "clientCancelReason"
    },
    {
      "meta": {
        "invisible": true
      },
      "canWrite": [],
      "canRead": [],
      "name": "falseCallPercent"
    },
    {
      "groupName": "bill",
      "name": "bill"
    },
    {
      "meta": {
        "label": "Дилер"
      },
      "groupName": "partner",
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "name": "towDealer"
    },
    {
      "meta": {
        "label": "Адрес (куда доставить)"
      },
      "groupName": "address",
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "name": "rentAddress"
    },
    {
      "meta": {
        "transform": "uppercase",
        "label": "VIN подменного автомобиля"
      },
      "canWrite": [
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "name": "vinRent"
    },
    {
      "meta": {
        "label": "Класс автомобиля",
        "bounded": true,
        "dictionaryName": "CarClasses"
      },
      "type": "dictionary",
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "name": "carClass"
    },
    {
      "meta": {
        "label": "Партнёр"
      },
      "groupName": "partner",
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy",
        "account"
      ],
      "name": "contractor"
    },
    {
      "groupName": "countedCost",
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy",
        "account"
      ],
      "name": "cost"
    },
    {
      "meta": {
        "readonly": true,
        "label": "Предельная стоимость"
      },
      "canWrite": [],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "name": "marginalCost"
    },
    {
      "meta": {
        "label": "Срок, на который предоставлен автомобиль (дней)"
      },
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "name": "providedFor"
    },
    {
      "type": "dictionary",
      "meta": {
        "label": "Марка, предоставленного автомобиля",
        "bounded": true,
        "dictionaryName": "CarMakers"
      },
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "name": "rentedMake"
    },
    {
      "type": "dictionary",
      "meta": {
        "label": "Модель, предоставленного автомобиля",
        "bounded": true,
        "dictionaryParent": "rentedMake",
        "dictionaryName": "CarModels"
      },
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "name": "rentedModel"
    },
    {
      "meta": {
        "label": "Приоритетная услуга",
        "dictionaryName": "UrgentServiceReason",
        "bounded": false
      },
      "type": "dictionary",
      "name": "urgentService"
    },
    {
      "meta": {
        "dictionaryName": "ServiceStatuses",
        "bounded": true,
        "label": "Статус услуги"
      },
      "type": "dictionary",
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy",
        "account"
      ],
      "name": "status"
    },
    {
      "meta": {
        "label": "Клиент доволен",
        "dictionaryName": "Satisfaction"
      },
      "type": "dictionary",
      "canWrite": [
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "name": "clientSatisfied"
    },
    {
      "meta": {
        "label": "Номер заказ-наряда"
      },
      "canWrite": [
        "head",
        "admin",
        "psaanalyst"
      ],
      "canRead": [
        "head",
        "admin",
        "psaanalyst"
      ],
      "name": "orderNumber"
    },
    {
      "type": "checkbox",
      "meta": {
        "label": "Оплата"
      },
      "canWrite": [
        "accManager"
      ],
      "canRead": [
        "manager",
        "accManager"
      ],
      "name": "paid"
    },
    {
      "type": "checkbox",
      "meta": {
        "label": "Скан загружен"
      },
      "canWrite": [
        "accManager"
      ],
      "canRead": [
        "manager",
        "accManager"
      ],
      "name": "scan"
    },
    {
      "type": "checkbox",
      "meta": {
        "label": "Оригинал получен"
      },
      "canWrite": [
        "accManager"
      ],
      "canRead": [
        "manager",
        "accManager"
      ],
      "name": "original"
    },
    {
      "meta": {
        "label": "Гарантийный случай"
      },
      "type": "checkbox",
      "canWrite": [
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "name": "warrantyCase"
    },
    {
      "meta": {
        "label": "Прикрепленные файлы"
      },
      "type": "reference",
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy",
        "account"
      ],
      "name": "files"
    },
    {
      "meta": {
        "readonly": true,
        "invisible": true
      },
      "canWrite": [],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy",
        "account"
      ],
      "name": "assignedTo"
    },
    {
      "meta": {
        "readonly": true,
        "invisible": true
      },
      "canWrite": [],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy",
        "account"
      ],
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
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy",
        "account"
      ],
      "targets": [
        "cost_countedCost"
      ]
    },
    {
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy",
        "account"
      ],
      "targets": [
        "urgentService",
        "cost_counted",
        "cost_serviceTarifOptions"
      ]
    },
    {
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy",
        "account"
      ],
      "targets": [
        "rentAddress_address",
        "rentAddress_coords",
        "rentAddress_city",
        "rentAddress_comment"
      ]
    },
    {
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "targets": [
        "towDealer_partner",
        "towDealer_partnerTable",
        "towDealer_address"
      ]
    },
    {
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy",
        "account"
      ],
      "targets": [
        "contractor_partner",
        "contractor_partnerId",
        "contractor_partnerTable",
        "contractor_partnerCancel",
        "contractor_address"
      ]
    },
    {
      "canWrite": [
        "back",
        "front",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "back",
        "front",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy",
        "account"
      ],
      "targets": [
        "payment_partnerCost",
        "payment_costTranscript"
      ]
    },
    {
      "canWrite": [
        "parguy"
      ],
      "canRead": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy",
        "account"
      ],
      "targets": [
        "payment_calculatedCost",
        "payment_overcosted"
      ]
    },
    {
      "canRead": [
        "back",
        "front",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman"
      ],
      "targets": [
        "payment_limitedCost"
      ]
    },
    {
      "canWrite": [
        "back",
        "front",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "targets": [
        "payment_paidByRUAMC",
        "payment_paidByClient"
      ]
    },
    {
      "canWrite": [
        "parguy"
      ],
      "canRead": [
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "targets": [
        "bill_billNumber",
        "bill_billingCost",
        "bill_billingDate"
      ]
    },
    {
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "targets": [
        "times_expectedServiceStart"
      ]
    },
    {
      "canWrite": [
        "back",
        "front",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "back",
        "front",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "targets": [
        "times_factServiceStart",
        "times_expectedServiceEnd",
        "times_factServiceEnd",
        "times_expectedServiceFinancialClosure",
        "times_factServiceFinancialClosure",
        "times_expectedServiceClosure",
        "times_factServiceClosure",
        "times_repairEndDate"
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
