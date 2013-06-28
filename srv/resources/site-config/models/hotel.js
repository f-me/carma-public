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
        "label": "Адрес кейса"
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
        "parguy",
        "account"
      ],
      "name": "caseAddress"
    },
    {
      "meta": {
        "label": "Гостиница"
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
      "meta": {
        "label": "Расчетная стоимость"
      },
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
        "label": "Срок, на который предоставлена гостиница (дней)"
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
        "invisible": true
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
      "name": "service_tarifOptions"
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
    }
  ],
  "applications": [
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
      "meta": {
        "label": "Адрес кейса"
      },
      "targets": [
        "caseAddress_address"
      ]
    },
    {
      "meta": {
        "label": "Гостиница"
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
        "caseAddress_address",
        "caseAddress_coords",
        "caseAddress_city",
        "caseAddress_comment"
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
        "programman"
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
        "programman"
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
        "programman"
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
    }
  ],
  "canDelete": true,
  "canUpdate": true,
  "canRead": true,
  "canCreate": true,
  "title": "Гостиница",
  "name": "hotel"
}
