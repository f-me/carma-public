{
  "fields": [
    {
      "meta": {
        "readonly": true,
        "label": "Дата звонка"
      },
      "type": "datetime",
      "indexCollate": true,
      "index": true,
      "canWrite": [
        "head",
        "supervisor",
        "director",
        "analyst",
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
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy",
        "account"
      ],
      "name": "callDate"
    },
    {
      "meta": {
        "label": "Дата звонка"
      },
      "type": "datetime",
      "indexCollate": true,
      "index": true,
      "canWrite": [
        "vwfake"
      ],
      "canRead": [
        "vwfake"
      ],
      "name": "vwcreatedate"
    },
    {
      "meta": {
        "readonly": true,
        "label": "Сотрудник РАМК"
      },
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
      "name": "callTaker"
    },
    {
      "meta": {
        "infoText": "comment",
        "required": true,
        "label": "Что случилось",
        "dictionaryName": "Wazzup"
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
      "name": "comment"
    },
    {
      "meta": {
        "infoText": "system",
        "required": true,
        "label": "Система",
        "dictionaryName": "Diagnosis1"
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
      "name": "diagnosis1"
    },
    {
      "meta": {
        "infoText": "detail",
        "label": "Узел/деталь",
        "dictionaryParent": "diagnosis1",
        "dictionaryName": "Diagnosis2"
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
        "programman"
      ],
      "name": "diagnosis2"
    },
    {
      "meta": {
        "infoText": "diagnosis3",
        "label": "Описание причины неисправности",
        "dictionaryName": "Diagnosis3"
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
        "account",
        "admin",
        "programman"
      ],
      "name": "diagnosis3"
    },
    {
      "meta": {
        "infoText": "recomendation",
        "label": "Рекомендация",
        "dictionaryName": "Diagnosis4"
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
        "account",
        "admin",
        "programman"
      ],
      "name": "diagnosis4"
    },
    {
      "meta": {
        "required": true,
        "label": "Клиент"
      },
      "groupName": "carContact",
      "name": "contact"
    },
    {
      "meta": {
        "infoText": "program",
        "targetCategory": "program",
        "bounded": true,
        "required": true,
        "label": "Программа",
        "dictionaryName": "Programs"
      },
      "type": "dictionary",
      "index": true,
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
      "name": "program"
    },
    {
      "groupName": "car",
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
      "name": "car"
    },
    {
      "groupName": "cardNumber",
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
      "name": "cardNumber"
    },
    {
      "meta": {
        "infoText": "vinChecked",
        "bounded": true,
        "required": true,
        "label": "Участие в программе",
        "dictionaryName": "VINChecked"
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
      "name": "vinChecked"
    },
    {
      "meta": {
        "infoText": "city",
        "bounded": true,
        "required": true,
        "label": "Город",
        "dictionaryName": "DealerCities"
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
      "name": "city"
    },
    {
      "groupName": "address",
      "name": "caseAddress"
    },
    {
      "meta": {
        "infoText": "temperature",
        "label": "Температура"
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
      "name": "temperature"
    },
    {
      "meta": {
        "label": "Дата починки"
      },
      "type": "datetime",
      "canWrite": [
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
      "name": "repair"
    },
    {
      "meta": {
        "label": "Номер согласования"
      },
      "canWrite": [
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
      "name": "accord"
    },
    {
      "type": "textarea",
      "meta": {
        "infoText": "dealerCause",
        "label": "Неисправность со слов дилера/партнёра"
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
      "name": "dealerCause"
    },
    {
      "meta": {
        "label": "Статус кейса",
        "dictionaryName": "CaseStatuses",
        "bounded": true,
        "required": true
      },
      "type": "dictionary",
      "canWrite": [
        "psaanalyst",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin"
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
        "psaanalyst"
      ],
      "name": "caseStatus"
    },
    {
      "meta": {
        "label": "Требуется выгрузка в PSA"
      },
      "canWrite": [
        "head",
        "supervisor",
        "director",
        "admin",
        "psaanalyst"
      ],
      "canRead": [
        "head",
        "supervisor",
        "director",
        "admin",
        "psaanalyst"
      ],
      "type": "checkbox",
      "name": "psaExportNeeded"
    },
    {
      "meta": {
        "label": "Выгружен в PSA",
        "readonly": true
      },
      "canWrite": [
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
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "psaanalyst"
      ],
      "type": "checkbox",
      "name": "psaExported"
    },
    {
      "type": "textarea",
      "meta": {
        "infoText": "claim",
        "label": "Претензия / Благодарность"
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
      "name": "claim"
    },
    {
      "type": "textarea",
      "meta": {
        "infoText": "betaComment",
        "label": "Комментарии"
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
      "name": "betaComment"
    },
    {
      "meta": {
        "label": "Услуги"
      },
      "type": "reference",
      "canWrite": true,
      "canRead": true,
      "name": "services"
    },
    {
      "meta": {
        "invisible": true,
        "label": "Действия"
      },
      "type": "reference",
      "canWrite": true,
      "canRead": true,
      "name": "actions"
    },
    {
      "meta": {
        "invisible": true
      },
      "type": "json",
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
      "name": "comments"
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
    }
  ],
  "applications": [
    {
      "meta": {
        "targetCoords": "caseAddress_coords",
        "targetMap": "caseAddress_map"
      },
      "targets": [
        "caseAddress_address"
      ]
    },
    {
      "meta": {
        "targetAddr": "caseAddress_address",
        "targetMap": "caseAddress_map"
      },
      "targets": [
        "caseAddress_coords"
      ]
    },
    {
      "meta": {
        "cityField": "city",
        "targetCoords": "caseAddress_coords",
        "targetAddr": "caseAddress_address"
      },
      "targets": [
        "caseAddress_map"
      ]
    },
    {
      "meta": {
        "infoText": "caseAddress",
        "label": "Адрес места поломки"
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
        "account",
        "admin",
        "programman",
        "parguy",
        "account"
      ],
      "targets": [
        "caseAddress_address"
      ]
    },
    {
      "meta": {
        "label": "Звонящий"
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
        "parguy",
        "account"
      ],
      "targets": [
        "contact_name"
      ]
    },
    {
      "meta": {
        "infoText": "cardnum",
        "label": "Карта участника"
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
      "targets": [
        "cardNumber_cardNumber"
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
        "caseAddress_map",
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
        "contact_email",
        "contact_phone1",
        "contact_phone2",
        "contact_phone3",
        "contact_phone4",
        "contact_ownerName",
        "contact_contactOwner",
        "contact_ownerEmail",
        "contact_ownerPhone1",
        "contact_ownerPhone2",
        "contact_ownerPhone3",
        "contact_ownerPhone4"
      ]
    },
    {
      "meta": {
        "dictionaryParent": "car_make"
      },
      "targets": [
        "car_model"
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
        "car_vin",
        "car_seller",
        "car_make",
        "car_model",
        "car_plateNum",
        "car_color",
        "car_transmission",
        "car_engine",
        "car_liters",
        "car_capacity",
        "car_dims",
        "car_weight",
        "car_checkPeriod",
        "car_class",
        "car_buyDate",
        "car_mileage",
        "car_checkupDate",
        "car_checkupMileage",
        "car_dealerTO",
        "car_makeYear",
        "car_warrantyStart",
        "car_warrantyEnd",
        "car_contractType"
      ]
    },
    {
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
        "cardNumber_validFrom",
        "cardNumber_validUntil",
        "cardNumber_validUntilMilage",
        "cardNumber_milageTO",
        "cardNumber_serviceInterval",
        "cardNumber_cardOwner",
        "cardNumber_manager"
      ]
    },
    {
      "meta": {
        "infoText": "platenum",
        "mainToo": true
      },
      "targets": [
        "car_plateNum"
      ]
    },
    {
      "meta": {
        "infoText": "owner",
        "mainToo": true
      },
      "targets": [
        "contact_contactOwner"
      ]
    }
  ],
  "canDelete": true,
  "canUpdate": true,
  "canRead": true,
  "canCreate": true,
  "title": "Кейс",
  "name": "case"
}
