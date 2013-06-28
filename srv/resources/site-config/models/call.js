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
      "name": "callDate"
    },
    {
      "meta": {
        "readonly": true,
        "required": true,
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
        "label": "Программа",
        "bounded": true,
        "dictionaryName": "Programs"
      },
      "type": "dictionary",
      "name": "program"
    },
    {
      "meta": {
        "label": "Что случилось",
        "dictionaryName": "Wazzup"
      },
      "type": "dictionary",
      "name": "wazzup"
    },
    {
      "meta": {
        "label": "ФИО"
      },
      "groupName": "carContact",
      "name": "callerName"
    },
    {
      "meta": {
        "dictionaryName": "CallerTypes",
        "bounded": true,
        "label": "Кто звонит?"
      },
      "type": "dictionary",
      "name": "callerType"
    },
    {
      "meta": {
        "label": "Город",
        "bounded": true,
        "dictionaryName": "DealerCities"
      },
      "type": "dictionary",
      "name": "city"
    },
    {
      "type": "dictionary",
      "meta": {
        "label": "Марка",
        "dictionaryName": "CarMakers"
      },
      "name": "carMake"
    },
    {
      "type": "dictionary",
      "meta": {
        "label": "Модель",
        "dictionaryParent": "carMake",
        "dictionaryName": "CarModels"
      },
      "name": "carModel"
    },
    {
      "meta": {
        "label": "Тип звонка",
        "bounded": true,
        "dictionaryParent": "callerType",
        "dictionaryName": "CallTypes"
      },
      "type": "dictionary",
      "name": "callType"
    }
  ],
  "applications": [
    {
      "canRead": true,
      "canWrite": true,
      "targets": true
    }
  ],
  "canDelete": true,
  "canUpdate": true,
  "canRead": true,
  "canCreate": true,
  "title": "Входящий звонок",
  "name": "call"
}
