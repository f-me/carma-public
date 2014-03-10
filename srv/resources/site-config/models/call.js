{
  "fields": [
    {
      "meta": {
        "label": "Дата звонка",
        "readonly": true
      },
      "type": "datetime",
      "groupName": null,
      "name": "callDate"
    },
    {
      "meta": {
        "label": "Сотрудник РАМК",
        "required": true,
        "readonly": true
      },
      "type": null,
      "groupName": null,
      "name": "callTaker"
    },
    {
      "meta": {
        "dictionaryName": "Program",
        "dictionaryType": "ModelDict",
        "dictionaryStringify": true,
        "bounded": true,
        "label": "Программа"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "program"
    },
    {
      "meta": {
        "dictionaryName": "SubProgram",
        "dictionaryType": "ModelDict",
        "dictionaryStringify": true,
        "dictionaryParent": "program",
        "bounded": true,
        "label": "Подпрограмма"
      },
      "type": "dictionary",
      "name": "subprogram"
    },
    {
      "meta": {
        "dictionaryName": "Wazzup",
        "label": "Что случилось"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "wazzup"
    },
    {
      "meta": {
        "label": "ФИО"
      },
      "type": null,
      "groupName": "carContact",
      "name": "callerName"
    },
    {
      "meta": {
        "label": "Кто звонит?",
        "bounded": true,
        "dictionaryName": "CallerTypes"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "callerType"
    },
    {
      "meta": {
        "dictionaryName": "DealerCities",
        "bounded": true,
        "label": "Город"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "city"
    },
    {
      "meta": {
        "label": "Координаты места поломки",
        "invisible": true
      },
      "type": "coords",
      "name": "coords"
    },
    {
      "meta": {
        "label": "Адрес места поломки",
        "invisible": true
      },
      "name": "address"
    },
    {
      "meta": {
        "dictionaryName": "CarMakers",
        "label": "Марка"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "carMake"
    },
    {
      "meta": {
        "dictionaryName": "CarModels",
        "dictionaryParent": "carMake",
        "label": "Модель"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "carModel"
    },
    {
      "meta": {
        "dictionaryName": "CallTypes",
        "dictionaryParent": "callerType",
        "bounded": true,
        "label": "Тип звонка"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "callType"
    }
  ],
  "applications": [],
  "canDelete": true,
  "canUpdate": true,
  "canRead": true,
  "canCreate": true,
  "title": "Входящий звонок",
  "name": "call"
}
