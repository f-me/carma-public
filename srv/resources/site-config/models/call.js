{
    "name": "call",
    "title": "Входящий звонок",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "applications": [
        {
            "targets": true,
            "canWrite": true,
            "canRead": true
        }
    ],
    "fields": [
        {
            "name": "callDate",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman"
            ],
            "index": true,
            "indexCollate": true,
            "type": "datetime",
            "meta": {
                "label": "Дата звонка",
                "readonly": true
            }
        },
        {
            "name": "callTaker",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "meta": {
                "label": "Сотрудник РАМК",
                "required": true,
                "readonly": true
            }
        },
        {
            "name": "program",
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Programs",
                "bounded":true,
                "label": "Программа"
            }
        },
        {
            "name": "wazzup",
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Wazzup",
                "label": "Что случилось"
            }
        },
        {
            "name": "callerName",
            "groupName": "carContact",
            "meta": {
                "label": "ФИО"
            }
        },
        {
            "name": "callerType",
            "type": "dictionary",
            "meta": {
                "label": "Кто звонит?",
                "bounded":true,
                "dictionaryName": "CallerTypes"
            }
        },
        {
            "name": "city",
            "type": "dictionary",
            "meta": {
                "dictionaryName": "DealerCities",
                "label": "Город"
            }
        },
        {
            "name": "make",
            "meta": {
                "dictionaryName": "CarMakers",
                "label": "Марка"
            },
            "type": "dictionary"
        },
        {
            "name": "model",
            "meta": {
                "dictionaryName": "CarModels",
                "dictionaryParent": "make",
                "label": "Модель"
            },
            "type": "dictionary"
        },
        {
            "name": "callType",
            "type": "dictionary",
            "meta": {
                "dictionaryName": "CallTypes",
                "dictionaryParent": "callerType",
                "bounded":true,
                "label": "Тип звонка"
            }
        }
    ]
}
