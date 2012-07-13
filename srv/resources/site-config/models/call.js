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
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "canWrite": [ "head" ],
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
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "meta": {
                "label": "Сотрудник РАМК",
                "required": true,
                "readonly": true
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
            "groupName": "contact",
            "meta": {
                "label": "ФИО"
            }
        },
        {
            "name": "callerType",
            "type": "dictionary",
            "meta": {
                "label": "Кто звонит?",
                "dictionaryName": "CallerTypes"
            }
        },
        {
            "name": "callType",
            "type": "dictionary",
            "meta": {
                "dictionaryName": "CallTypes",
                "dictionaryParent": "callerType",
                "label": "Тип звонка"
            }
        }
    ]
}
