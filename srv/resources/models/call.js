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
            "name": "wazzup",
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Wazzup",
                "label": "Что случилось"
            }
        },
        {
            "name": "callType",
            "meta": {
                "label": "Тип звонка"
            }
        },
        {
            "name": "callerType",
            "type": "dictionary",
            "meta": {
                "label": "Кто звонит?",
                "dictionaryName": "CallerTypes",
                "widget": "radio"
            }
        },
        {
            "name": "callerName",
            "meta": {
                "label": "ФИО"
            }
        },
        {
            "name": "callerPhone",
            "index": true,
            "meta": {
                "label": "Телефон"
            }
        }
    ]
}
