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
