{
    "name": "call",
    "title": "Входящий звонок",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "fields": [
        {
            "name": "wazzup",
            "type": "textarea",
            "canWrite": true,
            "canRead": true,
            "meta": {
                "label": "Что случилось?"
            }
        },
        {
            "name": "callType",
            "canWrite": true,
            "canRead": true,
            "meta": {
                "label": "Тип звонка"
            }
        },
        {
            "name": "callerType",
            "type": "dictionary",
            "canWrite": true,
            "canRead": true,
            "meta": {
                "label": "Кто звонит?",
                "dictionaryName": "CallerTypes"
            }
        },
        {
            "name": "callerName",
            "canWrite": true,
            "canRead": true,
            "meta": {
                "label": "ФИО"
            }
        },
        {
            "name": "callerPhone",
            "canWrite": true,
            "canRead": true,
            "index": true,
            "meta": {
                "label": "Телефон"
            }
        }
    ]
}
