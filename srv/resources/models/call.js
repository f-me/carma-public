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
            "label": "Что случилось?",
            "type": "textarea",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "callType",
            "label": "Тип звонка",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "callerType",
            "label": "Кто звонит?",
            "type": "radio",
            "choice": [
                "Клиент",
                "Подрядчик",
                "Дилерский центр",
                "Заказчик программы",
                "Другое"],
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "callerName",
            "label": "ФИО",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "callerPhone",
            "label": "Телефон",
            "canWrite": true,
            "canRead": true,
            "index": true
        }
    ]
}
