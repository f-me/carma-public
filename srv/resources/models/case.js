{
    "name": "case",
    "title": "Кейс",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "fields": [
        {
            "name": "callDate",
            "label": "Дата звонка",
            "canWrite": true,
            "canRead": true,
            "index": true
        },
        {
            "name": "callTime",
            "label": "Время звонка",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "callTaker",
            "label": "Сотрудник РАМК",
            "canWrite": false,
            "canRead": true,
            "required": true
        },
        {
            "name": "program",
            "label": "Программа",
            "canWrite": true,
            "canRead": true,
            "required": true,
            "index": true
        },
        {
            "name": "callerName",
            "label": "ФИО звонящего",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "ownerName",
            "label": "ФИО владельца",
            "canWrite": true,
            "canRead": true,
            "index": true
        },
        {
            "name": "phone",
            "label": "Телефон звонящего",
            "canWrite": true,
            "canRead": true,
            "index": true
        },
        {
            "name": "status",
            "label": "Статус звонка",
            "canWrite": true,
            "canRead": true,
            "required": true
        },
        {
            "name": "plateNum",
            "label": "Регистрационный номер автомобиля",
            "canWrite": true,
            "canRead": true,
            "required": true,
            "index": true
        },
        {
            "name": "comment",
            "label": "Комментарий",
            "type": "textarea",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "service",
            "label": "Услуги",
            "type": "reference",
            "canWrite": true,
            "canRead": true,
            "required": true,
            "index": true
        }
    ]
}
