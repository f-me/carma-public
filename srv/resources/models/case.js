{
    "name": "case",
    "title": "Кейс",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "fields": [
        {
            "name": "comment",
            "label": "Что случилось",
            "type": "textarea",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "diagnosis1",
            "label": "Диагностика",
            "canWrite": true,
            "canRead": true,
            "type": "dictionary",
            "dictionaryName": "Diagnosis1"
        },            
        {
            "name": "diagnosis2",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "diagnosis3",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "diagnosis4",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "callDate",
            "label": "Дата звонка",
            "canWrite": true,
            "canRead": true,
            "index": true,
            "indexCollate": true,
            "readonly": true
        },
        {
            "name": "callTime",
            "label": "Время звонка",
            "canWrite": true,
            "canRead": true,
            "readonly": true
        },
        {
            "name": "callTaker",
            "label": "Сотрудник РАМК",
            "canWrite": false,
            "canRead": true,
            "required": true,
            "readonly": true
        },
        {
            "name": "program",
            "label": "Программа",
            "canWrite": true,
            "canRead": true,
            "required": true,
            "index": true,
            "type": "dictionary",
            "dictionaryName": "Programs"
        },
        {
            "name": "address",
            "label": "Адрес",
            "canWrite": true,
            "canRead": true,
            "type": "group",
            "groupName": "address"
        },
        {
            "name": "caller",
            "label": "Звонящий",
            "canWrite": true,
            "canRead": true,
            "type": "group",
            "groupName": "caller"
        },
        {
            "name": "status",
            "label": "Статус звонка",
            "canWrite": true,
            "canRead": true,
            "required": true
        },
        {
            "name": "car",
            "groupName": "car",
            "type": "group"
        },
        {
            "name": "services",
            "label": "Услуги",
            "type": "reference",
            "canWrite": true,
            "canRead": true
        }
    ]
}
