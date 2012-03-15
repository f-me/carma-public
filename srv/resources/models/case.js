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
            "name": "callDate",
            "label": "Дата звонка",
            "canWrite": true,
            "canRead": true,
            "index": true,
            "indexCollate": true
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
            "name": "address",
            "label": "Адрес",
            "canWrite": true,
            "canRead": true,
            "type": "reference"
        },
        {
            "name": "caller",
            "label": "Звонящий",
            "canWrite": true,
            "canRead": true,
            "type": "reference"
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
            "name": "services",
            "label": "Услуги",
            "type": "reference",
            "canWrite": true,
            "canRead": true
        }
    ]
}
