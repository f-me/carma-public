{
    "name": "case",
    "title": "Кейс",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "applications": [
        {
            "targets": true,
            "canWrite": true,
            "canRead": true
        },
        {
            "targets": ["car_model"],
            "meta": {
                "dictionaryParent": "car_make"
            }
        }
    ],
    "fields": [
        {
            "name": "comment",
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Wazzup",
                "label": "Что случилось",
                "infoText": "comment"
            }
        },
        {
            "name": "diagnosis1",
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Diagnosis1",
                "label": "Диагностика"
            }
        },            
        {
            "name": "diagnosis2",
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Diagnosis2",
                "dictionaryParent": "diagnosis1"
            }
        },
        {
            "name": "diagnosis3"
        },
        {
            "name": "diagnosis4"
        },
        {
            "name": "callDate",
            "index": true,
            "indexCollate": true,
            "meta": {
                "label": "Дата звонка",
                "readonly": true
            }
        },
        {
            "name": "callTime",
            "meta": {
                "label": "Время звонка",
                "readonly": true
            }
        },
        {
            "name": "callTaker",
            "meta": {
                "label": "Сотрудник РАМК",
                "required": true,
                "readonly": true
            }
        },
        {
            "name": "program",
            "index": true,
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Programs",
                "label": "Программа",
                "required": true
            }
        },
        {
            "name": "caller",
            "groupName": "contact",
            "meta": {
                "label": "Клиент"
            }
        },
        {
            "name": "status",
            "type": "dictionary",
            "meta": {
                "required": true,
                "dictionaryName": "CaseStatuses",
                "label": "Статус звонка"
            }
        },
        {
            "name": "car",
            "groupName": "car"
        },
        {
            "name": "services",
            "type": "reference",
            "meta": {
                "label": "Услуги"
            }
        }
    ]
}
