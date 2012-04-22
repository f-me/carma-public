{
    "name": "case",
    "title": "Кейс",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "applications": [
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
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Wazzup",
                "label": "Что случилось",
                "infoText": "comment"
            }
        },
        {
            "name": "notVandal",
            "type": "checkbox",
            "meta": {
                "label": "Не вандализм"
            }
        },
        {
            "name": "notAccident",
            "type": "checkbox",
            "meta": {
                "label": "Не ДТП"
            }
        },
        {
            "name": "diagnosis1",
            "canRead": ["front", "back", "head"],
            "canWrite": ["front", "back", "head"],
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Diagnosis1",
                "label": "Диагностика"
            }
        },            
        {
            "name": "diagnosis2",
            "canRead": ["front", "back", "head"],
            "canWrite": ["front", "back", "head"],
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Diagnosis2",
                "dictionaryParent": "diagnosis1"
            }
        },
        {
            "name": "diagnosis3",
            "canRead": ["front", "back", "head"],
            "canWrite": ["front", "back", "head"]
        },
        {
            "name": "diagnosis4",
            "canRead": ["front", "back", "head"],
            "canWrite": ["front", "back", "head"]
        },
        {
            "name": "caller",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "groupName": "contact",
            "meta": {
                "label": "Клиент"
            }
        },
        {
            "name": "car",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "groupName": "car"
        },
        {
            "name": "program",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "index": true,
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Programs",
                "label": "Программа",
                "required": true,
                "targetCategory": "program"
            }
        },
        {
            "name": "callDate",
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
            "canRead": ["front", "back", "head", "parguy"],
            "meta": {
                "label": "Сотрудник РАМК",
                "required": true,
                "readonly": true
            }
        },
        {
            "name": "status",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["head"],
            "type": "dictionary",
            "meta": {
                "required": true,
                "dictionaryName": "CaseStatuses",
                "label": "Статус кейса"
            }
        },
        {
            "name": "services",
            "canRead": true,
            "canWrite": true,
            "type": "reference",
            "meta": {
                "label": "Услуги"
            }
        }
    ]
}
