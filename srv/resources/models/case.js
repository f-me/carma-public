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
            "name": "caller",
            "groupName": "contact",
            "meta": {
                "label": "Клиент"
            }
        },
        {
            "name": "car",
            "groupName": "car"
        },
        {
            "name": "program",
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
            "meta": {
                "label": "Сотрудник РАМК",
                "required": true,
                "readonly": true
            }
        },
        {
            "name": "status",
            "type": "dictionary",
            "meta": {
                "required": true,
                "dictionaryName": "CaseStatuses",
                "label": "Статус кейса"
            }
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
