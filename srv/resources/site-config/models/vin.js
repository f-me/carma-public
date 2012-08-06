{
    "name": "vin",
    "title": "VIN",
    "canCreate": false,
    "canRead": true,
    "canUpdate": true,
    "canDelete": false,
    "applications": [
        {
            "targets": true,
            "canWrite": true,
            "canRead": true
        }
    ],
    "fields": [
        {
            "name": "vin",
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "meta": {
                "label": "VIN",
                "readonly": true
            }
        },
        {
            "name": "callTaker",
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "meta": {
                "label": "Сотрудник РАМК",
                "required": true,
                "readonly": true
            }
        },
        {
            "name": "program",
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "index": true,
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Programs",
                "label": "Программа",
                "required": true,
                "targetCategory": "program",
                "infoText": "program"
            }
        },
        {
            "name": "owner",
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "groupName": "vinContact",
            "meta": {
                "label": "Владелец",
                "infoText": "ownerName"
            }
        },
        {
            "name": "make",
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "meta": {
                "dictionaryName": "CarMakers",
                "required": true,
                "label": "Марка"
            },
            "type": "dictionary"
        },
        {
            "name": "model",
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "meta": {
                "dictionaryName": "CarModels",
                "required": true,
                "label": "Модель"
            },
            "type": "dictionary"
        },
        {
            "name": "plateNum",
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "meta": {
                "transform": "uppercase",
                "label": "Госномер",
                "required": true,
                "regexp": "plateNum"
            },
            "index": true,
            "indexCollate": true
        },
        {
            "name": "makeYear",
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "meta": {
                "label": "Год производства автомобиля"
            }
        },
        {
            "name": "color",
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "meta": {
                "dictionaryName": "Colors",
                "required": true,
                "label": "Цвет"
            },
            "type": "dictionary"
        },
        {
            "name": "buyDate",
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "type": "date",
            "meta": {
                "label": "Дата покупки",
                "required": true
            }
        },
        {
            "name": "checkupDate",
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "type": "date",
            "meta": {
                "label": "Дата последнего ТО",
                "required": true
            }
        },
        {
            "name": "cardNumber",
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "type": "picker",
            "meta": {
                "label": "Номер карты участника"
            }
        },
        {
            "name": "validFrom",
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "type": "date",
            "meta": {
                "label": "Дата регистрации в программе"
            }
        },
        {
            "name": "validUntil",
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "type": "date",
            "meta": {
                "label": "Программа действует до (дата)"
            }
        },
        {
            "name": "validUntilMilage",
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "meta": {
                "label": "Программа действует до (пробег)"
            }
        },
        {
            "name": "milageTO",
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "meta": {
                "label": "Пробег при регистрации в программе"
            }
        },
        {
            "name": "serviceInterval",
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "meta": {
                "label": "Межсервисный интервал"
            }
        },
        {
            "name": "cardOwner",
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "meta": {
                "label": "ФИО владельца карты"
            }
        },
        {
            "name": "manager",
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "meta": {
                "label": "ФИО менеджера"
            }
        }

    ]
}
