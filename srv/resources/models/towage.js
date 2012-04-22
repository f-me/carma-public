{
    "name": "towage",
    "title": "Эвакуация",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "applications": [
        {
            "targets": ["caseAddress_address"],
            "meta": {
                "label": "Адрес места поломки"
            }
        },
        {
            "targets": ["towAddress_address"],
            "meta": {
                "label": "Адрес доставки"
            }
        }
    ],
    "fields": [
        {
            "name": "status",
            "type": "dictionary",
            "meta": {
                "label": "Статус услуги",
                "dictionaryName": "ServiceStatuses"
            }
        },
        {
            "name": "towerType",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "meta": {
                "dictionaryName": "TowerTypes",
                "label": "Тип эвакуатора"
            },
            "index": true,
            "type": "dictionary"
        },
        {
            "name": "towType",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "meta": {
                "dictionaryName": "TowTypes",
                "label": "Вид эвакуации"
            },
            "type": "dictionary"
        },
        {
            "name": "vandalism",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "meta": {
                "default": false,
                "label": "Случай вандализма"
            },
            "type": "checkbox"
        },
        {
            "name": "accident",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "meta": {
                "default": false,
                "label": "ДТП"
            },
            "type": "checkbox"
        },          
        {
            "name": "caseAddress",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "groupName": "address"
        },
        {
            "name": "towDealer",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "groupName": "partner",
            "meta": {
                "label": "Дилер"
            }
        },
        {
            "name": "towAddress",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "groupName": "address"
        },
        {
            "name": "towContractor",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "groupName": "partner",
            "meta": {
                "label": "Подрядчик"
            }
        },
        {
            "name": "wheelsUnblocked",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "meta": {
                "default": true,
                "label": "Колёса не заблокированы"
            },
            "type": "checkbox"
        },
        {
            "name": "manipulatorPossible",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "meta": {
                "default": true,
                "label": "Есть место для манипулятора"
            },
            "type": "checkbox"
        },
        {
            "name": "suburbanMilage",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],            
            "meta": {
                "label": "Пробег эвакуатора за городом"
            }
        },
        {
            "name": "loadDate",
            "canRead": ["back", "head"],
            "canWrite": ["back", "head"],
            "type": "datetime",
            "meta": {
                "label": "Время погрузки"
            }
        },
        {
            "name": "unloadDate",
            "canRead": ["back", "head"],
            "canWrite": ["back", "head"],
            "type": "datetime",
            "meta": {
                "label": "Время разгрузки"
            }
        }
    ]
}
