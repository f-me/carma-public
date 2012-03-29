{
    "name": "towage",
    "title": "Эвакуация",
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
            "name": "towerType",
            "meta": {
                "dictionaryName": "TowerTypes",
                "label": "Тип эвакуатора"
            },
            "index": true,
            "type": "dictionary"
        },
        {
            "name": "towType",
            "meta": {
                "dictionaryName": "TowTypes",
                "label": "Вид эвакуации"
            },
            "type": "dictionary"
        },
        {
            "name": "caseAddress",
            "groupName": "address"
        },
        {
            "name": "towDealer",
            "groupName": "partner",
            "meta": {
                "label": "Дилер"
            }
        },
        {
            "name": "towAddress",
            "groupName": "address"
        },
        {
            "name": "towContractor",
            "groupName": "partner",
            "meta": {
                "label": "Подрядчик"
            }
        },
        {
            "name": "wheelsUnblocked",
            "meta": {
                "default": true,
                "label": "Колёса не заблокированы"
            },
            "type": "checkbox"
        },
        {
            "name": "manipulatorPossible",
            "meta": {
                "default": true,
                "label": "Есть место для манипулятора"
            },
            "type": "checkbox"
        },
        {
            "name": "status",
            "type": "dictionary",
            "meta": {
                "label": "Статус услуги",
                "dictionaryName": "ServiceStatuses"
            }
        }
    ]
}
