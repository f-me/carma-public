{
    "name": "towage",
    "title": "Эвакуация",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "fields": [
        {
            "name": "towerType",
            "meta": {
                "dictionaryName": "TowerTypes",
                "label": "Тип эвакуатора"
            },
            "canWrite": true,
            "canRead": true,
            "index": true,
            "type": "dictionary"
        },
        {
            "name": "towType",
            "meta": {
                "dictionaryName": "TowTypes",
                "label": "Вид эвакуации"
            },
            "canWrite": true,
            "canRead": true,
            "type": "dictionary"
        },
        {
            "name": "caseAddress",
            "groupName": "address",
            "meta": {
                "label": "Адрес места поломки"
            },
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "towDealer",
            "groupName": "partner",
            "meta": {
                "label": "Дилер"
            },
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "towAddress",
            "groupName": "address",
            "meta": {
                "label": "Адрес доставки"
            },
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "towContractor",
            "groupName": "partner",
            "meta": {
                "label": "Подрядчик"
            },
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "wheelsUnblocked",
            "meta": {
                "default": true,
                "label": "Колёса не заблокированы"
            },
            "type": "checkbox",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "manipulatorPossible",
            "meta": {
                "default": true,
                "label": "Есть место для манипулятора"
            },
            "type": "checkbox",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "status",
            "canWrite": true,
            "canRead": true,
            "type": "dictionary",
            "meta": {
                "label": "Статус услуги",
                "dictionaryName": "ServiceStatuses"
            }
        }
    ]
}
