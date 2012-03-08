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
            "label": "Тип эвакуатора",
            "canWrite": true,
            "canRead": true,
            "index": true,
            "type": "dictionary",
            "dictionaryName": "TowerTypes"
        },
        {
            "name": "towType",
            "label": "Вид эвакуации",
            "canWrite": true,
            "canRead": true,
            "type": "dictionary",
            "dictionaryName": "TowTypes"
        },
        {
            "name": "caseAddress",
            "label": "Адрес места поломки",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "towDealer",
            "label": "Дилер",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "towAddress",
            "label": "Адрес доставки",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "towContractor",
            "label": "Подрядчик",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "wheelsUnblocked",
            "label": "Колёса не заблокированы",
            "type": "checkbox",
            "default": true,
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "manipulatorPossible",
            "label": "Есть место для манипулятора",
            "type": "checkbox",
            "default": true,
            "canWrite": true,
            "canRead": true
        }
    ]
}
