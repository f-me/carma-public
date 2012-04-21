{
    "name": "rent",
    "title": "Подменный автомобиль",
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
            "targets": ["rentAddress_address"],
            "meta": {
                "label": "Куда доставить"
            }
        },
        {
            "targets": ["towDealer_partner"],
            "meta": {
                "label": "Дилер"
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
            "name": "status",
            "type": "dictionary",
            "meta": {
                "label": "Статус услуги",
                "dictionaryName": "ServiceStatuses"
            }
        },
        {
            "name": "towDealer",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "groupName": "partner"
        },
        {
            "name": "rentAddress",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "groupName": "address"
        },
        {
            "name": "carClass",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "meta": {
                "label": "Класс автомобиля"
            }
        },
        {
            "name": "rentContractor",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "groupName": "partner",
            "meta": {
                "label": "Подрядчик"
            }
        }
    ]
}
