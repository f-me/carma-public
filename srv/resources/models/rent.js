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
            "groupName": "partner"
        },
        {
            "name": "rentAddress",
            "groupName": "address"
        },
        {
            "name": "carClass",
            "meta": {
                "label": "Класс автомобиля"
            }
        },
        {
            "name": "rentContractor",
            "groupName": "partner",
            "meta": {
                "label": "Подрядчик"
            }
        }
    ]
}
