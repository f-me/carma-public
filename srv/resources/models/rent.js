{
    "name": "rent",
    "title": "Подменный автомобиль",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "fields": [
        {
            "name": "towDealer",
            "groupName": "partner",
            "canWrite": true,
            "canRead": true,
            "meta": {
                "label": "Дилер"
            }
        },
        {
            "name": "rentAddress",
            "canWrite": true,
            "canRead": true,
            "meta": {
                "label": "Куда доставить"
            }
        },
        {
            "name": "carClass",
            "meta": {
                "label": "Класс автомобиля"
            },
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "rentContractor",
            "groupName": "partner",
            "meta": {
                "label": "Подрядчик"
            },
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
