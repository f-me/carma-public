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
            "meta": {
                "label": "Подрядчик"
            },
            "canWrite": true,
            "canRead": true
        }
    ]
}
