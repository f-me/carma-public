{
    "name": "tarifOption",
    "title": "Тарифная опция",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "applications": [
        {
            "targets": true,
            "canWrite": true,
            "canRead": true
        }
    ],
    "fields": [
        {
            "name": "parentId",
            "canRead": true,
            "canWrite": true,
            "meta": {
                "invisible": true
            }
        },
        {
            "name": "optionName",
            "meta": {
                "label": "Название опции"
            }
        },
        {
            "name": "price1",
            "meta": {
                "label": "Стоимость за единицу за нал"
            }
        },
        {
            "name": "price2",
            "meta": {
                "label": "Стоимость за единицу по безналу"
            }
        }
    ]
}
