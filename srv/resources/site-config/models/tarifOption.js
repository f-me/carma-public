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
            "name": "optionName",
            "meta": {
                "label": "Название опции"
            }
        },
        {
            "name": "price",
            "meta": {
                "label": "Цена за единицу",
                "regexp": "number"
            }
        }
    ]
}
