{
    "name": "service_tarifOption",
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
                "label": "Название опции",
                "readonly": true
            }
        },
        {
            "name": "count",
            "meta": {
                "label": "Колличество",
                "regexp": "number"
            }
        },
        {
            "name": "price",
            "meta": {
                "label": "Стоимость",
                "readonly": true
            }
        }
    ]
}
