{
    "name": "cost_serviceTarifOption",
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
            "name": "tarifOptionId",
            "canRead": true,
            "canWrite": true,
            "meta":{
                "invisible": true
            }

        },
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
