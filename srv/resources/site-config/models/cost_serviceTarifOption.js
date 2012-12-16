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
            "name": "price",
            "meta": {
                "label": "Цена за единицу",
                "readonly": true
            }
        },
        {
            "name": "count",
            "meta": {
                "label": "Количество",
                "regexp": "number"
            }
        },
        {
            "name": "cost",
            "meta": {
                "label": "Стоимость",
                "readonly": true
            }
        },
        {
            "name": "price1",
            "meta": {
                "label": "Стоимость за единицу за нал",
                "invisible": true
            }
        },
        {
            "name": "price2",
            "meta": {
                "label": "Стоимость за единицу по безналу",
                "invisible": true
            }
        }
    ]
}
