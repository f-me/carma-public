
{
    "name": "partner-service",
    "title": "Тариф",
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
            "name": "tarifName",
            "meta": {
                "label": "Тарифная опция"
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
