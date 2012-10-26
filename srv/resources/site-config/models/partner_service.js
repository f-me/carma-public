
{
    "name": "partner_service",
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
            "name": "parentId",
            "canRead": true,
            "canWrite": true,
            "meta": {
                "invisible": true
            }
        },
        {
            "name": "priority1",
            "meta": {
                "label": "Приоритет за нал"
            }
        },
        {
            "name": "priority2",
            "meta": {
                "label": "Приоритет по безналу город"
            }
        },
        {
            "name": "priority3",
            "meta": {
                "label": "Приоритет по безналу за город"
            }
        },
        {
            "name": "serviceName",
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Services",
                "label": "Услуга"
            }
        },
        {
            "name": "falseCallPercent",
            "meta": {
                "label": "Процент за ложный вызов"
            }
        },
        {
            "name": "tarifOptions",
            "type": "reference"
        }
    ]
}
