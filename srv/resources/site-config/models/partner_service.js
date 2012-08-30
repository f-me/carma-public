
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
                "label": "Прцент за ложный вызов"
            }
        },
        {
            "name": "tarifOptions",
            "type": "reference"
        }
    ]
}
