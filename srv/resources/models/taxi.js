{
    "name": "taxi",
    "title": "Такси",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "fields": [
        {
            "name": "taxiFrom",
            "canWrite": true,
            "canRead": true,
            "meta": {
                "label": "Где забрать"
            }
        },
        {
            "name": "taxiTo",
            "canWrite": true,
            "canRead": true,
            "meta": {
                "label": "Куда доставить"
            }
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
