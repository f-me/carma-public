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
        }
    ]
}
