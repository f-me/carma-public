{
    "name": "taxi",
    "title": "Такси",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "applications": [
        {
            "targets": true,
            "canWrite": true,
            "canRead": true
        },
        {
            "targets": ["taxiFrom_address"],
            "meta": {
                "label": "Где забрать"
            }
        },
        {
            "targets": ["taxiTo_address"],
            "meta": {
                "label": "Куда доставить"
            }
        }
    ],
    "fields": [
        {
            "name": "status",
            "type": "dictionary",
            "meta": {
                "label": "Статус услуги",
                "dictionaryName": "ServiceStatuses"
            }
        },
        {
            "name": "taxiFrom",
            "groupName": "address"
        },
        {
            "name": "taxiTo",
            "groupName": "address"
        },
        {
            "name": "taxiContractor",
            "groupName": "partner",
            "meta": {
                "label": "Название партнёра"
            }
        }
    ]
}
