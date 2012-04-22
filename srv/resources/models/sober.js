{
    "name": "sober",
    "title": "Трезвый водитель",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "applications": [
        {
            "targets": ["fromAddress_address"],
            "meta": {
                "label": "Где забрать"
            }
        },
        {
            "targets": ["toAddress_address"],
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
            "name": "fromAddress",
            "groupName": "address"
        },
        {
            "name": "toAddress",
            "groupName": "address"
        },
        {
            "name": "multidrive",
            "meta": {
                "label": "Мультидрайв"
            },
            "type": "checkbox"
        }
    ]
}
