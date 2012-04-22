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
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],            
            "groupName": "address"
        },
        {
            "name": "toAddress",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],            
            "groupName": "address"
        },
        {
            "name": "multidrive",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],            
            "meta": {
                "label": "Мультидрайв"
            },
            "type": "checkbox"
        }
    ]
}
