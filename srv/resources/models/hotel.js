{
    "name": "hotel",
    "title": "Гостиница",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
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
            "name": "hotelContractor",
            "groupName": "partner",
            "meta": {
                "label": "Название партнёра"
            }
        },
        {
            "name": "hotelProvidedFor",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "meta": {
                "label": "Срок, на который предоставлена гостиница (дней)"
            }
        }
    ]
}
