{
    "name": "hotel",
    "title": "Гостиница",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "fields": [
        {
            "name": "caseAddress",
            "groupName": "address",
            "canWrite": true,
            "canRead": true,
            "meta": {
                "label": "Откуда везём"
            }
        },
        {
            "name": "hotelContractor",
            "groupName": "partner",
            "meta": {
                "label": "Название партнёра"
            },
            "canWrite": true,
            "canRead": true
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
