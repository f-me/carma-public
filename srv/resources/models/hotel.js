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
            "name": "caseAddress",
            "groupName": "address",
            "meta": {
                "label": "Откуда везём"
            }
        },
        {
            "name": "hotelContractor",
            "groupName": "partner",
            "meta": {
                "label": "Название партнёра"
            }
        }
    ]
}
