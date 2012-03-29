{
    "name": "tech",
    "title": "Техпомощь",
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
            "targets": ["caseAddress_address"],
            "meta": {
                "label": "Адрес места поломки"
            }
        }
    ],
    "fields": [
        {
            "name": "techType",
            "meta": {
                "dictionaryName": "TechTypes",
                "label": "Услуга",
                "required": true
            },
            "type": "dictionary",
            "index": true
        },
        {
            "name": "caseAddress",
            "groupName": "address"
        },
        {
            "name": "techContractor",
            "groupName": "partner",
            "meta": {
                "label": "Название партнёра"
            }
        },
        {
            "name": "techComments",
            "meta": {
                "label": "Описание неисправности со слов клиента"
            },
            "type": "textarea"
        },
        {
            "name": "status",
            "type": "dictionary",
            "meta": {
                "label": "Статус услуги",
                "dictionaryName": "ServiceStatuses"
            }
        }
    ]
}
