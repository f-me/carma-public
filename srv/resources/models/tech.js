{
    "name": "tech",
    "title": "Техпомощь",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "fields": [
        {
            "name": "techType",
            "meta": {
                "dictionaryName": "TechTypes",
                "label": "Услуга",
                "required": true
            },
            "canWrite": true,
            "canRead": true,
            "type": "dictionary",
            "index": true
        },
        {
            "name": "caseAddress",
            "meta": {
                "label": "Адрес места поломки"
            },
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "techContractor",
            "meta": {
                "label": "Название партнёра"
            },
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "techComments",
            "meta": {
                "label": "Описание неисправности со слов клиента"
            },
            "type": "textarea",
            "canWrite": true,
            "canRead": true
        }
    ]
}
