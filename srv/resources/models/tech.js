{
    "title": "Техпомощь",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "fields": [
        {
            "name": "techType",
            "label": "Услуга",
            "canWrite": true,
            "canRead": true,
            "index": true,
            "required": true
        },
        {
            "name": "caseAddress",
            "label": "Адрес места поломки",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "techContractor",
            "label": "Название партнёра",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "techComments",
            "label": "Описание неисправности со слов клиента",
            "type": "textarea",
            "canWrite": true,
            "canRead": true
        },
        {
            "name": "caseid",
            "label": "Кейс, к которому привязана услуга",
            "type": "reference",
            "reference-model": "case",
            "canWrite": true,
            "canRead": true,
            "invisible": true
        }
    ]
}
