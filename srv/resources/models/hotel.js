{
    "title": "Гостиница",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "fields": [
        {
            "name": "caseAddress",
            "label": "Откуда везём",
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
