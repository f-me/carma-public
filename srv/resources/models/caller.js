{
    "name": "caller",
    "title": "Звонящий",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "fields": [
        {
            "name": "callerName",
            "label": "ФИО звонящего",
            "canWrite": true,
            "canRead": true,
            "index": true,
            "indexCollate": true
        },
        {
            "name": "ownerName",
            "label": "ФИО владельца",
            "canWrite": true,
            "canRead": true,
            "index": true,
            "indexCollate": true
        },
        {
            "name": "phone",
            "label": "Телефон звонящего",
            "canWrite": true,
            "canRead": true,
            "index": true,
            "indexCollate": true
        }
    ]
}
