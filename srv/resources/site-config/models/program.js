 {
    "name": "program",
    "title": "Прогарма",
    "canCreate": true,
    "canRead": true,
    "canUpdate": ["admin"],
    "canDelete": ["admin"],
     "applications": [
         {
            "targets": true,
            "canWrite": true,
            "canRead": true
         }
    ],
    "fields": [
        {
            "name": "value",
            "meta": {
                "label": "Внутреннее название"
            }
        },
        {
            "name": "label",
            "meta": {
                "label": "Название"
            }
        }
    ]
}
