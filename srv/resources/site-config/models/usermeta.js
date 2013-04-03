 {
    "name": "usermeta",
    "title": "Метаданные пользователя",
    "canCreate": ["admin"],
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
            "name": "realName",
            "meta": {
                "label": "Настоящее имя"
            }
        },
        {
            "name": "roles",
            "meta": {
                "label": "Роли"
            },
            "type": "dictionary-many"
        },
        {
            "name": "boCities",
            "type": "dictionary-many",
            "meta": {
                "dictionaryName": "DealerCities"
            }
        },
        {
            "name": "boPrograms",
            "type": "dictionary-many",
            "meta": {
                "dictionaryName": "Programs"
            }
        },
        {
            "name": "weathercities",
            "type": "dictionary-many",
            "meta": {
                "dictionaryName": "DealerCities"
            }
        }
    ]
}
