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
            "name": "uid",
            "meta": {
                "invisible": true,
                "sqltype": "integer"
            }
        },
        {
            "name": "realName",
            "meta": {
                "label": "Настоящее имя"
            }
        },
        {
            "name": "password",
            "meta": {
                "label": "Пароль",
                "nosql": true
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
                "label": "boCities",
                "dictionaryName": "DealerCities"
            }
        },
        {
            "name": "boPrograms",
            "type": "dictionary-many",
            "meta": {
                "label": "boPrograms",
                "dictionaryName": "Programs"
            }
        },
        {
            "name": "weatherCities",
            "type": "dictionary-many",
            "meta": {
                "label": "weatherCities",
                "dictionaryName": "DealerCities"
            }
        }
    ]
}
