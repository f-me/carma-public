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
            }
        },
        {
            "name": "boCities"
        },
        {
            "name": "boPrograms"
        }
    ]
}
