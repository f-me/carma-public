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
            "name": "isActive",
            "type": "checkbox",
            "meta": {
                "label": "Активен"
            }
        },
        {
            "name": "realName",
            "meta": {
                "label": "ФИО пользователя"
            }
        },
        {
            "name": "login",
            "meta": {
                "label": "Логин",
                "required": true
            }
        },
        {
            "name": "password",
            "meta": {
                "label": "Пароль",
                "nosql": true,
                "required": true
            }
        },
        {
            "name": "boCities",
            "type": "dictionary-many",
            "meta": {
                "label": "boCities",
                "dictionaryName": "DealerCities",
                "invisible": true
            }
        },
        {
            "name": "boPrograms",
            "type": "dictionary-many",
            "meta": {
                "label": "boPrograms",
                "dictionaryName": "Programs",
                "invisible": true
            }
        },
        {
            "name": "weatherCities",
            "type": "dictionary-many",
            "meta": {
                "label": "weatherCities",
                "dictionaryName": "DealerCities",
                "invisible": true
            }
        },
        {
            "name": "isDealer",
            "type": "checkbox",
            "meta": {
                "label": "Дилер"
            }
        },
        {
            "name": "workPhone",
            "meta": {
                "label": "Рабочий телефон"
            }
        },
        {
            "name": "workPhoneSuffix",
            "meta": {
                "label": "Добавочный номер"
            }
        },
        {
            "name": "mobilePhone",
            "meta": {
                "label": "Мобильный телефон"
            }
        },
        {
            "name": "homePhone",
            "meta": {
                "label": "Домашний телефон"
            }
        },
        {
            "name": "email",
            "meta": {
                "regexp": "email",
                "label": "E-mail"
            }
        },
        {
            "name": "birthday",
            "meta": {
                "label": "День рождения"
            }
        },
        {
            "name": "position",
            "meta": {
                "label": "Должность"
            }
        },
        {
            "name": "roles",
            "type": "dictionary-many",
            "meta": {
                "label": "Роль в системе",
                "dictionaryName": "Roles"
            }
        },
        {
            "name": "programs",
            "type": "dictionary-many",
            "meta": {
                "label": "Программы",
                "dictionaryName": "user_programs:"
            }
        }
    ]
}
