{
    "name": "usermeta",
    "title": "Метаданные пользователя",
    "canCreate": ["admin", "head", "supervisor"],
    "canRead": true,
    "canUpdate": ["admin", "head", "supervisor"],
    "canDelete": ["admin", "head", "supervisor"],
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
            "type": "password",
            "meta": {
                "label": "Пароль",
                "nosql": true,
                "required": true
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
                "dictionaryName": "allProgramsDict:"
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
            "type": "date",
            "meta": {
                "label": "День рождения"
            }
        },
        {
            "name": "position",
            "meta": {
                "label": "Должность"
            }
        }
    ]
}
