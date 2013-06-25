 {
    "name": "program",
    "title": "Программа",
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
            "name": "active",
            "type": "checkbox",
            "meta": {
                "label": "Активна"
            }
        },
        {
            "name": "label",
            "meta": {
                "label": "Название"
            }
        },
        {
            "name": "client",
            "meta": {
            "label": "Заказчик"
            }

        },
        {
            "name": "clientCode",
            "meta": {
                "label": "Код заказчика"
            }
        },
        {
            "name": "clientAddress",
            "meta": {
                "label": "Адрес заказчика"
            }
        },
        {
            "name": "services",
            "meta": {
                "dictionaryName": "Services",
                "required": true,
                "bounded": true,
                "label": "Услуги, предоставляемые по программе"
            },
            "type": "dictionary-many"
        },
        {
            "name": "carCheckPeriodDefault",
            "meta": {
                "required": true,
                "label": "Межсервисный интервал по умолчанию",
                "sqltype": "integer"
            }
        },
        {
            "name": "duedateDefault",
            "meta": {
                "required": true,
                "label": "Срок действия программы по умолчанию",
                "sqltype": "integer"
            }
        },
        {
            "name": "contracts",
            "type": "file",
            "meta": {
                "label": "Шаблон договора"
            }
        },
        {
            "name": "programPermissions",
            "type": "reference",
            "meta": {
                "label": "Ограничение прав"
            }
        },
        {
            "name": "vinFormat",
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Programs",
                "label": "Формат файлов VIN"
            }
        },
        {
            "name": "logo",
            "type": "file",
            "meta": {
                "label": "Логотип"
            }
        },
        {
            "name": "help",
            "type": "textarea",
            "meta": {
                "label": "Справка"
            }
        }
    ]
}
