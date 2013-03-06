 {
    "name": "program",
    "title": "Прогармма",
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
            "name": "contracts",
            "type": "files",
            "meta": {
                "label": "Шаблон договора"
            }
        }
    ]
}
