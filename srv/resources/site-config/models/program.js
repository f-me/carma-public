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
                "label": "Внутреннее название",
                "invisible": true,
                "readonly": true
            }
        },
        {
            "name": "label",
            "meta": {
                "label": "Название"
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
