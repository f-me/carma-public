{
    "name": "programPermissions",
    "title": "",
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
            "name": "parentId",
            "canRead": true,
            "canWrite": true,
            "meta": {
                "invisible": true
            }
        },
        {
            "name": "contractField",
            "canRead": ["head"],
            "canWrite": ["head"],
            "type": "dictionary",
            "meta": {
                "label": "Тип поля",
                "dictionaryName": "ContractFields",
                "required": true,
                "bounded":true
            }
        },
        {
            "name": "showTable",
            "type": "checkbox",
            "canRead": ["head"],
            "canWrite": ["head"],
            "meta": {
                "label": "Отображается в таблице"
            }
        },
        {
            "name": "showForm",
            "type": "checkbox",
            "canRead": ["head"],
            "canWrite": ["head"],
            "meta": {
                "label": "Отображается в форме"
            }
        }
    ]
}
