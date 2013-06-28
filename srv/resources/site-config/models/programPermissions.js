{
  "fields": [
    {
      "meta": {
        "invisible": true
      },
      "canWrite": true,
      "canRead": true,
      "name": "parentId"
    },
    {
      "meta": {
        "bounded": true,
        "required": true,
        "dictionaryName": "ContractFields",
        "label": "Тип поля"
      },
      "type": "dictionary",
      "canWrite": [
        "head"
      ],
      "canRead": [
        "head"
      ],
      "name": "contractField"
    },
    {
      "meta": {
        "label": "Отображается в таблице"
      },
      "canWrite": [
        "head"
      ],
      "canRead": [
        "head"
      ],
      "type": "checkbox",
      "name": "showTable"
    },
    {
      "meta": {
        "label": "Отображается в форме"
      },
      "canWrite": [
        "head"
      ],
      "canRead": [
        "head"
      ],
      "type": "checkbox",
      "name": "showForm"
    }
  ],
  "applications": [
    {
      "canRead": true,
      "canWrite": true,
      "targets": true
    }
  ],
  "canDelete": [
    "admin"
  ],
  "canUpdate": [
    "admin"
  ],
  "canRead": true,
  "canCreate": true,
  "title": "",
  "name": "programPermissions"
}
