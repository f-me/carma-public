{
  "fields": [
    {
      "meta": {
        "label": "Активна"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "active"
    },
    {
      "meta": {
        "label": "Название"
      },
      "type": null,
      "groupName": null,
      "name": "label"
    },
    {
      "meta": {
        "label": "Внутренняя метка"
      },
      "name": "value"
    },
    {
      "meta": {
        "label": "Заказчик"
      },
      "type": null,
      "groupName": null,
      "name": "client"
    },
    {
      "meta": {
        "label": "Код заказчика"
      },
      "type": null,
      "groupName": null,
      "name": "clientCode"
    },
    {
      "meta": {
        "label": "Адрес заказчика"
      },
      "type": null,
      "groupName": null,
      "name": "clientAddress"
    },
    {
      "meta": {
        "dictionaryName": "Services",
        "required": true,
        "bounded": true,
        "label": "Услуги, предоставляемые по программе"
      },
      "type": "dictionary-many",
      "groupName": null,
      "name": "services"
    },
    {
      "meta": {
        "required": true,
        "label": "Межсервисный интервал по умолчанию",
        "sqltype": "integer"
      },
      "type": null,
      "groupName": null,
      "name": "carCheckPeriodDefault"
    },
    {
      "meta": {
        "required": true,
        "label": "Срок действия программы по умолчанию",
        "sqltype": "integer"
      },
      "type": null,
      "groupName": null,
      "name": "duedateDefault"
    },
    {
      "meta": {
        "label": "Шаблон договора",
        "widget": "inline-uploader",
        "reference-widget": "files"
      },
      "type": "reference",
      "groupName": null,
      "name": "contracts"
    },
    {
      "meta": {
        "label": "Ограничение прав",
        "model": "programPermissions",
        "reference-label": "Добавить ограничение на поле контракта"
      },
      "type": "reference",
      "groupName": null,
      "name": "programPermissions"
    },
    {
      "meta": {
        "dictionaryName": "Programs",
        "label": "Формат файлов VIN"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "vinFormat"
    },
    {
      "meta": {
        "label": "Логотип",
        "widget": "inline-uploader",
        "reference-widget": "files",
        "single-uploader": true
      },
      "type": "reference",
      "groupName": null,
      "name": "logo"
    },
    {
      "meta": {
        "label": "Справка"
      },
      "type": "textarea",
      "groupName": null,
      "name": "help"
    }
  ],
  "applications": [],
  "canDelete": [
    "admin"
  ],
  "canUpdate": [
    "admin"
  ],
  "canRead": true,
  "canCreate": true,
  "title": "Программа",
  "name": "program"
}
