{
  "fields": [
    {
      "meta": {
        "label": "Активна"
      },
      "type": "checkbox",
      "name": "active"
    },
    {
      "meta": {
        "label": "Название"
      },
      "name": "label"
    },
    {
      "meta": {
        "label": "Заказчик"
      },
      "name": "client"
    },
    {
      "meta": {
        "label": "Код заказчика"
      },
      "name": "clientCode"
    },
    {
      "meta": {
        "label": "Адрес заказчика"
      },
      "name": "clientAddress"
    },
    {
      "type": "dictionary-many",
      "meta": {
        "label": "Услуги, предоставляемые по программе",
        "bounded": true,
        "required": true,
        "dictionaryName": "Services"
      },
      "name": "services"
    },
    {
      "meta": {
        "sqltype": "integer",
        "label": "Межсервисный интервал по умолчанию",
        "required": true
      },
      "name": "carCheckPeriodDefault"
    },
    {
      "meta": {
        "sqltype": "integer",
        "label": "Срок действия программы по умолчанию",
        "required": true
      },
      "name": "duedateDefault"
    },
    {
      "meta": {
        "label": "Шаблон договора"
      },
      "type": "file",
      "name": "contracts"
    },
    {
      "meta": {
        "label": "Ограничение прав"
      },
      "type": "reference",
      "name": "programPermissions"
    },
    {
      "meta": {
        "label": "Формат файлов VIN",
        "dictionaryName": "Programs"
      },
      "type": "dictionary",
      "name": "vinFormat"
    },
    {
      "meta": {
        "label": "Логотип"
      },
      "type": "file",
      "name": "logo"
    },
    {
      "meta": {
        "label": "Справка"
      },
      "type": "textarea",
      "name": "help"
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
  "title": "Программа",
  "name": "program"
}
