{
  "fields": [
    {
      "meta": {
        "label": "Партнёр активен"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "isActive"
    },
    {
      "meta": {
        "label": "Дилер"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "isDealer"
    },
    {
      "meta": {
        "label": "Мобильный партнёр"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "isMobile"
    },
    {
      "meta": {
        "label": "Название"
      },
      "type": null,
      "groupName": null,
      "name": "name"
    },
    {
      "meta": {
        "label": "Код"
      },
      "type": null,
      "groupName": null,
      "name": "code"
    },
    {
      "meta": {
        "label": "Город",
        "dictionaryName": "DealerCities"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "city"
    },
    {
      "meta": {
        "dictionaryName": "CarMakers",
        "required": true,
        "bounded": true,
        "label": "Обслуживаемые марки"
      },
      "type": "dictionary-many",
      "groupName": null,
      "name": "makes"
    },
    {
      "meta": {
         "label": "Телефоны",
         "jsonSchema": "dict-objects",
         "dictionaryName": "PhoneTypes",
         "noteLabel": "Время работы",
         "showNote": true,
         "regexp": "phone",
         "addLabel": "Добавить телефон и время работы",
         "widget": "dict-objects"
      },
      "type": "json",
      "name": "phones"
    },
    {
      "meta": {
        "label": "Адреса",
        "jsonSchema": "dict-objects",
        "dictionaryName": "AddressTypes",
        "addLabel": "Добавить адрес",
        "widget": "dict-objects"
      },
      "type": "json",
      "name": "addrs"
    },
    {
      "meta": {
        "label": "E-mail",
        "jsonSchema": "dict-objects",
        "dictionaryName": "EmailTypes",
        "regexp": "email",
        "addLabel": "Добавить e-mail",
        "widget": "dict-objects"
      },
      "type": "json",
      "name": "emails"
    },
    {
      "meta": {
        "label": "Ответственное лицо"
      },
      "type": null,
      "groupName": null,
      "name": "personInCharge"
    },
    {
      "meta": {
        "dictionaryName": "TaxSchemes",
        "label": "Форма налогообложения"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "taxScheme"
    },
    {
      "meta": {
        "label": "Соглашение о вознаграждении"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "isPayBackConfirmed"
    },
    {
      "meta": {
        "invisible": true
      },
      "type": "datetime",
      "groupName": null,
      "name": "mtime"
    },
    {
      "meta": {
        "reference-widget": "partner_services",
        "model": "partner_service",
        "reference-label": "Добавить услугу"
      },
      "type": "reference",
      "groupName": null,
      "name": "services"
    },
    {
      "meta": {
        "label": "Комментарий"
      },
      "type": "textarea",
      "groupName": null,
      "name": "comment"
    }
  ],
  "applications": [],
  "canDelete": [
    "parguy"
  ],
  "canUpdate": [
    "parguy"
  ],
  "canRead": true,
  "canCreate": true,
  "title": "Партнёр",
  "name": "partner"
}
