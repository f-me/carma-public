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
        "label": "Юридический адрес"
      },
      "type": null,
      "groupName": null,
      "name": "addrDeJure"
    },
    {
      "meta": {
        "label": "Фактический адрес"
      },
      "type": null,
      "groupName": null,
      "name": "addrDeFacto"
    },
    {
      "meta": {
        "widget": "picker",
        "label": "Координаты фактического адреса",
        "infoText": "coords",
        "picker": "mapPicker",
        "targetAddr": "addrDeFacto",
        "targetCoords": "coords",
        "cityField": "city",
        "currentBlipType": "partner"
      },
      "type": "coords",
      "groupName": null,
      "name": "coords"
    },
    {
      "meta": {
        "label": "Время работы"
      },
      "type": null,
      "groupName": null,
      "name": "workingTime"
    },
    {
      "meta": {
        "label": "Телефоны диспетчерской"
      },
      "type": null,
      "groupName": null,
      "name": "phone1"
    },
    {
      "meta": {
        "label": "Факс"
      },
      "type": null,
      "groupName": null,
      "name": "fax"
    },
    {
      "meta": {
        "label": "Телефон для закрытия заявок"
      },
      "type": null,
      "groupName": null,
      "name": "closeTicketPhone"
    },
    {
      "meta": {
        "label": "Email для закрытия заявок",
        "regexp": "email"
      },
      "type": null,
      "groupName": null,
      "name": "closeTicketEmail"
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
        "label": "Адрес сервисного отдела"
      },
      "type": null,
      "groupName": null,
      "name": "serviceAddress"
    },
    {
      "meta": {
        "label": "Телефон сервисного отдела",
        "regexp": "phone"
      },
      "type": null,
      "groupName": null,
      "name": "servicePhone"
    },
    {
      "meta": {
        "label": "Время работы сервисного отдела"
      },
      "type": null,
      "groupName": null,
      "name": "serviceWorking"
    },
    {
      "meta": {
        "label": "Адрес отдела продаж"
      },
      "type": null,
      "groupName": null,
      "name": "salesAddress"
    },
    {
      "meta": {
        "label": "Телефон отдела продаж",
        "regexp": "phone"
      },
      "type": null,
      "groupName": null,
      "name": "salesPhone"
    },
    {
      "meta": {
        "label": "Время работы отдела продаж"
      },
      "type": null,
      "groupName": null,
      "name": "salesWorking"
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
        "add-reference-field-label": "Услуги",
        "add-reference-fn": "showPartnServRefTarget",
        "add-reference-btn-label": "Добавить услугу"
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
