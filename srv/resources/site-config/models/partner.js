{
  "fields": [
    {
      "meta": {
        "label": "Партнёр активен"
      },
      "type": "checkbox",
      "name": "isActive"
    },
    {
      "meta": {
        "label": "Дилер"
      },
      "type": "checkbox",
      "name": "isDealer"
    },
    {
      "meta": {
        "label": "Мобильный партнёр"
      },
      "type": "checkbox",
      "name": "isMobile"
    },
    {
      "meta": {
        "label": "Название"
      },
      "index": true,
      "name": "name"
    },
    {
      "meta": {
        "label": "Код"
      },
      "name": "code"
    },
    {
      "meta": {
        "dictionaryName": "DealerCities",
        "label": "Город"
      },
      "type": "dictionary",
      "name": "city"
    },
    {
      "type": "dictionary-many",
      "meta": {
        "label": "Обслуживаемые марки",
        "bounded": true,
        "required": true,
        "dictionaryName": "CarMakers"
      },
      "name": "makes"
    },
    {
      "meta": {
        "label": "Юридический адрес"
      },
      "name": "addrDeJure"
    },
    {
      "meta": {
        "label": "Фактический адрес"
      },
      "name": "addrDeFacto"
    },
    {
      "meta": {
        "currentBlipType": "partner",
        "cityField": "city",
        "targetCoords": "coords",
        "targetAddr": "addrDeFacto",
        "picker": "mapPicker",
        "infoText": "coords",
        "label": "Координаты фактического адреса",
        "widget": "picker"
      },
      "type": "coords",
      "name": "coords"
    },
    {
      "meta": {
        "label": "Время работы"
      },
      "name": "workingTime"
    },
    {
      "meta": {
        "label": "Телефоны диспетчерской"
      },
      "name": "phone1"
    },
    {
      "meta": {
        "label": "Факс"
      },
      "name": "fax"
    },
    {
      "meta": {
        "label": "Телефон для закрытия заявок"
      },
      "name": "closeTicketPhone"
    },
    {
      "meta": {
        "regexp": "email",
        "label": "Email для закрытия заявок"
      },
      "name": "closeTicketEmail"
    },
    {
      "meta": {
        "label": "Ответственное лицо"
      },
      "name": "personInCharge"
    },
    {
      "meta": {
        "label": "Адрес сервисного отдела"
      },
      "name": "serviceAddress"
    },
    {
      "meta": {
        "regexp": "phone",
        "label": "Телефон сервисного отдела"
      },
      "name": "servicePhone"
    },
    {
      "meta": {
        "label": "Время работы сервисного отдела"
      },
      "name": "serviceWorking"
    },
    {
      "meta": {
        "label": "Адрес отдела продаж"
      },
      "name": "salesAddress"
    },
    {
      "meta": {
        "regexp": "phone",
        "label": "Телефон отдела продаж"
      },
      "name": "salesPhone"
    },
    {
      "meta": {
        "label": "Время работы отдела продаж"
      },
      "name": "salesWorking"
    },
    {
      "meta": {
        "label": "Форма налогообложения",
        "dictionaryName": "TaxSchemes"
      },
      "type": "dictionary",
      "name": "taxScheme"
    },
    {
      "meta": {
        "label": "Соглашение о вознаграждении"
      },
      "type": "checkbox",
      "name": "isPayBackConfirmed"
    },
    {
      "meta": {
        "invisible": true
      },
      "canRead": true,
      "type": "datetime",
      "name": "mtime"
    },
    {
      "type": "reference",
      "name": "services"
    },
    {
      "type": "textarea",
      "meta": {
        "label": "Комментарий"
      },
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "canRead": [
        "partner",
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "parguy"
      ],
      "name": "comment"
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
