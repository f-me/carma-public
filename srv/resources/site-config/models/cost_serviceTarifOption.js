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
        "invisible": true
      },
      "canWrite": true,
      "canRead": true,
      "name": "tarifOptionId"
    },
    {
      "meta": {
        "readonly": true,
        "label": "Название опции"
      },
      "name": "optionName"
    },
    {
      "meta": {
        "readonly": true,
        "label": "Цена за единицу"
      },
      "name": "price"
    },
    {
      "meta": {
        "regexp": "number",
        "label": "Количество"
      },
      "name": "count"
    },
    {
      "meta": {
        "readonly": true,
        "label": "Стоимость"
      },
      "name": "cost"
    },
    {
      "meta": {
        "invisible": true,
        "label": "Стоимость за единицу за нал"
      },
      "name": "price1"
    },
    {
      "meta": {
        "invisible": true,
        "label": "Стоимость за единицу по безналу"
      },
      "name": "price2"
    }
  ],
  "applications": [
    {
      "canRead": true,
      "canWrite": true,
      "targets": true
    }
  ],
  "canDelete": true,
  "canUpdate": true,
  "canRead": true,
  "canCreate": true,
  "title": "Тарифная опция",
  "name": "cost_serviceTarifOption"
}
