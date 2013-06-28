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
        "label": "Приоритет за нал"
      },
      "name": "priority1"
    },
    {
      "meta": {
        "label": "Приоритет по безналу город"
      },
      "name": "priority2"
    },
    {
      "meta": {
        "label": "Приоритет по безналу за город"
      },
      "name": "priority3"
    },
    {
      "meta": {
        "label": "Услуга",
        "dictionaryName": "Services"
      },
      "type": "dictionary",
      "name": "serviceName"
    },
    {
      "meta": {
        "label": "Процент за ложный вызов"
      },
      "name": "falseCallPercent"
    },
    {
      "type": "reference",
      "name": "tarifOptions"
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
  "title": "Тариф",
  "name": "partner_service"
}
