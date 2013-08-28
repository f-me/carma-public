{
  "fields": [
    {
      "meta": {
        "label": "Название региона"
      },
      "type": null,
      "groupName": null,
      "name": "label"
    },
    {
      "meta": {
        "label": "Города",
        "dictionaryName": "DealerCities",
        "required": true,
        "bounded": true,
        "infoText": "city"
      },
      "type": "dictionary-many",
      "groupName": null,
      "name": "cities"
    }
  ],
  "applications": [],
  "canDelete": true,
  "canUpdate": true,
  "canRead": true,
  "canCreate": true,
  "title": "Регионы",
  "name": "region"
}
