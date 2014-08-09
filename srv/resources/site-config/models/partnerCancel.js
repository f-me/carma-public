{
  "fields": [
    {
      "meta": {
        "invisible": true,
        "sqltype": "timestamp"
      },
      "type": null,
      "groupName": null,
      "name": "ctime"
    },
    {
      "meta": {
        "invisible": true
      },
      "type": null,
      "groupName": null,
      "name": "caseId"
    },
    {
      "meta": {
        "invisible": true
      },
      "type": null,
      "groupName": null,
      "name": "partnerId"
    },
    {
      "meta": {
        "invisible": true
      },
      "name": "serviceType"
    },
    {
      "meta": {
        "invisible": true
      },
      "type": null,
      "groupName": null,
      "name": "serviceId"
    },
    {
      "meta": {
        "invisible": true
      },
      "type": null,
      "groupName": null,
      "name": "owner"
    },
    {
      "meta": {
        "dictionaryName": "PartnerRefusalReason",
        "dictionaryType": "ModelDict",
        "bounded": "false",
        "dictionaryLabel": "label",
        "dictionaryKey": "label",
        "label": "Причина отказа партнера"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "partnerCancelReason"
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
  "canDelete": true,
  "canUpdate": true,
  "canRead": true,
  "canCreate": true,
  "title": "Отказ партнера",
  "name": "partnerCancel"
}
