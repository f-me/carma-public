{
  "name": "partnerCancel",
  "title": "Отказ партнера",
  "canCreate": true,
  "canRead": true,
  "canUpdate": true,
  "canDelete": true,
  "fields": [
    {
      "name": "ctime",
      "canRead": true,
      "canWrite": true,
      "meta": {
        "invisible": true,
        "sqltype": "timestamp"
      }
    },
    {
      "name": "caseId",
      "canRead": true,
      "canWrite": true,
      "meta": {
        "invisible": true
      }
    },
    {
      "name": "partnerId",
      "canRead": true,
      "canWrite": true,
      "meta": {
        "invisible": true
      }
    },
    {
      "name": "owner",
      "canRead": true,
      "canWrite": true,
      "meta": {
        "invisible": true
      }
    },
    {
      "name": "partnerCancelReason",
      "canRead": true,
      "canWrite": true,
      "type": "dictionary",
      "meta": {
        "dictionaryName": "ClientCancelReason",
        "label": "Причина отказа партнера"
      }
    },
    {
      "name": "comment",
      "canRead": true,
      "canWrite": true,
      "type": "textarea",
      "meta": {
        "label": "Комментарий"
      }
    }
  ]
}
