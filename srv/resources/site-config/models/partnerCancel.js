{
  "fields": [
    {
      "meta": {
        "sqltype": "timestamp",
        "invisible": true
      },
      "canWrite": true,
      "canRead": true,
      "name": "ctime"
    },
    {
      "meta": {
        "invisible": true
      },
      "canWrite": true,
      "canRead": true,
      "name": "caseId"
    },
    {
      "meta": {
        "invisible": true
      },
      "canWrite": true,
      "canRead": true,
      "name": "partnerId"
    },
    {
      "meta": {
        "invisible": true
      },
      "canWrite": true,
      "canRead": true,
      "name": "owner"
    },
    {
      "meta": {
        "label": "Причина отказа партнера",
        "dictionaryName": "PartnerCancelReason"
      },
      "type": "dictionary",
      "canWrite": true,
      "canRead": true,
      "name": "partnerCancelReason"
    },
    {
      "meta": {
        "label": "Комментарий"
      },
      "type": "textarea",
      "canWrite": true,
      "canRead": true,
      "name": "comment"
    }
  ],
  "canDelete": true,
  "canUpdate": true,
  "canRead": true,
  "canCreate": true,
  "title": "Отказ партнера",
  "name": "partnerCancel"
}
