{
  "fields": [
    {
      "meta": {
        "sqltype": "timestamp",
        "invisible": true
      },
      "name": "ctime"
    },
    {
      "meta": {
        "sqltype": "text",
        "invisible": true
      },
      "name": "status"
    },
    {
      "meta": {
        "sqltype": "timestamp",
        "invisible": true
      },
      "name": "sentTime"
    },
    {
      "meta": {
        "sqltype": "timestamp",
        "invisible": true
      },
      "name": "deliveredTime"
    },
    {
      "meta": {
        "label": "Кейс"
      },
      "canWrite": true,
      "canRead": true,
      "name": "caseId"
    },
    {
      "meta": {
        "regexp": "phone",
        "label": "Кому"
      },
      "canWrite": true,
      "canRead": true,
      "name": "phone"
    },
    {
      "meta": {
        "label": "Шаблон",
        "dictionaryName": "smsTpl"
      },
      "type": "dictionary",
      "canWrite": true,
      "canRead": true,
      "name": "template"
    },
    {
      "meta": {
        "label": "Текст сообщения"
      },
      "type": "textarea",
      "canWrite": true,
      "canRead": true,
      "name": "msg"
    },
    {
      "meta": {
        "invisible": true
      },
      "name": "sender"
    }
  ],
  "canDelete": true,
  "canUpdate": true,
  "canRead": true,
  "canCreate": true,
  "title": "смс",
  "name": "sms"
}
