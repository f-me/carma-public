{
    "name": "sms",
    "title": "смс",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "fields": [
      {
        "name":"ctime",
        "meta": {
          "invisible":true,
          "sqltype": "timestamp"
        }
      },
      {
        "name":"status",
        "meta": {
          "invisible":true,
          "sqltype":"text"
        }
      },
      {
        "name":"sentTime",
        "meta": {
          "invisible":true,
          "sqltype":"timestamp"
        }
      },
      {
        "name":"deliveredTime",
        "meta": {
          "invisible":true,
          "sqltype":"timestamp"
        }
      },
      {
        "name":"caseId",
        "canRead": true,
        "canWrite": true,
        "meta": {
            "label": "Кейс"
        }
      },
      {
        "name": "phone",
        "canRead": true,
        "canWrite": true,
        "meta": {
          "label": "Кому",
          "regexp": "phone"
        }
      },
      {
        "name": "template",
        "canRead": true,
        "canWrite": true,
        "type": "dictionary",
        "meta": {
          "dictionaryName": "smsTpl",
          "label": "Шаблон"
        }
      },
      {
        "name": "msg",
        "canRead": true,
        "canWrite": true,
        "type":"textarea",
        "meta": {
          "label": "Текст сообщения"
        }
      },
      {
        "name": "sender",
        "meta": {
          "invisible": true
        }
      }
    ]
}
