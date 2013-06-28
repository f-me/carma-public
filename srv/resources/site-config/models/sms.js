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
        "invisible": true,
        "sqltype": "text"
      },
      "type": null,
      "groupName": null,
      "name": "status"
    },
    {
      "meta": {
        "invisible": true,
        "sqltype": "timestamp"
      },
      "type": null,
      "groupName": null,
      "name": "sentTime"
    },
    {
      "meta": {
        "invisible": true,
        "sqltype": "timestamp"
      },
      "type": null,
      "groupName": null,
      "name": "deliveredTime"
    },
    {
      "meta": {
        "label": "Кейс"
      },
      "type": null,
      "groupName": null,
      "name": "caseId"
    },
    {
      "meta": {
        "label": "Кому",
        "regexp": "phone"
      },
      "type": null,
      "groupName": null,
      "name": "phone"
    },
    {
      "meta": {
        "dictionaryName": "smsTpl",
        "label": "Шаблон"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "template"
    },
    {
      "meta": {
        "label": "Текст сообщения"
      },
      "type": "textarea",
      "groupName": null,
      "name": "msg"
    },
    {
      "meta": {
        "invisible": true
      },
      "type": null,
      "groupName": null,
      "name": "sender"
    }
  ],
  "applications": [],
  "canDelete": true,
  "canUpdate": true,
  "canRead": true,
  "canCreate": true,
  "title": "смс",
  "name": "sms"
}
