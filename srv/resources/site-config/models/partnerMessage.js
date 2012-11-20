{
    "name": "partnerMessage",
    "title": "Сообщение для партнёра",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "fields": [
      {
        "name":"ctime",
        "meta": {
          "invisible":true
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
        "name":"partnerId",
        "canRead": true,
        "canWrite": true,
        "meta": {
            "label": "Партнёр"
        }
      },
      {
        "name":"message",
        "canRead": true,
        "canWrite": true,
        "meta": {
            "label": "Сообщение"
        }
      }
    ]
}
