{
    "name": "action",
    "title": "Действие",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "fields": [
      {
        "name":"serviceId",
        "canRead": true,
        "canWrite": true,
        "meta": {
          "invisible": true
        }
      },
      {
        "name":"caseId",
        "canRead": true,
        "canWrite": true,
        "meta": {
          "invisible": true
        }
      },
      {
        "name": "result",
        "label": "Результат действия",
        "type": "dictionary"
      },
      {
        "name": "ctime",
        "type": "datetime",
        "meta": {
          "invisible": true
        }
      },
      {
        "name": "mtime",
        "type": "datetime"
      },
      {
        "name": "duetime",
        "type": "datetime"
      },
      {
        "name": "targetGroup",
        "index": true
      },
      {
        "name": "assignedTo",
        "index": true
      }
    ]
}
