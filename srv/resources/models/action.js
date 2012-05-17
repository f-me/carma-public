{
    "name": "action",
    "title": "Действие",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "fields": [
      {
        "name":"service",
        "canRead": true,
        "canWrite": true,
        "meta": {
          "invisible": true
        }
      },
      {
        "name":"case",
        "canRead": true,
        "canWrite": true,
        "meta": {
          "invisible": true
        }
      },
      {
        "name": "name",
        "canRead": true,
        "type": "dictionary",
        "meta": {
          "dictionaryName": "ActionNames",
          "invisible": true
        }
      },
      {
        "name": "description",
        "type": "statictext",
        "canRead": true
      },
      {
        "name": "duetime",
        "type": "datetime",
        "canRead": true,
        "meta": {
          "label": "Ожидаемое время выполнения"
        }
      },
      {
        "name": "comment",
        "type": "textarea",
        "canRead": ["front","back","head"],
        "canWrite":["back","head"],
        "meta": {
          "label": "Комментарий"
        }
      },
      {
        "name": "result",
        "canRead": ["front","back","head"],
        "canWrite":["back","head"], 
        "type": "dictionary",
        "meta": {
          "addClass": "redirectOnChange",
          "label": "Результат",
          "dictionaryName": "ActionResults",
          "dictionaryParent": "name"
        }
      },
      {
        "name": "ctime",
        "type": "datetime",
        "canRead": true,
        "meta": {
          "invisible": true
        }
      },
      {
        "name": "mtime",
        "type": "datetime",
        "canRead": true,
        "meta": {
          "invisible": true
        }
      },
      {
        "name": "targetGroup",
        "canRead": ["front","back","head"],
        "canWrite":["back","head"], 
        "index": true,
        "meta": {
          "invisible": true
        }
      },
      {
        "name": "assignedTo",
        "canRead": ["front","back","head"],
        "canWrite":["back","head"], 
        "meta": {
          "invisible": true
        }
      }
    ]
}
