{
    "name": "action",
    "title": "Действие",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "fields": [
      {
        "name":"parentId",
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
        "canWrite": true,
        "meta": {
          "label": "Ожидаемое время выполнения",
    	  "readonly": true
        }
      },
      {
        "name": "comment",
        "type": "textarea",
        "canRead": ["front","back","head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman", "account", "parguy"],
        "canWrite":["front","back","head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman", "account", "parguy"],
        "meta": {
          "label": "Комментарий"
        }
      },
      {
        "name": "result",
        "canRead": ["front","back","head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman", "account", "parguy"],
        "canWrite":["front","back","head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman", "account", "parguy"], 
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
        "name": "openTime",
        "type": "datetime",
        "canRead": true,
        "meta": {
          "invisible": true
        }
      },
      {
        "name": "closeTime",
        "type": "datetime",
        "canRead": true,
        "meta": {
          "invisible": true
        }
      },
      {
        "name": "assignedTo",
        "canRead": ["front","back","head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman", "account", "parguy"],
        "canWrite":["back","head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman"], 
        "type": "dictionary",
        "meta": {
            "label": "Ответственный",
            "invisible": true,
            "dictionaryName": "users_with_roles: head, back, supervisor, parguy, account, analyst, op_checker, op_close, op_dealer"
        }
      },
      {
        "name": "targetGroup",
        "canRead": ["front","back","head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman", "account", "parguy"],
        "canWrite":["back","head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman"], 
        "type": "dictionary",
        "meta": {
            "label": "Роль",
            "invisible": true,
            "dictionaryName": "Roles"
        }
      },
      {
        "name": "priority",
        "canRead": ["front","back","head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman", "account", "parguy"],
        "canWrite":["back","head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman"], 
        "meta": {
            "label": "Приоритет",
            "invisible": true
        }
      },
      {
        "name": "closed",
        "canRead": ["front","back","head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman", "account", "parguy"],
        "canWrite":["back","head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman"], 
        "type": "checkbox",
        "meta": {
            "label": "Закрыто",
            "invisible": true
        }
      }
    ]
}
