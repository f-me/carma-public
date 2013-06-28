{
  "fields": [
    {
      "meta": {
        "invisible": true
      },
      "canWrite": true,
      "canRead": true,
      "name": "parentId"
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
        "invisible": true,
        "dictionaryName": "ActionNames"
      },
      "type": "dictionary",
      "canRead": true,
      "name": "name"
    },
    {
      "canRead": true,
      "type": "statictext",
      "name": "description"
    },
    {
      "meta": {
        "readonly": true,
        "label": "Ожидаемое время выполнения"
      },
      "canWrite": true,
      "canRead": true,
      "type": "datetime",
      "name": "duetime"
    },
    {
      "meta": {
        "label": "Комментарий"
      },
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "account",
        "parguy"
      ],
      "canRead": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "account",
        "parguy"
      ],
      "type": "textarea",
      "name": "comment"
    },
    {
      "meta": {
        "dictionaryParent": "name",
        "dictionaryName": "ActionResults",
        "label": "Результат",
        "addClass": "redirectOnChange"
      },
      "type": "dictionary",
      "canWrite": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "account",
        "parguy"
      ],
      "canRead": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "account",
        "parguy"
      ],
      "name": "result"
    },
    {
      "meta": {
        "invisible": true
      },
      "canRead": true,
      "type": "datetime",
      "name": "ctime"
    },
    {
      "meta": {
        "invisible": true
      },
      "canRead": true,
      "type": "datetime",
      "name": "mtime"
    },
    {
      "meta": {
        "invisible": true
      },
      "canRead": true,
      "type": "datetime",
      "name": "openTime"
    },
    {
      "meta": {
        "invisible": true
      },
      "canRead": true,
      "type": "datetime",
      "name": "closeTime"
    },
    {
      "meta": {
        "dictionaryType": "BoUsersDict",
        "invisible": true,
        "label": "Ответственный"
      },
      "type": "dictionary",
      "canWrite": [
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman"
      ],
      "canRead": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "account",
        "parguy"
      ],
      "name": "assignedTo"
    },
    {
      "meta": {
        "dictionaryName": "Roles",
        "invisible": true,
        "label": "Роль"
      },
      "type": "dictionary",
      "canWrite": [
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman"
      ],
      "canRead": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "account",
        "parguy"
      ],
      "name": "targetGroup"
    },
    {
      "meta": {
        "invisible": true,
        "label": "Приоритет"
      },
      "canWrite": [
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman"
      ],
      "canRead": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "account",
        "parguy"
      ],
      "name": "priority"
    },
    {
      "meta": {
        "invisible": true,
        "label": "Закрыто"
      },
      "type": "checkbox",
      "canWrite": [
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman"
      ],
      "canRead": [
        "front",
        "back",
        "head",
        "supervisor",
        "director",
        "analyst",
        "vwfake",
        "parguy",
        "account",
        "admin",
        "programman",
        "account",
        "parguy"
      ],
      "name": "closed"
    }
  ],
  "canDelete": true,
  "canUpdate": true,
  "canRead": true,
  "canCreate": true,
  "title": "Действие",
  "name": "action"
}
