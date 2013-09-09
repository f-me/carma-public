{
  "fields": [
    {
      "meta": {
        "invisible": true
      },
      "type": null,
      "groupName": null,
      "name": "parentId"
    },
    {
      "meta": {
        "invisible": true
      },
      "type": null,
      "groupName": null,
      "name": "caseId"
    },
    {
      "meta": {
        "dictionaryName": "ActionNames",
        "invisible": true
      },
      "type": "dictionary",
      "groupName": null,
      "name": "name"
    },
    {
      "meta": null,
      "type": "statictext",
      "groupName": null,
      "name": "description"
    },
    {
      "meta": {
        "label": "Ожидаемое время выполнения",
        "readonly": true
      },
      "type": "datetime",
      "groupName": null,
      "name": "duetime"
    },
    {
      "meta": {
        "label": "Комментарий"
      },
      "type": "textarea",
      "groupName": null,
      "name": "comment"
    },
    {
      "meta": {
        "label": "Отложить на",
        "dictionaryName": "DeferTimes",
        "regexp": "timespan"
      },
      "type": "dictionary",
      "name": "deferBy"
    },
    {
      "meta": {
        "addClass": "redirectOnChange",
        "label": "Результат",
        "dictionaryName": "ActionResults",
        "dictionaryParent": "name"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "result"
    },
    {
      "meta": {
        "invisible": true
      },
      "type": "datetime",
      "groupName": null,
      "name": "ctime"
    },
    {
      "meta": {
        "invisible": true
      },
      "type": "datetime",
      "groupName": null,
      "name": "mtime"
    },
    {
      "meta": {
        "invisible": true
      },
      "type": "datetime",
      "groupName": null,
      "name": "openTime"
    },
    {
      "meta": {
        "invisible": true
      },
      "type": "datetime",
      "groupName": null,
      "name": "closeTime"
    },
    {
      "meta": {
        "label": "Ответственный",
        "invisible": true,
        "dictionaryType": "BoUsersDict"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "assignedTo"
    },
    {
      "meta": {
        "label": "Роль",
        "invisible": true,
        "dictionaryName": "Roles"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "targetGroup"
    },
    {
      "meta": {
        "label": "Приоритет",
        "invisible": true
      },
      "type": null,
      "groupName": null,
      "name": "priority"
    },
    {
      "meta": {
        "label": "Закрыто",
        "invisible": true
      },
      "type": "checkbox",
      "groupName": null,
      "name": "closed"
    }
  ],
  "applications": [],
  "canDelete": true,
  "canUpdate": true,
  "canRead": true,
  "canCreate": true,
  "title": "Действие",
  "name": "action"
}
