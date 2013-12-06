{
  "fields": [
    {
      "meta": {
        "invisible": true,
        "sqltype": "integer"
      },
      "type": null,
      "groupName": null,
      "name": "uid"
    },
    {
      "meta": {
        "label": "Активен"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "isActive"
    },
    {
      "meta": {
        "label": "ФИО пользователя"
      },
      "type": null,
      "groupName": null,
      "name": "realName"
    },
    {
      "meta": {
        "label": "Логин",
        "required": true
      },
      "type": null,
      "groupName": null,
      "name": "login"
    },
    {
      "meta": {
        "label": "Пароль",
        "nosql": true,
        "required": true
      },
      "type": "password",
      "groupName": null,
      "name": "password"
    },
    {
      "meta": {
        "label": "Роли в системе",
        "dictionaryName": "Roles",
        "bounded": true
      },
      "type": "dictionary-many",
      "groupName": null,
      "name": "roles"
    },
    {
      "meta": {
        "label": "Подпрограммы",
        "dictionaryName": "usermetaPrograms",
        "dictionaryType": "ComputedDict"
      },
      "type": "dictionary-many",
      "groupName": null,
      "name": "programs"
    },
    {
      "meta": {
        "label": "boCities",
        "dictionaryName": "DealerCities",
        "invisible": true
      },
      "type": "dictionary-many",
      "groupName": null,
      "name": "boCities"
    },
    {
      "meta": {
        "label": "boPrograms",
        "dictionaryName": "Programs",
        "invisible": true
      },
      "type": "dictionary-many",
      "groupName": null,
      "name": "boPrograms"
    },
    {
      "meta": {
        "label": "Дилер"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "isDealer"
    },
    {
      "meta": {
        "label": "Рабочий телефон"
      },
      "type": null,
      "groupName": null,
      "name": "workPhone"
    },
    {
      "meta": {
        "label": "Добавочный номер"
      },
      "type": null,
      "groupName": null,
      "name": "workPhoneSuffix"
    },
    {
      "meta": {
        "label": "Мобильный телефон"
      },
      "type": null,
      "groupName": null,
      "name": "mobilePhone"
    },
    {
      "meta": {
        "label": "Домашний телефон"
      },
      "type": null,
      "groupName": null,
      "name": "homePhone"
    },
    {
      "meta": {
        "regexp": "email",
        "label": "E-mail"
      },
      "type": null,
      "groupName": null,
      "name": "email"
    },
    {
      "meta": {
        "label": "День рождения"
      },
      "type": "date",
      "groupName": null,
      "name": "birthday"
    },
    {
      "meta": {
        "label": "Должность"
      },
      "type": null,
      "groupName": null,
      "name": "position"
    },
    {
      "meta": {
        "invisible": "true"
      },
      "type": "datetime",
      "groupName": null,
      "name": "lastactivity"
    },
    {
      "meta": {
        "invisible": "true"
      },
      "type": "datetime",
      "groupName": null,
      "name": "lastlogout"
    }
  ],
  "applications": [],
  "canDelete": [
    "admin",
    "head",
    "supervisor"
  ],
  "canUpdate": [
    "admin",
    "head",
    "supervisor"
  ],
  "canRead": true,
  "canCreate": [
    "admin",
    "head",
    "supervisor"
  ],
  "title": "Метаданные пользователя",
  "name": "usermeta"
}
