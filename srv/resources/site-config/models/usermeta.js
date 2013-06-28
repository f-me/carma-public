{
  "fields": [
    {
      "meta": {
        "sqltype": "integer",
        "invisible": true
      },
      "name": "uid"
    },
    {
      "meta": {
        "label": "Активен"
      },
      "type": "checkbox",
      "name": "isActive"
    },
    {
      "meta": {
        "label": "ФИО пользователя"
      },
      "name": "realName"
    },
    {
      "meta": {
        "required": true,
        "label": "Логин"
      },
      "name": "login"
    },
    {
      "meta": {
        "required": true,
        "nosql": true,
        "label": "Пароль"
      },
      "type": "password",
      "name": "password"
    },
    {
      "meta": {
        "dictionaryName": "Roles",
        "label": "Роль в системе"
      },
      "type": "dictionary-many",
      "name": "roles"
    },
    {
      "meta": {
        "dictionaryType": "ComputedDict",
        "dictionaryName": "allPrograms",
        "label": "Программы"
      },
      "type": "dictionary-many",
      "name": "programs"
    },
    {
      "meta": {
        "invisible": true,
        "dictionaryName": "DealerCities",
        "label": "boCities"
      },
      "type": "dictionary-many",
      "name": "boCities"
    },
    {
      "meta": {
        "invisible": true,
        "dictionaryName": "Programs",
        "label": "boPrograms"
      },
      "type": "dictionary-many",
      "name": "boPrograms"
    },
    {
      "meta": {
        "label": "Дилер"
      },
      "type": "checkbox",
      "name": "isDealer"
    },
    {
      "meta": {
        "label": "Рабочий телефон"
      },
      "name": "workPhone"
    },
    {
      "meta": {
        "label": "Добавочный номер"
      },
      "name": "workPhoneSuffix"
    },
    {
      "meta": {
        "label": "Мобильный телефон"
      },
      "name": "mobilePhone"
    },
    {
      "meta": {
        "label": "Домашний телефон"
      },
      "name": "homePhone"
    },
    {
      "meta": {
        "label": "E-mail",
        "regexp": "email"
      },
      "name": "email"
    },
    {
      "meta": {
        "label": "День рождения"
      },
      "type": "date",
      "name": "birthday"
    },
    {
      "meta": {
        "label": "Должность"
      },
      "name": "position"
    },
    {
      "meta": {
        "invisible": "true"
      },
      "type": "datetime",
      "name": "lastactivity"
    },
    {
      "meta": {
        "invisible": "true"
      },
      "type": "datetime",
      "name": "lastlogout"
    }
  ],
  "applications": [
    {
      "canRead": true,
      "canWrite": true,
      "targets": true
    }
  ],
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
