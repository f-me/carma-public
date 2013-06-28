{
  "fields": [
    {
      "meta": {
        "label": "Имя шаблона"
      },
      "canWrite": true,
      "canRead": true,
      "name": "name"
    },
    {
      "meta": {
        "label": "Текст"
      },
      "type": "textarea",
      "canWrite": true,
      "canRead": true,
      "name": "text"
    },
    {
      "meta": {
        "label": "Кому отправлять",
        "dictionaryName": "SmsRecieverNames"
      },
      "type": "dictionary",
      "canWrite": true,
      "canRead": true,
      "name": "smsReciever"
    },
    {
      "meta": {
        "label": "Активный"
      },
      "canWrite": true,
      "canRead": true,
      "type": "checkbox",
      "name": "isActive"
    }
  ],
  "canDelete": true,
  "canUpdate": true,
  "canRead": true,
  "canCreate": true,
  "title": "Шаблон смс",
  "name": "smsTpl"
}
