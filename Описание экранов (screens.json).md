Файл с описанием прав: srv/resources/site-config/screens.json

Пример:

~~~
{ "screens" : [
    { "name"  : "call",
      "label" : "Звонок",
      "type"  : "li",
      "permissions": ["front","back","head","parguy"]
    },
    { "name"  :"case",
      "label" : "Кейс",
      "type"  : "li",
      "permissions": ["front","back","head","parguy"]
    },
    { "type"    : "dropdown",
      "label"   : "Еще",
      "permissions":["front","back","head","parguy"],
      "screens" : [
        { "name"  : "vin",
          "label" : "Обновление базы VIN",
          "type"  : "li",
          "permissions": ["front","back","head","parguy"]
        },
        { "name"  : "partner",
          "label" : "Редактирование партнёров",
          "type"  : "li",
          "permissions": ["front","back","head","parguy"]
        }
      ]
    }
  ]
}
~~~

Как видно, каждая запись состоит из обязательных label, permission, type.
Допустимые значение `type`:
- `li`, тогда поле name обязательно, оно будет определять id и из него же будет формироваться ссылка на экран.
- `dropdown`, то у него обязательно должно быть поле screens,  содержащее произвольные записи.
- `link` (в этом случае `name` содержит в себе URL, который используется в пункте меню как есть)
- `hack` (в `name` указывается название клиентского хака из `lib/hacking`, который переключается данным пунктом)
- `sms` — спецтип для ссылки на окошко для отправки SMS