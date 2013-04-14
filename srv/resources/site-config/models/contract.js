{
  "name": "contract",
  "title": "contract",
  "canCreate": true,
  "canRead": false,
  "canUpdate": true,
  "canDelete": false,
  "fields": [
    {
      "name": "ctime",
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "type": "datetime",
      "meta": {
        "invisible": true,
        "label": "Дата создания записи"
      }
    },
    {
        "name": "isActive",
        "type": "checkbox",
        "canRead":  true,
        "canWrite": true,
        "meta": {
            "label": "Активен"
        }
    },
    {
      "name": "program",
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "meta": {
        "invisible": true,
        "label": "Программа"
      }
    },
    {
      "name": "carVin",
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "meta": {
        "required": true,
        "label": "VIN",
        "transform": "uppercase",
        "regexp": "vin"
      }
    },
    {
      "name": "carSeller",
      "meta": {
        "required": true,
        "label": "Дилер, продавший автомобиль"
      }
    },
    {
      "name": "carMake",
      "canRead": ["ruslan","vwpartner"],
      "canWrite": ["ruslan"],
      "type": "dictionary",
      "meta": {
        "required": true,
        "dictionaryName": "CarMakers",
        "bounded": true,
        "label": "Марка"
      }
    },
    {
      "name": "carModel",
      "canRead": ["ruslan","vwpartner"],
      "canWrite": ["ruslan","vwpartner"],
      "type": "dictionary",
      "meta": {
        "required": true,
        "dictionaryName": "CarModels",
        "dictionaryParent": "carMake",
        "bounded": true,
        "label": "Модель"
      }
    },
    {
      "name": "carPlateNum",
      "canRead": ["ruslan"],
      "canWrite": ["ruslan"],
      "meta": {
        "transform": "uppercase",
        "label": "Госномер",
        "required": true,
        "regexp": "plateNum"
      }
    },
    {
      "name": "carMakeYear",
      "canRead": ["ruslan","vwpartner"],
      "canWrite": ["ruslan","vwpartner"],
      "meta": {
        "required": true,
        "label": "Год производства автомобиля",
        "sqltype": "integer"
      }
    },
    {
      "name": "carColor",
      "type": "dictionary",
      "meta": {
        "required": true,
        "dictionaryName": "Colors",
        "bounded": true,
        "label": "Цвет"
      }
    },
    {
      "name": "carBuyDate",
      "type": "date",
      "meta": {
        "required": true,
        "label": "Дата покупки"
      }
    },
    {
      "name": "carCheckupDate",
      "type": "date",
      "meta": {
        "required": true,
        "label": "Дата последнего ТО"
      }
    },
    {
      "name": "carDealerTO",
      "meta": {
        "required": true,
        "label": "Дилер у которого проходило последнее ТО"
      }
    },
    {
      "name": "carCheckupMilage",
      "canRead": ["vwpartner"],
      "canWrite": ["vwpartner"],
      "meta": {
        "required": true,
        "label": "Пробег на последнем ТО",
        "sqltype": "integer"
      }
    },
    {
      "name": "carTransmission",
      "type": "dictionary",
      "meta": {
        "required": true,
        "label": "Коробка передач",
        "dictionaryName": "Transmission",
        "required": true,
        "widget": "radio"
      }
    },
    {
      "name": "carEngine",
      "type": "dictionary",
      "meta": {
        "required": true,
        "label": "Тип двигателя",
        "dictionaryName": "EngineType",
        "widget": "radio"
      }
    },
    {
      "name": "carCheckPeriod",
      "canRead": ["ruslan"],
      "canWrite": ["ruslan"],
      "meta": {
        "required": true,
        "label": "Межсервисный интервал",
        "sqltype": "integer"
      }
    },
    {
      "name": "contractType",
      "meta": {
        "required": true,
        "label": "Тип контракта"
      }
    },
    {
      "name": "cardNumber",
      "canRead": ["vwpartner"],
      "canWrite": ["vwpartner"],
      "meta": {
        "required": true,
        "label": "Номер карты участника"
      }
    },
    {
      "name": "contractValidFromDate",
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "type": "date",
      "meta": {
        "required": true,
        "label": "Дата регистрации в программе"
      }
    },
    {
      "name": "contractValidUntilDate",
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "type": "date",
      "meta": {
        "required": true,
        "label": "Программа действует до (дата)"
      }
    },
    {
      "name": "milageTO",
      "canRead": ["ruslan"],
      "canWrite": ["ruslan"],
      "meta": {
        "required": true,
        "label": "Пробег при регистрации в программе",
        "sqltype": "integer"
      }
    },
    {
      "name": "contractValidUntilMilage",
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "meta": {
        "required": true,
        "label": "Программа действует до (пробег)",
        "sqltype": "integer"
      }
    },
    {
      "name": "cardOwner",
      "meta": {
        "required": true,
        "label": "ФИО владельца карты"
      }
    },
    {
      "name": "techType",
      "canRead": ["vwpartner"],
      "canWrite": ["vwpartner"],
      "type": "dictionary",
      "meta": {
        "required": true,
        "dictionaryName": "ToType",
        "bounded": true,
        "label": "Вид ТО"
      }
    },
    {
      "name": "orderNumber",
      "canRead": ["vwpartner"],
      "canWrite": ["vwpartner"],
      "meta": {
        "required": true,
        "label": "Номер заказ-наряда"
      }
    },
    {
      "name": "manager",
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "meta": {
        "required": true,
        "label": "ФИО менеджера"
      }
    },
    {
      "name": "warrantyStart",
      "type": "date",
      "meta": {
        "required": true,
        "label": "Дата начала действия программы"
      }
    },
    {
      "name": "warrantyEnd",
      "type": "date",
      "meta": {
        "required": true,
        "label": "Дата окончания действия программы"
      }
    },
    {
        "name": "comment",
        "canRead": true,
        "canWrite": true,
        "type": "textarea",
        "meta": {
            "label": "Комментарий",
            "required": true
        }
    },
    {
      "name": "dixi",
      "type": "checkbox",
      "canRead" : true,
      "canWrite": true,
      "meta": {
          "label": "Сохранить",
          "widget": "checkbutton"
      }
    }
  ]
}
