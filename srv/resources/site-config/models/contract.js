{
  "name": "contract",
  "title": "contract",
  "canCreate": true,
  "canRead": true,
  "canUpdate": true,
  "canDelete": false,
  "fields": [
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
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "meta": {
        "required": true,
        "label": "Дилер, продавший автомобиль"
      }
    },
    {
      "name": "carMake",
      "canRead": ["partner"],
      "canWrite": ["partner"],
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
      "canRead": ["partner"],
      "canWrite": ["partner"],
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
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "meta": {
        "required": true,
        "transform": "uppercase",
        "label": "Госномер",
        "required": true,
        "regexp": "plateNum"
      }
    },
    {
      "name": "carMakeYear",
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "meta": {
        "required": true,
        "label": "Год производства автомобиля",
        "sqltype": "integer"
      }
    },
    {
      "name": "carColor",
      "canRead": ["partner"],
      "canWrite": ["partner"],
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
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "type": "date",
      "meta": {
        "required": true,
        "label": "Дата покупки"
      }
    },
    {
      "name": "carCheckupDate",
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "type": "date",
      "meta": {
        "required": true,
        "label": "Дата последнего ТО"
      }
    },
    {
      "name": "carDealerTO",
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "meta": {
        "required": true,
        "label": "Дилер у которого проходило последнее ТО"
      }
    },
    {
      "name": "carCheckupMileage",
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "meta": {
        "required": true,
        "label": "Пробег на последнем ТО",
        "sqltype": "integer"
      }
    },
    {
      "name": "carTransmission",
      "canRead": ["partner"],
      "canWrite": ["partner"],
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
      "canRead": ["partner"],
      "canWrite": ["partner"],
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
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "meta": {
        "required": true,
        "label": "Межсервисный интервал",
        "sqltype": "integer"
      }
    },
    {
      "name": "contractType",
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "meta": {
        "required": true,
        "label": "Тип контракта"
      }
    },
    {
      "name": "cardNumber",
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "type": "picker",
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
      "name": "milageTO",
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "meta": {
        "required": true,
        "label": "Пробег при регистрации в программе",
        "sqltype": "integer"
      }
    },
    {
      "name": "cardOwner",
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "meta": {
        "required": true,
        "label": "ФИО владельца карты"
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
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "type": "date",
      "meta": {
        "required": true,
        "label": "Дата начала гарантии"
      }
    },
    {
      "name": "warrantyEnd",
      "canRead": ["partner"],
      "canWrite": ["partner"],
      "type": "date",
      "meta": {
        "required": true,
        "label": "Дата окончания гарантии"
      }
    }
  ]
}
