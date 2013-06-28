{
  "fields": [
    {
      "meta": {
        "label": "Дата создания записи",
        "invisible": true
      },
      "type": "datetime",
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "ctime"
    },
    {
      "meta": {
        "label": "Активен"
      },
      "canWrite": true,
      "canRead": true,
      "type": "checkbox",
      "name": "isActive"
    },
    {
      "meta": {
        "label": "Программа",
        "invisible": true
      },
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "program"
    },
    {
      "meta": {
        "regexp": "vin",
        "transform": "uppercase",
        "label": "VIN",
        "required": true
      },
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "carVin"
    },
    {
      "meta": {
        "label": "Марка",
        "bounded": true,
        "dictionaryName": "CarMakers",
        "required": true
      },
      "type": "dictionary",
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "carMake"
    },
    {
      "meta": {
        "label": "Модель",
        "bounded": true,
        "dictionaryParent": "carMake",
        "dictionaryName": "CarModels",
        "required": true
      },
      "type": "dictionary",
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "carModel"
    },
    {
      "meta": {
        "label": "Дилер, продавший автомобиль",
        "required": true,
        "bounded": true,
        "dictionaryType": "DealersDict"
      },
      "type": "dictionary",
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "carSeller"
    },
    {
      "meta": {
        "regexp": "plateNum",
        "required": true,
        "label": "Госномер",
        "transform": "uppercase"
      },
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "carPlateNum"
    },
    {
      "meta": {
        "sqltype": "integer",
        "label": "Год производства автомобиля",
        "required": true
      },
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "carMakeYear"
    },
    {
      "meta": {
        "label": "Цвет",
        "bounded": true,
        "dictionaryName": "Colors",
        "required": true
      },
      "type": "dictionary",
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "carColor"
    },
    {
      "meta": {
        "label": "Дата покупки",
        "regexp": "date",
        "required": true
      },
      "type": "date",
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "carBuyDate"
    },
    {
      "meta": {
        "label": "Дата последнего ТО",
        "regexp": "date",
        "required": true
      },
      "type": "date",
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "carCheckupDate"
    },
    {
      "meta": {
        "label": "Дилер у которого проходило последнее ТО",
        "bounded": true,
        "dictionaryType": "DealersDict",
        "required": true
      },
      "type": "dictionary",
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "carDealerTO"
    },
    {
      "meta": {
        "sqltype": "integer",
        "label": "Пробег на последнем ТО",
        "required": true
      },
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "carCheckupMilage"
    },
    {
      "meta": {
        "widget": "radio",
        "dictionaryName": "Transmission",
        "label": "Коробка передач",
        "required": true
      },
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "type": "dictionary",
      "name": "carTransmission"
    },
    {
      "meta": {
        "widget": "radio",
        "dictionaryName": "EngineType",
        "label": "Тип двигателя",
        "required": true
      },
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "type": "dictionary",
      "name": "carEngine"
    },
    {
      "meta": {
        "label": "Тип контракта",
        "required": true
      },
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "contractType"
    },
    {
      "meta": {
        "label": "Номер карты участника",
        "required": true
      },
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "cardNumber"
    },
    {
      "meta": {
        "koupdate": "change",
        "label": "Дата регистрации в программе",
        "regexp": "date",
        "required": true
      },
      "type": "date",
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "contractValidFromDate"
    },
    {
      "meta": {
        "label": "Программа действует до (дата)",
        "regexp": "date",
        "required": true
      },
      "type": "date",
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "contractValidUntilDate"
    },
    {
      "meta": {
        "koupdate": "change",
        "sqltype": "integer",
        "label": "Пробег при регистрации в программе",
        "required": true
      },
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "milageTO"
    },
    {
      "meta": {
        "sqltype": "integer",
        "label": "Программа действует до (пробег)",
        "required": true
      },
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "contractValidUntilMilage"
    },
    {
      "meta": {
        "label": "ФИО владельца карты",
        "required": true
      },
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "cardOwner"
    },
    {
      "meta": {
        "label": "Вид ТО",
        "bounded": true,
        "dictionaryName": "ToType",
        "required": true
      },
      "type": "dictionary",
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "techType"
    },
    {
      "meta": {
        "label": "Номер заказ-наряда",
        "required": true
      },
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "orderNumber"
    },
    {
      "meta": {
        "label": "ФИО менеджера",
        "required": true
      },
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "manager"
    },
    {
      "meta": {
        "label": "Дата начала действия программы",
        "regexp": "date",
        "required": true
      },
      "type": "date",
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "warrantyStart"
    },
    {
      "meta": {
        "label": "Дата окончания действия программы",
        "regexp": "date",
        "required": true
      },
      "type": "date",
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "warrantyEnd"
    },
    {
      "meta": {
        "label": "Комментарий"
      },
      "type": "textarea",
      "canWrite": [
        "partner"
      ],
      "canRead": [
        "partner"
      ],
      "name": "comment"
    },
    {
      "meta": {
        "label": "Дилер",
        "invisible": true
      },
      "canWrite": false,
      "canRead": [
        "partner"
      ],
      "name": "owner"
    },
    {
      "meta": {
        "widget": "checkbutton",
        "label": "Сохранить"
      },
      "canWrite": true,
      "canRead": true,
      "type": "checkbox",
      "name": "dixi"
    }
  ],
  "canDelete": false,
  "canUpdate": true,
  "canRead": false,
  "canCreate": true,
  "title": "contract",
  "name": "contract"
}
