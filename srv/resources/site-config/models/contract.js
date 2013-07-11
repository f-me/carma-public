{
  "fields": [
    {
      "meta": {
        "invisible": true,
        "label": "Дата создания записи"
      },
      "type": "datetime",
      "groupName": null,
      "name": "ctime"
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
        "invisible": true,
        "label": "Программа"
      },
      "type": null,
      "groupName": null,
      "name": "program"
    },
    {
      "meta": {
        "required": true,
        "label": "VIN",
        "transform": "uppercase",
        "regexp": "vin"
      },
      "type": null,
      "groupName": null,
      "name": "carVin"
    },
    {
      "meta": {
        "required": true,
        "dictionaryName": "CarMakers",
        "bounded": true,
        "label": "Марка"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "carMake"
    },
    {
      "meta": {
        "required": true,
        "dictionaryName": "CarModels",
        "dictionaryParent": "carMake",
        "bounded": true,
        "label": "Модель"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "carModel"
    },
    {
      "meta": {
        "dictionaryType": "DealersDict",
        "bounded": true,
        "required": true,
        "label": "Дилер, продавший автомобиль"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "carSeller"
    },
    {
      "meta": {
        "transform": "uppercase",
        "label": "Госномер",
        "required": true,
        "regexp": "plateNum"
      },
      "type": null,
      "groupName": null,
      "name": "carPlateNum"
    },
    {
      "meta": {
        "required": true,
        "label": "Год производства автомобиля",
        "sqltype": "integer"
      },
      "type": null,
      "groupName": null,
      "name": "carMakeYear"
    },
    {
      "meta": {
        "required": true,
        "dictionaryName": "Colors",
        "bounded": true,
        "label": "Цвет"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "carColor"
    },
    {
      "meta": {
        "required": true,
        "regexp": "date",
        "label": "Дата покупки"
      },
      "type": "date",
      "groupName": null,
      "name": "carBuyDate"
    },
    {
      "meta": {
        "required": true,
        "regexp": "date",
        "label": "Дата последнего ТО"
      },
      "type": "date",
      "groupName": null,
      "name": "carCheckupDate"
    },
    {
      "meta": {
        "required": true,
        "dictionaryType": "DealersDict",
        "bounded": true,
        "label": "Дилер у которого проходило последнее ТО"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "carDealerTO"
    },
    {
      "meta": {
        "required": true,
        "label": "Пробег на последнем ТО",
        "sqltype": "integer"
      },
      "type": null,
      "groupName": null,
      "name": "carCheckupMilage"
    },
    {
      "meta": {
        "required": true,
        "label": "Коробка передач",
        "dictionaryName": "Transmission",
        "widget": "radio"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "carTransmission"
    },
    {
      "meta": {
        "required": true,
        "label": "Тип двигателя",
        "dictionaryName": "EngineType",
        "widget": "radio"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "carEngine"
    },
    {
      "meta": {
        "required": true,
        "label": "Тип контракта"
      },
      "type": null,
      "groupName": null,
      "name": "contractType"
    },
    {
      "meta": {
        "required": true,
        "label": "Номер карты участника"
      },
      "type": null,
      "groupName": null,
      "name": "cardNumber"
    },
    {
      "meta": {
        "required": true,
        "regexp": "date",
        "label": "Дата регистрации в программе",
        "koupdate": "change"
      },
      "type": "date",
      "groupName": null,
      "name": "contractValidFromDate"
    },
    {
      "meta": {
        "required": true,
        "regexp": "date",
        "label": "Программа действует до (дата)"
      },
      "type": "date",
      "groupName": null,
      "name": "contractValidUntilDate"
    },
    {
      "meta": {
        "required": true,
        "label": "Пробег при регистрации в программе",
        "sqltype": "integer",
        "koupdate": "change"
      },
      "type": null,
      "groupName": null,
      "name": "milageTO"
    },
    {
      "meta": {
        "required": true,
        "label": "Программа действует до (пробег)",
        "sqltype": "integer"
      },
      "type": null,
      "groupName": null,
      "name": "contractValidUntilMilage"
    },
    {
      "meta": {
        "required": true,
        "label": "ФИО владельца карты"
      },
      "type": null,
      "groupName": null,
      "name": "cardOwner"
    },
    {
      "meta": {
        "required": true,
        "dictionaryName": "ToType",
        "bounded": true,
        "label": "Вид ТО"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "techType"
    },
    {
      "meta": {
        "required": true,
        "label": "Номер заказ-наряда"
      },
      "type": null,
      "groupName": null,
      "name": "orderNumber"
    },
    {
      "meta": {
        "required": true,
        "label": "ФИО менеджера"
      },
      "type": null,
      "groupName": null,
      "name": "manager"
    },
    {
      "meta": {
        "required": true,
        "regexp": "date",
        "label": "Дата начала действия программы"
      },
      "type": "date",
      "groupName": null,
      "name": "warrantyStart"
    },
    {
      "meta": {
        "required": true,
        "regexp": "date",
        "label": "Дата окончания действия программы"
      },
      "type": "date",
      "groupName": null,
      "name": "warrantyEnd"
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
        "invisible": true,
        "label": "Дилер"
      },
      "type": null,
      "groupName": null,
      "name": "owner"
    },
    {
      "meta": {
        "label": "Сохранить",
        "widget": "checkbutton"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "dixi"
    }
  ],
  "applications": [],
  "canDelete": false,
  "canUpdate": true,
  "canRead": false,
  "canCreate": true,
  "title": "contract",
  "name": "contract"
}
