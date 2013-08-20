{
  "fields": [
    {
      "meta": {
        "label": "Дата звонка",
        "readonly": true
      },
      "type": "datetime",
      "groupName": null,
      "name": "callDate"
    },
    {
      "meta": {
        "label": "Дата звонка"
      },
      "type": "datetime",
      "groupName": null,
      "name": "vwcreatedate"
    },
    {
      "meta": {
        "label": "Сотрудник РАМК",
        "readonly": true
      },
      "type": null,
      "groupName": null,
      "name": "callTaker"
    },
    {
      "meta": {
        "dictionaryName": "Wazzup",
        "label": "Что случилось",
        "required": true,
        "infoText": "comment"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "comment"
    },
    {
      "meta": {
        "dictionaryName": "Diagnosis1",
        "label": "Система",
        "required": true,
        "infoText": "system"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "diagnosis1"
    },
    {
      "meta": {
        "dictionaryName": "Diagnosis2",
        "dictionaryParent": "diagnosis1",
        "label": "Узел/деталь",
        "infoText": "detail"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "diagnosis2"
    },
    {
      "meta": {
        "dictionaryName": "Diagnosis3",
        "label": "Описание причины неисправности",
        "infoText": "diagnosis3"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "diagnosis3"
    },
    {
      "meta": {
        "dictionaryName": "Diagnosis4",
        "label": "Рекомендация",
        "infoText": "recomendation"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "diagnosis4"
    },
    {
      "meta": {
        "label": "Клиент",
        "required": true
      },
      "type": null,
      "groupName": "carContact",
      "name": "contact"
    },
    {
      "meta": {
        "dictionaryName": "Programs",
        "label": "Программа",
        "required": true,
        "bounded": true,
        "targetCategory": "program",
        "infoText": "program"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "program"
    },
    {
      "meta": null,
      "type": null,
      "groupName": "car",
      "name": "car"
    },
    {
      "meta": null,
      "type": null,
      "groupName": "cardNumber",
      "name": "cardNumber"
    },
    {
      "meta": {
        "dictionaryName": "VINChecked",
        "label": "Участие в программе",
        "required": true,
        "bounded": true,
        "infoText": "vinChecked"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "vinChecked"
    },
    {
      "meta": {
        "dictionaryName": "DealerCities",
        "label": "Город",
        "required": true,
        "bounded": true,
        "infoText": "city"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "city"
    },
    {
      "meta": null,
      "type": null,
      "groupName": "address",
      "name": "caseAddress"
    },
    {
      "meta": {
        "label": "Температура",
        "infoText": "temperature"
      },
      "type": null,
      "groupName": null,
      "name": "temperature"
    },
    {
      "meta": {
        "label": "Дата починки"
      },
      "type": "datetime",
      "groupName": null,
      "name": "repair"
    },
    {
      "meta": {
        "label": "Номер согласования"
      },
      "type": null,
      "groupName": null,
      "name": "accord"
    },
    {
      "meta": {
        "label": "Неисправность со слов дилера/партнёра",
        "infoText": "dealerCause"
      },
      "type": "textarea",
      "groupName": null,
      "name": "dealerCause"
    },
    {
      "meta": {
        "required": true,
        "bounded": true,
        "dictionaryName": "CaseStatuses",
        "label": "Статус кейса"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "caseStatus"
    },
    {
      "meta": {
        "label": "Требуется выгрузка в PSA"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "psaExportNeeded"
    },
    {
      "meta": {
        "readonly": true,
        "label": "Выгружен в PSA"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "psaExported"
    },
    {
      "meta": {
        "label": "Претензия / Благодарность",
        "infoText": "claim"
      },
      "type": "textarea",
      "groupName": null,
      "name": "claim"
    },
    {
      "meta": {
        "label": "Услуги"
      },
      "type": "reference",
      "groupName": null,
      "name": "services"
    },
    {
      "meta": {
        "label": "Действия",
        "invisible": true
      },
      "type": "reference",
      "groupName": null,
      "name": "actions"
    },
    {
      "meta": {
        "invisible": true
      },
      "type": "json",
      "groupName": null,
      "name": "comments"
    },
    {
      "meta": {
        "label": "Прикрепленные файлы",
        "widget": "inline-uploader"
      },
      "type": "reference",
      "groupName": null,
      "name": "files"
    }
  ],
  "applications": [
    {
      "meta": {
        "targetMap": "caseAddress_map",
        "targetCoords": "caseAddress_coords"
      },
      "targets": [
        "caseAddress_address"
      ]
    },
    {
      "meta": {
        "targetMap": "caseAddress_map",
        "targetAddr": "caseAddress_address",
        "cityField": "city"
      },
      "targets": [
        "caseAddress_coords"
      ]
    },
    {
      "meta": {
        "targetAddr": "caseAddress_address",
        "targetCoords": "caseAddress_coords",
        "cityField": "city"
      },
      "targets": [
        "caseAddress_map"
      ]
    },
    {
      "meta": {
        "label": "Адрес места поломки",
        "infoText": "caseAddress"
      },
      "targets": [
        "caseAddress_address"
      ]
    },
    {
      "meta": {
        "label": "Звонящий"
      },
      "targets": [
        "contact_name"
      ]
    },
    {
      "meta": {
        "label": "Карта участника",
        "infoText": "cardnum"
      },
      "targets": [
        "cardNumber_cardNumber"
      ]
    },
    {
      "meta": {
        "dictionaryParent": "car_make"
      },
      "targets": [
        "car_model"
      ]
    },
    {
      "meta": {
        "mainToo": true,
        "infoText": "platenum"
      },
      "targets": [
        "car_plateNum"
      ]
    },
    {
      "meta": {
        "mainToo": true,
        "infoText": "owner"
      },
      "targets": [
        "contact_contactOwner"
      ]
    }
  ],
  "canDelete": true,
  "canUpdate": true,
  "canRead": true,
  "canCreate": true,
  "title": "Кейс",
  "name": "case"
}
