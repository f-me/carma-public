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
        "label": "Дата создания услуги",
        "readonly": true
      },
      "type": "datetime",
      "groupName": null,
      "name": "createTime"
    },
    {
      "meta": {
        "dictionaryName": "PaymentTypes",
        "bounded": true,
        "label": "Тип оплаты"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "payType"
    },
    {
      "meta": null,
      "type": null,
      "groupName": "payment",
      "name": "payment"
    },
    {
      "meta": null,
      "type": null,
      "groupName": "times",
      "name": "times"
    },
    {
      "meta": {
        "dictionaryName": "FalseStatuses",
        "bounded": true,
        "label": "Ложный вызов",
        "infoText": "falsecall"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "falseCall"
    },
    {
      "meta": {
        "dictionaryName": "ClientCancelReason",
        "label": "Причина отказа клиента"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "clientCancelReason"
    },
    {
      "meta": {
        "invisible": true
      },
      "type": null,
      "groupName": null,
      "name": "falseCallPercent"
    },
    {
      "meta": null,
      "type": null,
      "groupName": "bill",
      "name": "bill"
    },
    {
      "meta": {
        "dictionaryName": "TowerTypes",
        "bounded": true,
        "label": "Тип эвакуатора"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "towerType"
    },
    {
      "meta": {
        "dictionaryName": "TowTypes",
        "bounded": true,
        "label": "Вид эвакуации"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "towType"
    },
    {
      "meta": {
        "label": "Случай вандализма"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "vandalism"
    },
    {
      "meta": {
        "label": "ДТП"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "accident"
    },
    {
      "meta": null,
      "type": null,
      "groupName": "address",
      "name": "caseAddress"
    },
    {
      "meta": {
        "label": "Дилер"
      },
      "type": null,
      "groupName": "partner",
      "name": "towDealer"
    },
    {
      "meta": {
        "readonly": true,
        "label": "Расстояние до дилера",
        "distanceTo1": "case-form/caseAddress_coords",
        "distanceTo2": "towAddress_coords"
      },
      "type": null,
      "groupName": null,
      "name": "dealerDistance"
    },
    {
      "meta": {
        "label": "Адрес доставки"
      },
      "type": null,
      "groupName": "address",
      "name": "towAddress"
    },
    {
      "meta": {
        "label": "Партнёр"
      },
      "type": null,
      "groupName": "partner",
      "name": "contractor"
    },
    {
      "meta": {
        "label": "Расчетная стоимость"
      },
      "type": null,
      "groupName": "countedCost",
      "name": "cost"
    },
    {
      "meta": {
        "label": "Предельная стоимость",
        "readonly": true
      },
      "type": null,
      "groupName": null,
      "name": "marginalCost"
    },
    {
      "meta": {
        "label": "Адрес выезда эвакуатора"
      },
      "type": null,
      "groupName": "address",
      "name": "towerAddress"
    },
    {
      "meta": {
        "dictionaryName": "WheelsBlockedCount",
        "bounded": true,
        "label": "Количество заблокированных колёс"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "wheelsUnblocked"
    },
    {
      "meta": {
        "label": "Переключается на нейтральную передачу"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "canNeutral"
    },
    {
      "meta": {
        "label": "Есть буксировочный крюк"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "towingPointPresent"
    },
    {
      "meta": {
        "label": "Есть место для манипулятора"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "manipulatorPossible"
    },
    {
      "meta": {
        "label": "Клиент/Доверенное лицо будет сопровождать автомобиль"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "companion"
    },    
    {
      "meta": {
        "label": "Пробег эвакуатора за городом"
      },
      "type": null,
      "groupName": null,
      "name": "suburbanMilage"
    },
    {
      "meta": {
        "label": "Номер заказ-наряда"
      },
      "type": null,
      "groupName": null,
      "name": "orderNumber"
    },
    {
      "meta": {
        "label": "Оплата"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "paid"
    },
    {
      "meta": {
        "label": "Скан загружен"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "scan"
    },
    {
      "meta": {
        "label": "Оригинал получен"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "original"
    },
    {
      "meta": {
        "label": "Дата окончания ремонта",
        "infoText": "date",
        "regexp": "date"
      },
      "type": "date",
      "groupName": null,
      "name": "repairEndDate"
    },
    {
      "meta": {
        "bounded": false,
        "dictionaryName": "UrgentServiceReason",
        "label": "Приоритетная услуга"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "urgentService"
    },
    {
      "meta": {
        "label": "Статус услуги",
        "bounded": true,
        "dictionaryName": "ServiceStatuses"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "status"
    },
    {
      "meta": {
        "dictionaryName": "Satisfaction",
        "label": "Клиент доволен"
      },
      "type": "dictionary",
      "groupName": null,
      "name": "clientSatisfied"
    },
    {
      "meta": {
        "label": "Гарантийный случай"
      },
      "type": "checkbox",
      "groupName": null,
      "name": "warrantyCase"
    },
    {
      "meta": {
        "label": "Прикрепленные файлы",
        "widget": "inline-uploader"
      },
      "type": "reference",
      "groupName": null,
      "name": "files"
    },
    {
      "meta": {
        "invisible": true
      },
      "type": "reference",
      "groupName": null,
      "name": "service_tarifOptions"
    },
    {
      "meta": {
        "invisible": true,
        "readonly": true
      },
      "type": null,
      "groupName": null,
      "name": "assignedTo"
    },
    {
      "meta": {
        "invisible": true,
        "readonly": true
      },
      "type": null,
      "groupName": null,
      "name": "falseCallPercent"
    }
  ],
  "applications": [
    {
      "meta": {
        "label": "Адрес доставки"
      },
      "targets": [
        "towAddress_address"
      ]
    },
    {
      "meta": {
        "label": "Дилер (куда эвакуируют автомобиль)"
      },
      "targets": [
        "towDealer_partner"
      ]
    },
    {
      "meta": {
        "label": "Партнёр"
      },
      "targets": [
        "contractor_partner"
      ]
    },
    {
      "meta": {
        "label": "Адрес выезда эвакуатора"
      },
      "targets": [
        "towerAddress_address"
      ]
    },
    {
      "meta": {
        "targetMap": "towAddress_map",
        "targetCoords": "towAddress_coords"
      },
      "targets": [
        "towAddress_address"
      ]
    },
    {
      "meta": {
        "targetMap": "towAddress_map",
        "targetAddr": "towAddress_address"
      },
      "targets": [
        "towAddress_coords"
      ]
    },
    {
      "meta": {
        "targetAddr": "towAddress_address",
        "targetCoords": "towAddress_coords",
        "currentBlipType": "dealer",
        "moreCoords": [
          "case-form/caseAddress_coords"
        ],
        "cityField": "case-form/city"
      },
      "targets": [
        "towAddress_map"
      ]
    },
    {
      "meta": {
        "targetMap": "towerAddress_map",
        "targetCoords": "towerAddress_coords"
      },
      "targets": [
        "towerAddress_address"
      ]
    },
    {
      "meta": {
        "targetMap": "towerAddress_map",
        "targetAddr": "towerAddress_address"
      },
      "targets": [
        "towerAddress_coords"
      ]
    },
    {
      "meta": {
        "targetAddr": "towerAddress_address",
        "targetCoords": "towerAddress_coords",
        "currentBlipType": "tow",
        "moreCoords": [
          "case-form/caseAddress_coords"
        ],
        "cityField": "case-form/city"
      },
      "targets": [
        "towerAddress_map"
      ]
    },
    {
      "meta": {
        "targetPartner": "towDealer_partner",
        "targetPartnerId": "towDealer_partnerId",
        "targetPartnerAddr": "towDealer_address",
        "targetPartnerCoords": "towDealer_coords",
        "partnerTable": "towDealer_partnerTable",
        "highlightIdFields": [
          "contractor_partnerId",
          "towDealer_partnerId"
        ],
        "cityField": "case-form/city"
      },
      "targets": [
        "towDealer_partnerMap"
      ]
    },
    {
      "meta": {
        "targetPartner": "contractor_partner",
        "targetPartnerId": "contractor_partnerId",
        "targetPartnerAddr": "contractor_address",
        "targetPartnerCoords": "contractor_coords",
        "partnerTable": "contractor_partnerTable",
        "highlightIdFields": [
          "contractor_partnerId",
          "towDealer_partnerId"
        ],
        "cityField": "case-form/city"
      },
      "targets": [
        "contractor_partnerMap"
      ]
    },
    {
      "meta": {
        "mainToo": true
      },
      "targets": [
        "times_expectedServiceStart"
      ]
    }
  ],
  "canDelete": true,
  "canUpdate": true,
  "canRead": true,
  "canCreate": true,
  "title": "Эвакуация",
  "name": "towage"
}
