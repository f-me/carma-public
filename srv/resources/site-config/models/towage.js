{
    "name": "towage",
    "title": "Эвакуация",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "applications": [
        {
            "targets": [
                "towAddress_address"
            ],
            "meta": {
                "label": "Адрес доставки"
            }
        },
        {
            "targets": [
                "towDealer_partner"
            ],
            "meta": {
                "label": "Дилер (куда эвакуируют автомобиль)"
            }
        },
        {
            "targets": [
                "contractor_partner"
            ],
            "meta": {
                "label": "Партнёр"
            }
        },
        {
            "targets": [
                "towerAddress_address"
            ],
            "meta": {
                "label": "Адрес выезда эвакуатора"
            }
        },
        {
            "targets": [
                "cost_countedCost"
            ],
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ]

        },
        {
            "targets": [
                "urgentService",
                "cost_counted",
                "cost_serviceTarifOptions"
            ],
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ]
        },
        {
            "targets": [
                "towAddress_address"
            ],
            "meta": {
                "targetMap": "towAddress_map",
                "targetCoords": "towAddress_coords"
            }
        },
        {
            "targets": [
                "towAddress_coords"
            ],
            "meta": {
                "targetMap": "towAddress_map",
                "targetAddr": "towAddress_address"
            }
        },
        {
            "targets": [
                "towAddress_map"
            ],
            "meta": {
                "targetAddr": "towAddress_address",
                "targetCoords": "towAddress_coords",
                "currentBlipType": "dealer",
                "moreCoords": ["case-form/caseAddress_coords"],
                "cityField": "case-form/city"
            }
        },
        {
            "targets": [
                "towAddress_address",
                "towAddress_coords",
                "towAddress_city",
                "towAddress_comment",
                "towAddress_map"
            ],
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ]
        },
        {
            "targets": [
                "payment_partnerCost",
                "payment_costTranscript"
            ],
            "canRead": [
                "back",
                "front",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "back",
                "front",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ]
        },
        {
            "targets": [
                "payment_calculatedCost",
                "payment_overcosted"
            ],
            "canRead": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "parguy"
            ]
        },
        {
            "targets": [
                "payment_limitedCost"
            ],
            "canRead": [
                "back",
                "front",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman"
            ]
        },
        {
            "targets": [
                "payment_paidByRUAMC",
                "payment_paidByClient"
            ],
            "canRead": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "back",
                "front",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ]
        },
        {
            "targets": [
                "towerAddress_address"
            ],
            "meta": {
                "targetMap": "towerAddress_map",
                "targetCoords": "towerAddress_coords"
            }
        },
        {
            "targets": [
                "towerAddress_coords"
            ],
            "meta": {
                "targetMap": "towerAddress_map",
                "targetAddr": "towerAddress_address"
            }
        },
        {
            "targets": [
                "towerAddress_map"
            ],
            "meta": {
                "targetAddr": "towerAddress_address",
                "targetCoords": "towerAddress_coords",
                "currentBlipType": "tow",
                "moreCoords": ["case-form/caseAddress_coords"]
            }
        },
        {
            "targets": [
                "towerAddress_address",
                "towerAddress_coords",
                "towerAddress_city",
                "towerAddress_comment",
                "towerAddress_map"
            ],
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ]
        },
        {
            "targets": [
                "towDealer_partnerMap"
            ],
            "meta": {
                "targetPartner": "towDealer_partner",
                "targetPartnerId": "towDealer_partnerId",
                "targetPartnerAddr": "towDealer_address",
                "targetPartnerCoords": "towDealer_coords",
                "partnerTable": "towDealer_partnerTable",
                "highlightIdFields": ["contractor_partnerId", "towDealer_partnerId"]
            }
        },
        {
            "targets": [
                "towDealer_partner",
                "towDealer_partnerId",
                "towDealer_partnerTable",
                "towDealer_partnerMap",
                "towDealer_coords",
                "towDealer_address"
            ],
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ]
        },
        {
            "targets": [
                "contractor_partnerMap"
            ],
            "meta": {
                "targetPartner": "contractor_partner",
                "targetPartnerId": "contractor_partnerId",
                "targetPartnerAddr": "contractor_address",
                "targetPartnerCoords": "contractor_coords",
                "partnerTable": "contractor_partnerTable",
                "highlightIdFields": ["contractor_partnerId", "towDealer_partnerId"]
            }
        },
        {
            "targets": [
                "contractor_partner",
                "contractor_partnerId",
                "contractor_partnerTable",
                "contractor_partnerMap",
                "contractor_coords",
                "contractor_partnerCancel",
                "contractor_address"
            ],
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ]
        },
        {
            "targets": [
                "bill_billNumber",
                "bill_billingCost",
                "bill_billingDate"
            ],
            "canRead": [
                "head",
                "supervisor",
                "director",
                "analyst",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "parguy"
            ]
        },
        {
            "targets": [
                "times_expectedServiceStart"
            ],
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ]
        },
        {
            "targets": [
                "times_factServiceStart",
                "times_expectedServiceEnd",
                "times_factServiceEnd",
                "times_expectedServiceFinancialClosure",
                "times_factServiceFinancialClosure",
                "times_expectedServiceClosure",
                "times_factServiceClosure"
            ],
            "canRead": [
                "back",
                "front",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "back",
                "front",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ]
        },
        {
            "targets": [
                "times_expectedServiceStart"
            ],
            "meta": {
                "mainToo": true
            }
        }
    ],
    "fields": [
        {
            "name": "parentId",
            "canRead": true,
            "canWrite": true,
            "meta": {
                "invisible": true
            }
        },
        {
            "name": "createTime",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy",
                "account"
            ],
            "type": "datetime",
            "meta": {
                "label": "Дата создания услуги",
                "readonly": true
            }
        },
        {
            "name": "payType",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "type": "dictionary",
            "meta": {
                "dictionaryName": "PaymentTypes",
                "bounded": true,
                "label": "Тип оплаты"
            }
        },
        {
            "name": "payment",
            "groupName": "payment"
        },
        {
            "name": "times",
            "groupName": "times"
        },
        {
            "name": "falseCall",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "type": "dictionary",
            "meta": {
                "dictionaryName": "FalseStatuses",
                "bounded": true,
                "label": "Ложный вызов",
                "infoText": "falsecall"
            }
        },
        {
            "name": "clientCancelReason",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman"
            ],            
            "type": "dictionary",
            "meta": {
              "dictionaryName": "ClientCancelReason",
              "label": "Причина отказа клиента"
            }
        },
        {
            "name": "falseCallPercent",
            "canRead":  [ ],
            "canWrite": [ ],
            "meta": {
                "invisible": true
            }
        },
        {
            "name": "bill",
            "groupName": "bill"
        },
        {
            "name": "towerType",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "meta": {
                "dictionaryName": "TowerTypes",
                "bounded": true,
                "label": "Тип эвакуатора"
            },
            "index": true,
            "type": "dictionary"
        },
        {
            "name": "towType",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "meta": {
                "dictionaryName": "TowTypes",
                "bounded": true,
                "label": "Вид эвакуации"
            },
            "type": "dictionary"
        },
        {
            "name": "vandalism",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "meta": {
                "label": "Случай вандализма"
            },
            "type": "checkbox"
        },
        {
            "name": "accident",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "meta": {
                "label": "ДТП"
            },
            "type": "checkbox"
        },
        {
            "name": "caseAddress",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "groupName": "address"
        },
        {
            "name": "towDealer",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman"
            ],
            "groupName": "partner",
            "meta": {
                "label": "Дилер"
            }
        },
       {
            "name": "dealerDistance",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman"
            ],
            "meta": {
                "readonly": true,
                "label": "Расстояние до дилера",
                "distanceTo1": "case-form/caseAddress_coords",
                "distanceTo2": "towAddress_coords"
            }
        },
        {
            "name": "towAddress",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman"
            ],
            "groupName": "address",
            "meta": {
                "label": "Адрес доставки"
            }
        },
        {
            "name": "contractor",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman"
            ],
            "groupName": "partner",
            "meta": {
                "label": "Партнёр"
            }
        },
        {
            "name": "cost",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "groupName": "countedCost",
            "meta": {
                "label": "Расчетная стоимость"
            }
        },
        {
            "name": "marginalCost",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [ ],
            "meta": {
                "label": "Предельная стоимость",
                "readonly": true
            }
        },
        {
            "name": "towerAddress",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "groupName": "address",
            "meta": {
                "label": "Адрес выезда эвакуатора"
            }
        },
        {
            "name": "wheelsUnblocked",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "meta": {
                "dictionaryName": "WheelsBlockedCount",
                "bounded":true,
                "label": "Количество заблокированных колёс"
            },
            "type": "dictionary"
        },
        {
            "name": "canNeutral",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "meta": {
                "label": "Переключается на нейтральную передачу"
            },
            "type": "checkbox"
        },
        {
            "name": "towingPointPresent",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "meta": {
                "label": "Есть буксировочный крюк"
            },
            "type": "checkbox"
        },
        {
            "name": "manipulatorPossible",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "meta": {
                "label": "Есть место для манипулятора"
            },
            "type": "checkbox"
        },
        {
            "name": "suburbanMilage",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "meta": {
                "label": "Пробег эвакуатора за городом"
            }
        },
        {
            "name": "orderNumber",
            "canRead": [
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman"
            ],
            "canWrite": [
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman"
            ],
            "meta": {
                "label": "Номер заказ-наряда"
            }
        },
        {
            "name": "paid",
            "canRead": [
                "manager",
                "accManager"
            ],
            "canWrite": [
                "accManager"
            ],
            "meta": {
                "label": "Оплата"
            },
            "type": "checkbox"
        },
        {
            "name": "scan",
            "canRead": [
                "manager",
                "accManager"
            ],
            "canWrite": [
                "accManager"
            ],
            "meta": {
                "label": "Скан загружен"
            },
            "type": "checkbox"
        },
        {
            "name": "original",
            "canRead": [
                "manager",
                "accManager"
            ],
            "canWrite": [
                "accManager"
            ],
            "meta": {
                "label": "Оригинал получен"
            },
            "type": "checkbox"
        },        
        {
            "name": "repairEndDate",
            "canRead": [
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman"
            ],
            "canWrite": [
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman"
            ],
            "type": "date",
            "meta": {
                "label": "Дата окончания ремонта",
                "infoText": "date",
                "regexp": "date"
            }
        },
        {
            "name": "urgentService",
            "type": "dictionary",
            "meta": {
                "bounded": false,
                "dictionaryName": "UrgentServiceReason",
                "label": "Приоритетная услуга"
            }
        },
        {
            "name": "status",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "type": "dictionary",
            "meta": {
                "label": "Статус услуги",
                "bounded": true,
                "dictionaryName": "ServiceStatuses"
            }
        },
        {
            "name": "clientSatisfied",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman"
            ],
            "type": "dictionary",
            "meta": {
                "dictionaryName": "Satisfaction",
                "label": "Клиент доволен"
            }
        },
        {
            "name": "warrantyCase",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "canWrite": [
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "type": "checkbox",
            "meta": {
                "label": "Гарантийный случай"
            }
        },
        {
            "name": "files",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "type": "files",
            "meta": {
                "label": "Прикрепленные файлы"
            }
        },
        {
            "name": "service_tarifOptions",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy"
            ],
            "type": "reference",
            "meta": {
                "invisible": true
            }
        },
        {
            "name": "assignedTo",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy",
                "account"
            ],
            "canWrite": [],
            "meta": {
                "invisible": true,
                "readonly": true
            }
        },
        {
            "name": "falseCallPercent",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "supervisor",
                "director",
                "analyst",
                "parguy",
                "account",
                "admin",
                "programman",
                "parguy",
                "account"
            ],
            "canWrite": [],
            "meta": {
                "invisible": true,
                "readonly": true
            }
        }
    ]
}
