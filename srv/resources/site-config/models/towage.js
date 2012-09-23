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
                "towAddress_address",
                "towAddress_coords",
                "towAddress_city",
                "towAddress_comment"
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
                "payment_expectedCost"
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
                "programman"
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
            ]
        },
        {
            "targets": [
                "payment_partnerCost",
                "payment_costTranscript"
            ],
            "canRead": [
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
                "towerAddress_address",
                "towerAddress_coords",
                "towerAddress_city",
                "towerAddress_comment"
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
                "towDealer_partner",
                "towDealer_partnerTable",
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
                "contractor_partner",
                "contractor_partnerTable",
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
            "type": "datetime",
            "meta": {
                "label": "Дата создания услуги"
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
                "label": "Ложный вызов",
                "infoText": "falsecall"
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
            "type": "checkbox",
            "meta": {
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
        }
    ]
}