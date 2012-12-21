{
    "name": "averageCommissioner",
    "title": "Аварийный комиссар",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "applications": [
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
                "commAddress_address"
            ],
            "meta": {
                "label": "Адрес выезда аваркома"
            }
        },
        {
            "targets": [
                "commAddress_address",
                "commAddress_coords",
                "commAddress_city",
                "commAddress_comment"
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
                "parguy"
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
                "parguy",
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
                "times_factServiceStart",
                "times_expectedServiceEnd",
                "times_factServiceEnd",
                "times_expectedServiceFinancialClosure",
                "times_factServiceFinancialClosure",
                "times_expectedServiceClosure",
                "times_factServiceClosure",
                "times_repairEndDate"
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
                "programman"
            ]
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
                "bounded":true,
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
                "programman"
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
            "name": "requestType",
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
                "label": "Тип запроса",
                "bounded":true,
                "dictionaryName": "RequestType"
            }
        },
        {
            "name": "whatToSay1",
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
                "account",
                "admin",
                "programman"
            ],
            "meta": {
                "label": "Описание проблемы"
            },
            "type": "textarea"
        },
        {
            "name": "activity",
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
                "label": "Тип действия",
                "bounded":true,
                "dictionaryName": "Activity"
            }
        },	
        {
            "name": "commAddress",
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
                "label": "Адрес выезда аваркома"
            }
        },		
        {
            "name": "commMilage",
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
                "label": "Пробег аваркома за городом"
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
                "label": "Название партнёра"
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
                "bounded":true,
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
        }
    ]
}
