{
    "name": "hotel",
    "title": "Гостиница",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "applications": [
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
                "caseAddress_address"
            ],
            "meta": {
                "label": "Адрес кейса"
            }
        },
        {
            "targets": [
                "contractor_partner"
            ],
            "meta": {
                "label": "Гостиница"
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
                "caseAddress_address",
                "caseAddress_coords",
                "caseAddress_city",
                "caseAddress_comment"
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
                "contractor_partner",
                "contractor_partnerId",
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
            "groupName": "address",
            "meta": {
                "label": "Адрес кейса"
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
                "programman",
                "parguy"
            ],
            "groupName": "partner",
            "meta": {
                "label": "Гостиница"
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
            "name": "providedFor",
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
                "label": "Срок, на который предоставлена гостиница (дней)"
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
        }
    ]
}
