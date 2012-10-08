{
    "name": "unfuel",
    "title": "Слив топлива",
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
                "caseAddress_address"
            ],
            "meta": {
                "label": "Адрес кейса"
            }
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
            ]
        },		
        {
            "targets": [
                "techAddress_address"
            ],
            "meta": {
                "label": "Адрес выезда техпомощи"
            }
        },
        {
            "targets": [
                "techAddress_address",
                "techAddress_coords",
                "techAddress_city",
                "techAddress_comment"
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
            "groupName": "address",
            "meta": {
                "label": "Адрес кейса"
            }
        },	
        {
            "name": "techAddress",
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
                "label": "Адрес выезда техпомощи"
            }
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
                "label": "Пробег техпомощи за городом"
            }
        },		
        {
            "name": "contact1",
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
                "label": "Контакт 1"
            }
        },
        {
            "name": "contactPhone1",
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
                "label": "Телефон 1",
                "regexp": "phone"
            },
            "type": "phone"
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
        }
    ]
}
