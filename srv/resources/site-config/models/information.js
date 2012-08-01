{
    "name": "information",
    "title": "Информирование о происшествии",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "applications": [
        {
            "targets": [
                "expectedServiceStart",
                "factServiceStart",
                "expectedServiceEnd",
                "factServiceEnd",
                "expectedServiceFinancialClosure",
                "factServiceFinancialClosure",
                "expectedDealerInfo",
                "factDealerInfo",
                "expectedServiceClosure",
                "factServiceClosure"
            ],
            "meta": {
                "regexp": "datetime"
            }
        },
        {
            "targets": [
                "payment_expectedCost"
            ],
            "canRead": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman"
            ]
        },
        {
            "targets": [
                "payment_partnerCost"
            ],
            "canRead": [
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman"
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
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy"
            ]
        },
        {
            "targets": [
                "payment_limitedCost"
            ],
            "canRead": [
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman"
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
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
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
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
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
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman"
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
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman"
            ],
            "canWrite": [
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman"
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
            "name": "payType",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman"
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
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman"
            ],
            "type": "dictionary",
            "meta": {
                "dictionaryName": "FalseStatuses",
                "label": "Ложный вызов",
                "infoText": "falsecall"
            }
        },
        {
            "name": "bill",
            "groupName": "bill"
        },
        {
            "name": "contact1",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "account", "admin", "programman"
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
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "account", "admin", "programman"
            ],
            "meta": {
                "label": "Телефон 1",
                "regexp": "phone"
            },
            "type": "phone"
        },
        {
            "name": "whatToSay1",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "account", "admin", "programman"
            ],
            "meta": {
                "label": "Что сказать 1"
            },
            "type": "textarea"
        },
        {
            "name": "contact2",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "account", "admin", "programman"
            ],
            "meta": {
                "label": "Контакт 2"
            }
        },
        {
            "name": "contactPhone2",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "account", "admin", "programman"
            ],
            "meta": {
                "label": "Телефон 2",
                "regexp": "phone"
            },
            "type": "phone"
        },
        {
            "name": "whatToSay2",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "account", "admin", "programman"
            ],
            "meta": {
                "label": "Что сказать 2"
            },
            "type": "textarea"
        },
        {
            "name": "contact3",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "account", "admin", "programman"
            ],
            "meta": {
                "label": "Контакт 3"
            }
        },
        {
            "name": "contactPhone3",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "account", "admin", "programman"
            ],
            "meta": {
                "label": "Телефон 3",
                "regexp": "phone"
            },
            "type": "phone"
        },
        {
            "name": "whatToSay3",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "account", "admin", "programman"
            ],
            "meta": {
                "label": "Что сказать 3"
            },
            "type": "textarea"
        },
        {
            "name": "status",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
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
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman"
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
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy"
            ],
            "type": "checkbox",
            "meta": {
                "label": "Гарантийный случай"
            }
        },
        {
            "name": "files",
            "canRead": [ "partner", "front", "back", "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman", "parguy" ],
            "canWrite": [ "front", "back", "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman", "parguy" ],
            "type": "files",
            "meta": {
                "label": "Прикрепленные файлы"
            }
        }
    ]
}