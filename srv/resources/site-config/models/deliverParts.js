{
    "name": "deliverParts",
    "title": "Доставка запчастей",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "applications": [
        {
            "targets": [
                "toAddress_address"
            ],
            "meta": {
                "label": "Адрес куда"
            }
        },
        {
            "targets": [
                "toAddress_address",
                "toAddress_coords",
                "toAddress_city",
                "toAddress_comment"
            ],
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
            ]
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
                "parguy",
                "account"
            ],
            "canWrite": [
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
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
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy",
                "account"
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
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
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
            "name": "parts",
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
            "meta": {
                "label": "Запчасти"
            },
            "type": "textarea"
        },
        {
            "name": "toAddress",
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
            "groupName": "address",
            "meta": {
                "label": "Адрес куда"
            }
        },
        {
            "name": "status",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman",
                "parguy",
                "account"
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
            "canRead": [ "partner", "front", "back", "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman", "parguy", "account" ],
            "canWrite": [ "front", "back", "head", "supervisor", "director", "analyst","parguy", "account", "admin", "programman", "parguy" ],
            "type": "files",
            "meta": {
                "label": "Прикрепленные файлы"
            }
        }
    ]
}