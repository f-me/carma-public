{
    "name": "sober",
    "title": "Трезвый водитель",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "applications": [
        {
            "targets": [
                "fromAddress_address"
            ],
            "meta": {
                "label": "Где забрать"
            }
        },
        {
            "targets": [
                "toAddress_address"
            ],
            "meta": {
                "label": "Куда доставить"
            }
        },
        {
            "targets": [
                "soberContractor_partner"
            ],
            "meta": {
                "label": "Партнёр"
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
                "fromAddress_address",
                "fromAddress_coords",
                "fromAddress_city",
                "fromAddress_comment"
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
                "soberContractor_partner",
                "soberContractor_partnerTable",
                "soberContractor_address"
            ],
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
            "name": "createTime",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy",
                "account"
            ],
            "canWrite": [
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman"
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
            "name": "fromAddress",
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
            "groupName": "address",
            "meta": {
                "label": "Где забрать"
            }
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
                "label": "Куда доставить"
            }
        },
        {
            "name": "soberContractor",
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
            "groupName": "partner",
            "meta": {
                "label": "Название партнёра"
            }
        },
        {
            "name": "multidrive",
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
                "label": "Мультидрайв"
            },
            "type": "checkbox"
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