{
    "name": "tech",
    "title": "Техпомощь",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "applications": [
        {
            "targets": [
                "techContractor_partner"
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
                "head"
            ],
            "canWrite": [
                "front",
                "back",
                "head"
            ]
        },
        {
            "targets": [
                "payment_partnerCost"
            ],
            "canRead": [
                "back",
                "head",
                "parguy"
            ],
            "canWrite": [
                "back",
                "head"
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
                "parguy"
            ]
        },
        {
            "targets": [
                "payment_limitedCost"
            ],
            "canRead": [
                "back",
                "head"
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
                "parguy"
            ],
            "canWrite": [
                "back",
                "head",
                "parguy"
            ]
        },
        {
            "targets": [
                "techContractor_partner",
                "techContractor_partnerTable",
                "techContractor_address"
            ],
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head"
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
                "head"
            ],
            "canWrite": [
                "front",
                "back",
                "head"
            ]
        },
        {
            "targets": [
                "times_factServiceStart",
                "times_expectedServiceEnd",
                "times_factServiceEnd",
                "times_expectedServiceFinancialClosure",
                "times_factServiceFinancialClosure",
                "times_expectedDealerInfo",
                "times_factDealerInfo",
                "times_expectedServiceClosure",
                "times_factServiceClosure",
                "times_repairEndDate"
            ],
            "canRead": [
                "back",
                "head"
            ],
            "canWrite": [
                "back",
                "head"
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
            "name": "payType",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head"
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
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head"
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
            "name": "techType",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head"
            ],
            "meta": {
                "dictionaryName": "TechTypes",
                "label": "Услуга",
                "required": true
            },
            "type": "dictionary",
            "index": true
        },
        {
            "name": "techContractor",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head"
            ],
            "groupName": "partner",
            "meta": {
                "label": "Партнёр"
            }
        },
        {
            "name": "suburbanMilage",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head"
            ],
            "meta": {
                "label": "Пробег техпомощи за городом"
            }
        },
        {
            "name": "status",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
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
                "parguy"
            ],
            "canWrite": [
                "back",
                "head"
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
                "parguy"
            ],
            "canWrite": [
                "back",
                "head",
                "parguy"
            ],
            "type": "checkbox",
            "meta": {
                "label": "Гарантийный случай"
            }
        }
    ]
}
