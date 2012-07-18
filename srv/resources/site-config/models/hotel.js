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
                "parguy",
                "account"
            ],
            "canWrite": [
                "back",
                "head",
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
                "caseAddress_address"
            ],
            "meta": {
                "label": "Адрес кейса"
            }
        },
        {
            "targets": [
                "hotelContractor_partner"
            ],
            "meta": {
                "label": "Гостиница"
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
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "parguy"
            ]
        },
        {
            "targets": [
                "hotelContractor_partner",
                "hotelContractor_partnerTable",
                "hotelContractor_address"
            ],
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
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
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
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
            "name": "caseAddress",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "parguy"
            ],
            "groupName": "address",
            "meta": {
                "label": "Адрес кейса"
            }
        },
        {
            "name": "hotelContractor",
            "canRead": [
                "partner",
                "front",
                "back",
                "head",
                "parguy",
                "account"
            ],
            "canWrite": [
                "front",
                "back",
                "head",
                "parguy"
            ],
            "groupName": "partner",
            "meta": {
                "label": "Гостиница"
            }
        },
        {
            "name": "providedFor",
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
                "parguy",
                "account"
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
        },
        {
            "name": "files",
            "canRead": [ "partner", "front", "back", "head", "parguy", "account" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "type": "files",
            "meta": {
                "label": "Прикрепленные файлы"
            }
        }
    ]
}
