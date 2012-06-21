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
            "canRead": [ "front", "back", "head" ],
            "canWrite": [ "front", "back", "head" ]
        },
        {
            "targets": [
				"payment_partnerCost"
            ],
            "canRead": [ "back", "head", "parguy" ],
            "canWrite": [ "back", "head" ]
        },
        {
            "targets": [
				"payment_calculatedCost",
				"payment_overcosted"
            ],
            "canRead": [ "front", "back", "head", "parguy" ]
        },
        {
            "targets": [
				"payment_limitedCost",
            ],
            "canRead": [ "back", "head" ]
        },
        {
            "targets": [
				"payment_payType",
				"payment_paidByRUAMC",
				"payment_paidByClient"
            ],
            "canRead": [ "front", "back", "head", "parguy" ],
            "canWrite": [ "back", "head", "parguy" ]
        },		
        {
            "targets": [
                "fromAddress_address",
                "fromAddress_coords",
                "fromAddress_city",
                "fromAddress_comment"
            ],
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "front", "back", "head" ]
        },
        {
            "targets": [
                "toAddress_address",
                "toAddress_coords",
                "toAddress_city",
                "toAddress_comment"
            ],
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "front", "back", "head" ]
        },
        {
            "targets": [
                "soberContractor_partner",
                "soberContractor_partnerTable",
                "soberContractor_coords"
            ],
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "front", "back", "head" ]
        },
        {
            "targets": [
				"bill_billNumber",
				"bill_billingCost",
				"bill_billingDate"
            ],
            "canRead": [ "head", "parguy" ],
            "canWrite": [ "parguy" ]
        },		
        {
            "targets": [
				"times_expectedServiceStart"
            ],
            "canRead": [ "partner", "front", "back", "head" ],
            "canWrite": [ "front", "back", "head" ]
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
            "canRead": [ "back", "head" ],
            "canWrite": [ "back", "head" ]
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
            "name": "payment",
            "groupName": "payment"
        },
        {
            "name": "times",
            "groupName": "times"
        },	
        {
            "name": "falseCall",
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "front", "back", "head" ],
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
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "front", "back", "head" ],
            "groupName": "address",
            "meta": {
                "label": "Где забрать"
            }
        },
        {
            "name": "toAddress",
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "front", "back", "head" ],
            "groupName": "address",
            "meta": {
                "label": "Куда доставить"
            }
        },
        {
            "name": "soberContractor",
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "front", "back", "head" ],
            "groupName": "partner",
            "meta": {
                "label": "Название партнёра"
            }
        },
        {
            "name": "multidrive",
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "front", "back", "head" ],
            "meta": {
                "label": "Мультидрайв"
            },
            "type": "checkbox"
        },
        {
            "name": "status",
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "front", "back", "head", "parguy" ],
            "type": "dictionary",
            "meta": {
                "label": "Статус услуги",
                "dictionaryName": "ServiceStatuses"
            }
        },
        {
            "name": "clientSatisfied",
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "back", "head" ],
            "type": "checkbox",
            "meta": {
                "label": "Клиент доволен"
            }
        },
        {
            "name": "warrantyCase",
            "canRead": [ "partner", "front", "back", "head", "parguy" ],
            "canWrite": [ "back", "head", "parguy" ],
            "type": "checkbox",
            "meta": {
                "label": "Гарантийный случай"
            }
        }
    ]
}
