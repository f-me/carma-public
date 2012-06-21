{
    "name": "transportation",
    "title": "Транспортировка",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "applications": [
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
                "fromToAddress_address"
            ],
            "meta": {
                "label": "Адрес куда/откуда"
            }
        },		
        {
            "targets": ["caseAddress_address","caseAddress_coords","caseAddress_city","caseAddress_comment"],
            "canRead": ["partner", "front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"]
        },     
        {
            "targets": ["fromToAddress_address","fromToAddress_coords","fromToAddress_city","fromToAddress_comment"],
            "canRead": ["partner", "front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"]
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
            "canRead": ["partner", "front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
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
            "name": "transportType",
            "canRead": ["partner", "front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],      			
            "type": "dictionary",
            "meta": {
                "label": "Тип транспортировки",
                "dictionaryName": "TransportTypes"
            }
        },		
        {
            "name": "fromToAddress",
            "canRead": ["partner", "front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],            
            "groupName": "address"
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
            "canRead": ["partner", "front", "back", "head", "parguy"],
            "canWrite": ["back", "head"],
            "type": "checkbox",
            "meta": {
                "label": "Клиент доволен"
            }
        },	
        {
            "name": "warrantyCase",
            "canRead": ["partner", "front", "back", "head", "parguy"],
            "canWrite": ["back", "head", "parguy"],			
            "type": "checkbox",
            "meta": {
                "label": "Гарантийный случай"
            }
        }
    ]
}
