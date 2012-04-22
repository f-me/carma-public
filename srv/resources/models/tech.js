{
    "name": "tech",
    "title": "Техпомощь",
    "canCreate": true,
    "canRead": true,
    "canUpdate": true,
    "canDelete": true,
    "applications": [
        {
            "targets": ["caseAddress_address"],
            "meta": {
                "label": "Адрес места поломки"
            }
        },
        {
            "targets": ["payment_payment"],
            "meta": {
                "label": "Стоимость"
            }
        }        
    ],
    "fields": [
        {
            "name": "status",
            "type": "dictionary",
            "meta": {
                "label": "Статус услуги",
                "dictionaryName": "ServiceStatuses"
            }
        },
    	{
            "name": "payType",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head", "parguy"],
            "type": "dictionary",
            "meta": {
                "dictionaryName": "PaymentTypes",
                "label": "Тип оплаты",
            }
        },
        {
            "name": "payment",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head", "parguy"],
            "groupName": "payment"
        },	
        {
            "name": "warrantyCase",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["back", "head", "parguy"],			
            "type": "checkbox",
            "meta": {
                "label": "Гарантийный случай"
            }
        },	
        {
            "name": "expectedCost",
            "canRead": ["front", "back", "head"],
            "canWrite": ["front", "back", "head"],
            "meta": {
                "label": "Ожидаемая стоимость"
            }
        },	
        {
            "name": "limitedCost",
            "canRead": ["back", "head"],
            "meta": {
                "label": "Предельная стоимость"
            }
        },
        {
            "name": "overcosted",
            "canRead": ["front", "back", "head", "parguy"],
            "type": "checkbox",
            "meta": {
                "label": "Стоимость превышена?"
            }
        },
        {
            "name": "partnerCost",
            "canRead": ["back", "head", "parguy"],
            "canWrite": ["back", "head"],
            "meta": {
                "label": "Стоимость со слов партнёра"
            }
        },	
        {
            "name": "expectedServiceStart",
            "canRead": ["front", "back", "head"],
            "canWrite": ["front", "back", "head"],			
            "type": "datetime",
            "meta": {
                "label": "Ожидаемое время начала оказания услуги",
            }
        },
        {
            "name": "factServiceStart",
            "canRead": ["back", "head"],
            "canWrite": ["back", "head"],
            "type": "datetime",
            "meta": {
                "label": "Фактическое  время начала оказания услуги",
            }
        },
        {
            "name": "expectedServiceEnd",
            "canRead": ["back", "head"],
            "canWrite": ["back", "head"],
            "type": "datetime",
            "meta": {
                "label": "Ожидаемое время окончания оказания услуги",
            }
        },
        {
            "name": "factServiceEnd",
            "canRead": ["back", "head"],
            "canWrite": ["back", "head"],
            "type": "datetime",
            "meta": {
                "label": "Фактическое время окончания оказания услуги",
            }
        },
        {
            "name": "expectedServiceFinancialClosure",
            "canRead": ["back", "head"],
            "canWrite": ["back", "head"],
            "type": "datetime",
            "meta": {
                "label": "Ожидаемое время финансового закрытия услуги",
            }
        },
        {
            "name": "factServiceFinancialClosure",
            "canRead": ["back", "head"],
            "canWrite": ["back", "head"],
            "type": "datetime",
            "meta": {
                "label": "Фактическое время финансового закрытия услуги",
            }
        },

        {
            "name": "expectedServiceClosure",
            "canRead": ["back", "head"],
            "canWrite": ["back", "head"],
            "type": "datetime",
            "meta": {
                "label": "Ожидаемое время закрытия услуги",
            }
        },
        {
            "name": "factServiceClosure",
            "canRead": ["head"],
            "canWrite": ["head"],
            "type": "datetime",
            "meta": {
                "label": "Фактическое время закрытия услуги",
            }
        },
        {
            "name": "repairEndDate",
            "canRead": ["back", "head"],
            "canWrite": ["back", "head"],
            "type": "date",
            "meta": {
                "label": "Дата окончания ремонта",
            }
        },
		{
            "name": "falseCall",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "type": "dictionary",
            "meta": {
                "dictionaryName": "FalseStatuses",
                "label": "Ложный вызов",
            }
        },		
        {
            "name": "clientSatisfied",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["back", "head"],
            "type": "checkbox",
            "meta": {
                "label": "Клиент доволен"
            }
        },		
        {
            "name": "billingDate",
            "canRead": ["head", "parguy"],
            "canWrite": ["parguy"],
            "type": "date",
            "meta": {
                "label": "Дата выставления счёта",
            }
        },	
        {
            "name": "billingCost",
            "canRead": ["head", "parguy"],
            "canWrite": ["parguy"],
            "meta": {
                "label": "Сумма по счёту",
            }
        },		
        {
            "name": "billNumber",
            "canRead": ["head", "parguy"],
            "canWrite": ["parguy"],
            "meta": {
                "label": "Номер счёта",
            }
        },        
        {
            "name": "techType",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "meta": {
                "dictionaryName": "TechTypes",
                "label": "Услуга",
                "required": true
            },
            "type": "dictionary",
            "index": true
        },
        {
            "name": "caseAddress",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "groupName": "address"
        },
        {
            "name": "techContractor",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],
            "groupName": "partner",
            "meta": {
                "label": "Название партнёра"
            }
        },
        {
            "name": "suburbanMilage",
            "canRead": ["front", "back", "head", "parguy"],
            "canWrite": ["front", "back", "head"],            
            "meta": {
                "label": "Пробег техпомощи за городом"
            }
        }
    ]
}
