{
    "name": "partner",
    "title": "Партнёр",
    "canCreate": true,
    "canRead": true,
    "canUpdate": ["parguy"],
    "canDelete": ["parguy"],
     "applications": [
         {
            "targets": true,
            "canWrite": true,
            "canRead": true
         }
    ],
    "fields": [
        {
            "name": "isActive",
            "type": "checkbox",
            "meta": {
                "label": "Партнёр активен"
            }
        },
        {
            "name": "isDealer",
            "type": "checkbox",
            "meta": {
                "label": "Дилер"
            }
        },       
        {
            "name": "isMobile",
            "type": "checkbox",
            "meta": {
                "label": "Мобильный партнёр"
            }
        }, 
        {
            "name": "name",
            "index": true,
            "meta": {
                "label": "Название"
            }
        },
        {
            "name": "code",
            "meta": {
                "label": "Код"
            }
        },
        {
            "name": "city",
            "meta": {
                "label": "Город"
            }
        },
        {
            "name": "addrDeJure",
            "meta": {
                "label": "Юридический адрес"
            }
        },
        {
            "name": "addrDeFacto",
            "meta": {
                "label": "Фактический адрес"
            }
        },
        {
            "name": "workingTime",
            "meta": {
                "label": "Время работы"
            }
        },
        {
            "name": "phone1",
            "meta": {
                "label": "Телефоны диспетчерской"
            }
        },
        {
            "name": "fax",
            "meta": {
                "label": "Факс"
            }
        },
        {
            "name": "closeTicketPhone",
            "meta": {
                "label": "Телефон для закрытия заявок"
            }
        },
        {
            "name": "closeTicketEmail",
            "meta": {
                "label": "Email для закрытия заявок",
                "regexp": "email"
            }
        },
        {
            "name": "personInCharge",
            "meta": {
                "label": "Ответственное лицо"
            }
        },
        {
            "name": "taxScheme",
            "type": "dictionary",
            "meta": {
                "dictionaryName": "TaxSchemes",
                "label": "Форма налогообложения"
            }
        },
        {
            "name": "comment",
            "meta": {
                "label": "Комментарии"
            }
        },
        {
            "name": "isPayBackConfirmed",
            "type": "checkbox",
            "meta": {
                "label": "Соглашение о вознаграждении"
            }
        },
        {
            "name": "services",
            "type": "reference"
        }
    ]
}
