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
            "name": "coords",
            "type": "coords",
            "meta": {
                "widget": "picker",
                "label": "Фактическыя координаты",
                "infoText": "coords",
                "picker": "reverseGeoPicker",
                "targetAddr": "addrDeFacto"
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
            "name": "makers",
            "meta": {
                "label": "Обслуживаемые марки"
            },
            "type": "textarea"
        },
        {
            "name": "serviceAddress",
            "meta": {
                "label": "Адрес сервисного отдела"
            }
        },
        {
            "name": "servicePhone",
            "meta": {
                "label": "Телефон сервисного отдела",
                "regexp": "phone"
            }
        },
        {
            "name": "serviceWorking",
            "meta": {
                "label": "Время работы сервисного отдела"
            }
        },
        {
            "name": "salesAddress",
            "meta": {
                "label": "Адрес отдела продаж"
            }
        },
        {
            "name": "salesPhone",
            "meta": {
                "label": "Телефон отдела продаж",
                "regexp": "phone"
            }
        },
        {
            "name": "salesWorking",
            "meta": {
                "label": "Время работы сервисного отдела"
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
            "name": "mtime",
            "type": "datetime",
            "canRead": true,
            "meta": {
                "invisible": true
            }
        },
        {
            "name": "services",
            "type": "reference"
        }
    ]
}
