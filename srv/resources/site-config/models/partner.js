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
            "type": "dictionary",
            "meta": {
                "label": "Город",
                "dictionaryName": "DealerCities"
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
                "label": "Координаты фактического адреса",
                "infoText": "coords",
                "picker": "mapPicker",
                "targetAddr": "addrDeFacto",
                "targetCoords": "coords",
                "currentBlipType": "partner"
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
                "label": "Время работы отдела продаж"
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
            "name": "makes",
            "meta": {
                "dictionaryName": "CarMakers",
                "required": true,
                "bounded": true,
                "label": "Обслуживаемые марки"
            },
            "type": "dictionary-many"
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
        },
        {
            "name": "comment",
            "canRead": [
                "partner",
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "canWrite": [
                "front",
                "back",
                "head", "supervisor", "director", "analyst", "parguy", "account", "admin", "programman",
                "parguy"
            ],
            "meta": {
                "label": "Комментарий"
            },
            "type": "textarea"
        }
    ]
}
